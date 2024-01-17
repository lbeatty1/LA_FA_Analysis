#Difference between API and Prism LINCOLN COUNTY LOUISIANA

rm(list=ls())

library(tidyverse)
library(httr2)
library(RCurl)
library(httpuv)
library(jsonlite)
library(data.table)
library(ggplot2)
library(sf)
library(scales)

codedirectory="C:/Users/lbeatty/Documents/LA_FA_Analysis"
setwd("C:/Users/lbeatty/OneDrive - Environmental Defense Fund - edf.org/OrphanedWells/State Advocacy/LA/Data")

statedata = read.csv('Well_Information.csv', colClasses = 'character')
enverus = read.csv("Enverus_Wells.csv", colClasses='character')


keeplist = c("API_UWI_14_Unformatted",
             "ENVProdWellType",
             "ENVProducingMethod",
             "ENVWellboreType",
             "ENVOperator",
             "ENVWellStatus",
             "FirstProdDate",
             "Last12MonthGasProduction_MCF",
             "Last12MonthOilProduction_BBL",
             "MD_FT",
             "PlugDate",
             "SpudDate",
             "StateWellType",
             "TVD_FT")

statekeep = c("Operator.Name",
              "Operator.ID",
              "Well.Status.Code",
              "Well.Status.Code.Description", 
              "API.Num", 
              "Well.Serial.Num",
              "Well.Status.Date", 
              "Ground.Elevation",
              "Latitude", 
              "Longitude", 
              "Product.Type.Code.Description")

enverus = enverus%>%
  select(all_of(keeplist))%>%
  mutate(API = as.character(API_UWI_14_Unformatted))

statedata=statedata%>%
  select(all_of(statekeep))%>%
  filter(Well.Status.Code.Description!="VIRTUAL/BAD DATA",
         !API.Num%in%c("00000000000000","0", ""))
  

FA = read.csv('FinancialSecurity.csv', colClasses='character')
statedata = left_join(statedata, FA, by=c("API.Num", "Well.Serial.Num"))
data=left_join(statedata, enverus, by=c("API.Num"="API"))

#View(data%>%select(API.Num, Well.Status.Code.Description, ENVWellStatus, ENVOperator, Operator.Name))

data=data%>%
  mutate(OFFSHORE_FLAG = substr(API.Num,3,5)%in%c("714", "726", "728", "727", "730", "729", "703", "704", "709", "710", "717", "718", "725", "724", "731", "711", "712", "707", "708", "721", "723", "722", "713", "715", "716", "705", "706","700", "702", "701", "719", "720"),
         MD_FT=as.numeric(MD_FT),
         Last12MonthGasProduction_MCF=as.numeric(Last12MonthGasProduction_MCF),
         Last12MonthOilProduction_BBL=as.numeric(Last12MonthOilProduction_BBL),
         marginal_flag = (Last12MonthOilProduction_BBL+6*Last12MonthGasProduction_MCF)/365<15,
         marginal_flag = replace(marginal_flag, is.na(marginal_flag)&Well.Status.Code.Description.x=="ACTIVE- INJECTION", 0),  #give benefit to injection wells
         marginal_flag = replace(marginal_flag, is.na(marginal_flag), 1)) #assume others with missing production marginal


counties = st_read('cb_2018_us_county_500k/cb_2018_us_county_500k.shp')%>%filter(STATEFP=="22")

data=st_as_sf(data%>%mutate(Longitude=as.numeric(Longitude), Latitude=as.numeric(Latitude))%>%filter(!is.na(Longitude)&!is.na(Latitude)), coords=c('Longitude', 'Latitude'), crs="WGS84")

##Ok Onshore/offshore looks like it works
ggplot(data=data)+
  geom_sf(aes(color=OFFSHORE_FLAG),size=0.3)+
  geom_sf(data=counties, fill=NA)+
  coord_sf(xlim=c(-94,-89), ylim=c(28.5,30))+
  theme_bw()
  

## calculate well plug costs
meandepth=mean(as.vector(data%>%data.frame()%>%filter(OFFSHORE_FLAG==0)%>%select(MD_FT))$MD_FT,na.rm=T)
meandepth_offshore=mean(as.vector(data%>%data.frame()%>%filter(OFFSHORE_FLAG==1)%>%select(MD_FT))$MD_FT,na.rm=T)
data=data%>%
  mutate(MD_FT=replace(MD_FT, is.na(MD_FT)&OFFSHORE_FLAG==0, meandepth),
         MD_FT = replace(MD_FT, is.na(MD_FT)&OFFSHORE_FLAG==1, meandepth_offshore),
         water_notoffshore_flag = OFFSHORE_FLAG==0&Site.Clearance.Required.!="No, land location",
         water_notoffshore_flag = replace(water_notoffshore_flag, is.na(water_notoffshore_flag),0))

ggplot(data=data%>%mutate(water_notoffshore_flag=as.character(water_notoffshore_flag))%>%filter(OFFSHORE_FLAG==0))+
  geom_sf(aes(color=water_notoffshore_flag),size=0.5)+
  geom_sf(data=counties, fill=NA)+
  coord_sf(xlim=c(-94,-89), ylim=c(28.5,30))+
  theme_bw()

data=data%>%
  mutate(plug_cost1 = 12*MD_FT,
         plug_cost1 = replace(plug_cost1, OFFSHORE_FLAG==1, 1500000),
         plug_cost1 = replace(plug_cost1, water_notoffshore_flag==1, 1500000),
         plug_cost2 = 70000,
         plug_cost2 = replace(plug_cost2, OFFSHORE_FLAG==1, 1500000),
         plug_cost2 = replace(plug_cost2, water_notoffshore_flag==1, 1500000),
         ind_well_fa_onshore = (2*MD_FT*(MD_FT<=3000)+5*MD_FT*(MD_FT>3000&MD_FT<=10000)+4*MD_FT*(MD_FT>10000))*(OFFSHORE_FLAG==0&water_notoffshore_flag==0),
         ind_well_fa_offshore = 12*MD_FT*(OFFSHORE_FLAG==1&water_notoffshore_flag==0),
         ind_well_fa_bays = 8*MD_FT*(water_notoffshore_flag==1))


operator_plug_costs = data%>%
  data.frame()%>%
  filter(!Well.Status.Code.Description.x%in%c('DRY AND PLUGGED', 'PLUGGED AND ABANDONED', 'ACT 404 ORPHAN WELL-ENG', 'ACT 404 ORPHAN WELL-INJECTION AND MINING', 'P&A PER INSPECTION'))%>%
  group_by(Operator.Name)%>%
  summarise(plug_cost1 = sum(plug_cost1),
            plug_cost2 = sum(plug_cost2),
            Last12Oil = sum(Last12MonthOilProduction_BBL,na.rm=T),
            Last12Gas = sum(Last12MonthGasProduction_MCF,na.rm=T))

operator_plug_costs_marginal =data%>%
  data.frame()%>%
  filter(!Well.Status.Code.Description.x%in%c('DRY AND PLUGGED', 'PLUGGED AND ABANDONED', 'ACT 404 ORPHAN WELL-ENG', 'ACT 404 ORPHAN WELL-INJECTION AND MINING', 'P&A PER INSPECTION'),
         marginal_flag==1)%>%
  group_by(Operator.Name)%>%
  summarise(marginal_plug_cost1 = sum(plug_cost1),
            marginal_plug_cost2 = sum(plug_cost2))

operator_plug_costs = left_join(operator_plug_costs, operator_plug_costs_marginal, by="Operator.Name")


operator_FA = data%>%
  data.frame()%>%
  filter(!Well.Status.Code.Description.x%in%c('DRY AND PLUGGED', 'PLUGGED AND ABANDONED', 'ACT 404 ORPHAN WELL-ENG', 'ACT 404 ORPHAN WELL-INJECTION AND MINING', 'P&A PER INSPECTION', 'PERMIT EXPIRED', 'PERMITTED'))%>%
  group_by(Operator.Name, OFFSHORE_FLAG, water_notoffshore_flag)%>%
  summarise(n=n(),
            ind_well_bond = sum(ind_well_fa_onshore,na.rm=T)+sum(ind_well_fa_offshore,na.rm=T)+sum(ind_well_fa_bays,na.rm=T))%>%
  mutate(blanket = 50000*(OFFSHORE_FLAG==F&water_notoffshore_flag==0),
         blanket = replace(blanket, (OFFSHORE_FLAG==F&water_notoffshore_flag==0&(n>=11&n<100)), 250000),
         blanket = replace(blanket, (OFFSHORE_FLAG==F&water_notoffshore_flag==0&(n>=100)), 500000),
         blanket = replace(blanket, (OFFSHORE_FLAG==F&water_notoffshore_flag==1&(n<=10)), 250000),
         blanket = replace(blanket, (OFFSHORE_FLAG==F&water_notoffshore_flag==1&(n>=11&n<100)), 1250000),
         blanket = replace(blanket, (OFFSHORE_FLAG==F&water_notoffshore_flag==1&(n>=100)), 2500000),
         blanket = replace(blanket, (OFFSHORE_FLAG==T&water_notoffshore_flag==0&(n<=10)), 500000),
         blanket = replace(blanket, (OFFSHORE_FLAG==T&water_notoffshore_flag==0&(n>=11&n<100)), 2500000),
         blanket = replace(blanket, (OFFSHORE_FLAG==T&water_notoffshore_flag==0&(n>=100)), 5000000),
         bond=pmin(ind_well_bond, blanket))


### Get inactive/marginal wells by operator

operator_shutins = data%>%
  data.frame()%>%
  filter(Well.Status.Code.Description.x%in%c('SHUT-IN PRODUCTIVE -FUTURE UTILITY', 'SHUT-IN DRY HOLE -FUTURE UTILITY', 'SHUT-IN PRODUCTIVE -NO FUTURE UTILITY', 'SHUT-IN DRY HOLE - NO FUTURE UTILITY', 'SHUT-IN WAITING ON MARKET', 'PA-35 TEMPORARY INACTIVE WELL TO BE OMITTED FROM PROD.REPORT', 'TEMPORARILY ABANDONED WELL'))%>%
  group_by(Operator.Name, OFFSHORE_FLAG, water_notoffshore_flag)%>%
  summarise(n=n(),
            plug_cost1 = sum(plug_cost1),
            plug_cost2 = sum(plug_cost2))
operator_shutins = pivot_wider(operator_shutins, id_cols='Operator.Name', names_from = c('OFFSHORE_FLAG', 'water_notoffshore_flag'), values_from=c('n', 'plug_cost1', 'plug_cost2'))%>%
  rename(n_shutin_land = n_FALSE_0,
         n_shutin_bays = n_FALSE_1,
         n_shutin_offshore=n_TRUE_0,
         plug_cost1_shutin_land = plug_cost1_FALSE_0,
         plug_cost1_shutin_bays = plug_cost1_FALSE_1,
         plug_cost1_shutin_offshore=plug_cost1_TRUE_0,
         plug_cost2_shutin_land = plug_cost2_FALSE_0,
         plug_cost2_shutin_bays = plug_cost2_FALSE_1,
         plug_cost2_shutin_offshore=plug_cost2_TRUE_0)%>%
  replace(is.na(.),0)

operator_marginal = data%>%
  data.frame()%>%
  mutate(BOEperday = (Last12MonthGasProduction_MCF*6+Last12MonthOilProduction_BBL)/365)%>%
  filter(Well.Status.Code.Description.x%in%c('SHUT-IN PRODUCTIVE -FUTURE UTILITY', 'SHUT-IN DRY HOLE -FUTURE UTILITY', 'SHUT-IN PRODUCTIVE -NO FUTURE UTILITY', 'SHUT-IN DRY HOLE - NO FUTURE UTILITY', 'SHUT-IN WAITING ON MARKET', 'ACTIVE - PRODUCING', 'PA-35 TEMPORARY INACTIVE WELL TO BE OMITTED FROM PROD.REPORT', 'TEMPORARILY ABANDONED WELL'),
         marginal_flag==1)%>%
  group_by(Operator.Name, OFFSHORE_FLAG, water_notoffshore_flag)%>%
  summarise(n=n(),
            plug_cost1 = sum(plug_cost1),
            plug_cost2 = sum(plug_cost2))  
operator_marginal = pivot_wider(operator_marginal, id_cols='Operator.Name', names_from = c('OFFSHORE_FLAG', 'water_notoffshore_flag'), values_from=c('n', 'plug_cost1', 'plug_cost2'))%>%
  rename(n_marginal_land = n_FALSE_0,
         n_marginal_bays = n_FALSE_1,
         n_marginal_offshore=n_TRUE_0,
         plug_cost1_marginal_land = plug_cost1_FALSE_0,
         plug_cost1_marginal_bays = plug_cost1_FALSE_1,
         plug_cost1_marginal_offshore=plug_cost1_TRUE_0,
         plug_cost2_marginal_land = plug_cost2_FALSE_0,
         plug_cost2_marginal_bays = plug_cost2_FALSE_1,
         plug_cost2_marginal_offshore=plug_cost2_TRUE_0)%>%
  replace(is.na(.),0)




##############
## Make histograms 
###################

##Onshore
ggplot(data=operator_FA%>%filter(OFFSHORE_FLAG==F, water_notoffshore_flag==0,n<=10))+
  geom_histogram(aes(x=ind_well_bond))+
  geom_vline(xintercept = 50000)+
  scale_x_continuous(label=dollar)+
  xlab("Sum of hypothetical individual well bonds for operators with less than 11 onshore wells")+
  #ggtitle("Histogram of hypothetical individual well bonds owed for TA wells \n for firms ")+
  labs(caption="Vertical black line drawn at the blanket amount.")+
  theme_bw()

ggsave(filename=paste(codedirectory,"/Figures/histogram_onshore_10.png", sep=""),
       width=6,
       height=4)

ggplot(data=operator_FA%>%filter(OFFSHORE_FLAG==F, water_notoffshore_flag==0,n>=11&n<100))+
  geom_histogram(aes(x=ind_well_bond))+
  geom_vline(xintercept = 250000)+
  scale_x_continuous(label=dollar)+
  xlab("Sum of hypothetical individual well bonds for operators with between 11 and 100 onshore wells")+
  #ggtitle("Histogram of hypothetical individual well bonds owed for TA wells \n for firms ")+
  labs(caption="Vertical black line drawn at the blanket amount.")+
  theme_bw()

ggsave(filename=paste(codedirectory,"/Figures/histogram_onshore_11_100.png", sep=""),
       width=6,
       height=4)

ggplot(data=operator_FA%>%filter(OFFSHORE_FLAG==F, water_notoffshore_flag==0,n>=100))+
  geom_histogram(aes(x=ind_well_bond))+
  geom_vline(xintercept = 500000)+
  scale_x_continuous(label=dollar)+
  xlab("Sum of hypothetical individual well bonds for operators with more than 100 onshore wells")+
  #ggtitle("Histogram of hypothetical individual well bonds owed for TA wells \n for firms ")+
  labs(caption="Vertical black line drawn at the blanket amount.")+
  theme_bw()

ggsave(filename=paste(codedirectory,"/Figures/histogram_onshore_100.png", sep=""),
       width=6,
       height=4)

## Offshore
ggplot(data=operator_FA%>%filter(OFFSHORE_FLAG==T, water_notoffshore_flag==0,n<=10))+
  geom_histogram(aes(x=ind_well_bond))+
  geom_vline(xintercept = 500000)+
  scale_x_continuous(label=dollar)+
  xlab("Sum of hypothetical individual well bonds for operators with less than 11 offshore wells")+
  #ggtitle("Histogram of hypothetical individual well bonds owed for TA wells \n for firms ")+
  labs(caption="Vertical black line drawn at the blanket amount.")+
  theme_bw()

ggsave(filename=paste(codedirectory,"/Figures/histogram_offshore_10.png", sep=""),
       width=6,
       height=4)

ggplot(data=operator_FA%>%filter(OFFSHORE_FLAG==T, water_notoffshore_flag==0,n>=11&n<100))+
  geom_histogram(aes(x=ind_well_bond))+
  geom_vline(xintercept = 2500000)+
  scale_x_continuous(label=dollar)+
  xlab("Sum of hypothetical individual well bonds for operators with between 11 and 100 offshore wells")+
  #ggtitle("Histogram of hypothetical individual well bonds owed for TA wells \n for firms ")+
  labs(caption="Vertical black line drawn at the blanket amount.")+
  theme_bw()

ggsave(filename=paste(codedirectory,"/Figures/histogram_offshore_11_100.png", sep=""),
       width=6,
       height=4)

ggplot(data=operator_FA%>%filter(OFFSHORE_FLAG==T, water_notoffshore_flag==0,n>=100))+
  geom_histogram(aes(x=ind_well_bond))+
  geom_vline(xintercept = 5000000)+
  scale_x_continuous(label=dollar)+
  xlab("Sum of hypothetical individual well bonds for operators with more than 100 offshore wells")+
  #ggtitle("Histogram of hypothetical individual well bonds owed for TA wells \n for firms ")+
  labs(caption="Vertical black line drawn at the blanket amount.")+
  theme_bw()

ggsave(filename=paste(codedirectory,"/Figures/histogram_offshore_100.png", sep=""),
       width=6,
       height=4)

## Bays.Lakes

ggplot(data=operator_FA%>%filter(OFFSHORE_FLAG==F, water_notoffshore_flag==1,n<=10))+
  geom_histogram(aes(x=ind_well_bond))+
  geom_vline(xintercept = 250000)+
  scale_x_continuous(label=dollar)+
  xlab("Sum of hypothetical individual well bonds for operators with less than 11 lakes+bays wells")+
  #ggtitle("Histogram of hypothetical individual well bonds owed for TA wells \n for firms ")+
  labs(caption="Vertical black line drawn at the blanket amount.")+
  theme_bw()

ggsave(filename=paste(codedirectory,"/Figures/histogram_bays_10.png", sep=""),
       width=6,
       height=4)

ggplot(data=operator_FA%>%filter(OFFSHORE_FLAG==F, water_notoffshore_flag==1,n>=11&n<100))+
  geom_histogram(aes(x=ind_well_bond))+
  geom_vline(xintercept = 1250000)+
  scale_x_continuous(label=dollar)+
  xlab("Sum of hypothetical individual well bonds for operators with between 11 and 100 lakes+bays wells")+
  #ggtitle("Histogram of hypothetical individual well bonds owed for TA wells \n for firms ")+
  labs(caption="Vertical black line drawn at the blanket amount.")+
  theme_bw()

ggsave(filename=paste(codedirectory,"/Figures/histogram_bays_11_100.png", sep=""),
       width=6,
       height=4)

ggplot(data=operator_FA%>%filter(OFFSHORE_FLAG==F, water_notoffshore_flag==1,n>=100))+
  geom_histogram(aes(x=ind_well_bond))+
  geom_vline(xintercept = 2500000)+
  scale_x_continuous(label=dollar)+
  xlab("Sum of hypothetical individual well bonds for operators with more than 100 lakes+bays wells")+
  #ggtitle("Histogram of hypothetical individual well bonds owed for TA wells \n for firms ")+
  labs(caption="Vertical black line drawn at the blanket amount.")+
  theme_bw()

ggsave(filename=paste(codedirectory,"/Figures/histogram_bays_100.png", sep=""),
       width=6,
       height=4)

operator_FA = operator_FA%>%
  group_by(Operator.Name)%>%
  summarise(sum_ind_well_bond = sum(ind_well_bond),
            bond = sum(bond))

operator_FA=left_join(operator_FA,operator_shutins, by="Operator.Name")
operator_FA = left_join(operator_FA, operator_marginal, by="Operator.Name")

operator_FA=operator_FA%>%
  mutate(plug_cost1_marginal = plug_cost1_marginal_land+plug_cost1_marginal_bays+plug_cost1_marginal_offshore,
         plug_cost2_marginal = plug_cost2_marginal_bays+plug_cost2_marginal_land+plug_cost2_marginal_offshore,
         plug_cost1_shutin = plug_cost1_shutin_bays+plug_cost1_shutin_land+plug_cost1_shutin_offshore,
         plug_cost2_shutin = plug_cost2_shutin_bays+plug_cost2_shutin_land+plug_cost2_shutin_offshore)

operator_plug_costs = left_join(operator_plug_costs, operator_FA, by="Operator.Name")%>%
  filter(Operator.Name!="INACTIVE OPERATOR")

FAtest = data%>%
  data.frame()%>%
  group_by(FS.Organization.Name, OFFSHORE_FLAG, FS.Summary)%>%
  summarise(ind_well_fa_onshore=sum(ind_well_fa_onshore,na.rm=T),
            ind_well_fa_offshore=sum(ind_well_fa_offshore,na.rm=T),
            n=n())

ggplot(data=operator_plug_costs)+
  geom_point(aes(x=bond, y=plug_cost1))+
  geom_abline(slope=1, intercept=0)+
  scale_x_continuous(label=dollar)+
  scale_y_continuous(label=dollar)+
  ylab("Total Plugging Liabilities")+
  labs(caption="Plot of firm-level total estimated plugging liabilities for all wells against required bonds. \n A line is plotted at y=x. Plugging costs assume onshore wells costs $12 per foot to plug and offshore wells cost $1.5 mil.")+
  xlab("Bond Amounts")+
  theme_bw()
ggsave(filename=paste(codedirectory,"/Figures/Bonds_v_TotPlug.png", sep=""),
       width=6,
       height=4)

ggplot(data=operator_plug_costs)+
  geom_point(aes(x=bond, y=sum_ind_well_bond))+
  geom_abline(slope=1, intercept=0)+
  scale_x_continuous(label=dollar)+
  scale_y_continuous(label=dollar)+
  ylab("Sum of Hypothetical Individual Well Bonds")+
  labs(caption="A line is drawn at x=y")+
  xlab("Actual Bond Amounts")+
  theme_bw()

ggsave(filename=paste(codedirectory,"/Figures/Blankets_v_IndBonds.png", sep=""),
       width=6,
       height=4)

ggplot(data=operator_plug_costs)+
  geom_point(aes(x=bond, y=sum_ind_well_bond))+
  geom_abline(slope=1, intercept=0)+
  scale_x_continuous(label=dollar, limits=c(0,3000000))+
  scale_y_continuous(label=dollar, limits=c(0,150000000))+
  ylab("Sum of Hypothetical Individual Well Bonds")+
  labs(caption="A line is drawn at x=y")+
  xlab("Bond Amounts")+
  theme_bw()
ggsave(filename=paste(codedirectory,"/Figures/Blankets_v_IndBonds_zoomed.png", sep=""),
       width=6,
       height=4)

ggplot(data=operator_plug_costs)+
  geom_point(aes(x=bond, y=plug_cost1_marginal))+
  geom_abline(slope=1, intercept=0)+
  scale_x_continuous(label=dollar)+
  scale_y_continuous(label=dollar)+
  ylab("Plugging costs for marginal wells")+
  labs(caption="A line is drawn at x=y.  Plug costs assume offshore wells cost $1.5mil, onshore cost $12 per foot.")+
  xlab("Bond Amounts")+
  theme_bw()
ggsave(filename=paste(codedirectory,"/Figures/Bonds_v_costs1_marginal.png", sep=""),
       width=6,
       height=4)

ggplot(data=operator_plug_costs)+
  geom_point(aes(x=bond, y=plug_cost1_marginal))+
  geom_abline(slope=1, intercept=0)+
  scale_x_continuous(label=dollar, limits=c(0,2000000))+
  scale_y_continuous(label=dollar, limits=c(0,60000000))+
  ylab("Plugging costs for marginal wells")+
  labs(caption="A line is drawn at x=y.  Plug costs assume offshore wells cost $1.5mil, onshore cost $12 per foot.")+
  xlab("Bond Amounts")+
  theme_bw()
ggsave(filename=paste(codedirectory,"/Figures/Bonds_v_costs1_marginal_zoomed.png", sep=""),
       width=6,
       height=4)

ggplot(data=operator_plug_costs)+
  geom_point(aes(x=bond, y=plug_cost1_shutin))+
  geom_abline(slope=1, intercept=0)+
  scale_x_continuous(label=dollar)+
  scale_y_continuous(label=dollar)+
  ylab("Plugging costs for shutin wells")+
  labs(caption="A line is drawn at x=y.  Plug costs assume offshore wells cost $1.5mil, onshore cost $12 per foot.")+
  xlab("Bond Amounts")+
  theme_bw()
ggsave(filename=paste(codedirectory,"/Figures/Bonds_v_costs1_shutin.png", sep=""),
       width=6,
       height=4)

ggplot(data=operator_plug_costs)+
  geom_point(aes(x=bond, y=plug_cost1_shutin))+
  geom_abline(slope=1, intercept=0)+
  scale_x_continuous(label=dollar, limits=c(0,2000000))+
  scale_y_continuous(label=dollar, limits=c(0,60000000))+
  ylab("Plugging costs for shutin wells")+
  labs(caption="A line is drawn at x=y.  Plug costs assume offshore wells cost $1.5mil, onshore cost $12 per foot.")+
  xlab("Bond Amounts")+
  theme_bw()
ggsave(filename=paste(codedirectory,"/Figures/Bonds_v_costs1_shutin_zoomed.png", sep=""),
       width=6,
       height=4)

ggplot(data=operator_plug_costs)+
  geom_point(aes(x=bond, y=plug_cost2_marginal))+
  geom_abline(slope=1, intercept=0)+
  scale_x_continuous(label=dollar)+
  scale_y_continuous(label=dollar)+
  ylab("Plugging costs for marginal wells")+
  labs(caption="A line is drawn at x=y.  Plug costs assume offshore wells cost $1.5mil, onshore cost $70k.")+
  xlab("Bond Amounts")+
  theme_bw()
ggsave(filename=paste(codedirectory,"/Figures/Bonds_v_costs2_marginal.png", sep=""),
       width=6,
       height=4)

ggplot(data=operator_plug_costs)+
  geom_point(aes(x=bond, y=plug_cost2_marginal))+
  geom_abline(slope=1, intercept=0)+
  scale_x_continuous(label=dollar, limits=c(0,2000000))+
  scale_y_continuous(label=dollar, limits=c(0,60000000))+
  ylab("Plugging costs for marginal wells")+
  labs(caption="A line is drawn at x=y.  Plug costs assume offshore wells cost $1.5mil, onshore cost $70k.")+
  xlab("Bond Amounts")+
  theme_bw()
ggsave(filename=paste(codedirectory,"/Figures/Bonds_v_costs2_marginal_zoomed.png", sep=""),
       width=6,
       height=4)

ggplot(data=operator_plug_costs)+
  geom_point(aes(x=bond, y=plug_cost2_shutin))+
  geom_abline(slope=1, intercept=0)+
  scale_x_continuous(label=dollar)+
  scale_y_continuous(label=dollar)+
  ylab("Plugging costs for shutin wells")+
  labs(caption="A line is drawn at x=y.  Plug costs assume offshore wells cost $1.5mil, onshore cost $70k")+
  xlab("Bond Amounts")+
  theme_bw()
ggsave(filename=paste(codedirectory,"/Figures/Bonds_v_costs2_shutin.png", sep=""),
       width=6,
       height=4)


ggplot(data=operator_plug_costs)+
  geom_point(aes(x=bond, y=plug_cost2_shutin))+
  geom_abline(slope=1, intercept=0)+
  scale_x_continuous(label=dollar, limits=c(0,2000000))+
  scale_y_continuous(label=dollar, limits=c(0,60000000))+
  ylab("Plugging costs for shutin wells")+
  labs(caption="A line is drawn at x=y.  Plug costs assume offshore wells cost $1.5mil, onshore cost $70k")+
  xlab("Bond Amounts")+
  theme_bw()

ggsave(filename=paste(codedirectory,"/Figures/Bonds_v_costs2_shutin_zoomed.png", sep=""),
       width=6,
       height=4)
