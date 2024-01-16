rm(list=ls())

library(tidyverse)
library(httr2)
library(RCurl)
library(httpuv)
library(jsonlite)
library(data.table)
library(ggplot2)


source("C:/Users/lbeatty/Documents/JMP/Build/SecretSauce.R")

base.url = 'https://api.enverus.com/v3/direct-access'

#############################
### GET Well Data ##############
#############################
token.path <- "https://api.enverus.com/v3/direct-access/tokens"

#headers
toke <- request(token.path) %>%
  req_body_json(list(secretKey = API_Key)) %>%
  req_perform()%>%
  resp_body_json()

###
my_token = toke$token


### GET DATA

well.path <- "https://api.enverus.com/v3/direct-access/wells"

wells = request(well.path)%>%
  req_auth_bearer_token(my_token)%>%
  req_url_query(StateProvince = "LA",
                StateProvince = "NGOM",
                DeletedDate = 'NULL',
                pagesize=10000)%>%
  req_perform()

tempdat = wells%>%
  resp_body_json(simplifyVector=T)%>%
  filter(substr(API_UWI_12,1,2)=="17")

doc.path <- "https://api.enverus.com/v3/direct-access/wells?docs"
docs = request(doc.path)%>%
  req_auth_bearer_token(my_token)%>%
  req_perform()


nxtpg = wells$headers$Link
nxtpg = str_match_all(nxtpg, "<(.*?)>")[[1]][1,2]
nxtpg = paste(base.url, nxtpg, sep="")

data=tempdat

request=1

while(nrow(tempdat)!=0){
  tempdat = request(nxtpg)%>%
    req_auth_bearer_token(my_token)%>%
    req_perform()
  
  nxtpg = tempdat$headers$Link
  nxtpg = str_match_all(nxtpg, "<(.*?)>")[[1]][1,2]
  nxtpg = paste(base.url, nxtpg, sep="")
  
  tempdat = tempdat%>%
    resp_body_json(simplifyVector=T)%>%
    filter(substr(API_UWI_12,1,2)=="17")
  
  data=rbind(data, tempdat)
  
  print(paste("Request #", request))
  request = request+1
}

#1.5mil offshore cost


write.csv(data, "C:/Users/lbeatty/OneDrive - Environmental Defense Fund - edf.org/OrphanedWells/State Advocacy/LA/Data/Enverus_Wells.csv")
