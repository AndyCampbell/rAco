#upload the survey information held in the traditional survey spreadsheet


#upload transect data to access DB
#first, save the data on the Transects worksheet to a csv file and saved in the survey data directory
#include a header

#An ODBC driver is required as connection is made using RODBC

library(RODBC)
library(dplyr)

#clear memory
rm(list=ls())
gc()

#load in libraries, function definitions, plotting data
source("./Scripts/Init.r")

#load Cruise data file
#Cruise <- loadCruise(CruiseName = "NWHAS2015", SpeciesName = "Herring")
Cruise <- loadCruise(CruiseName = "CSHAS2015", SpeciesName = "Herring")
summary(Cruise)
CruiseCode <- getCode(Cruise)

#load csv file
#transects
tr <- read.csv(file = paste0(".//Data//",getName(Cruise),"//",getName(Cruise),"_Transect.csv"),
               header = TRUE,
               stringsAsFactors = FALSE)

#CTDs
ctd <- read.csv(file = paste0(".//Data//",getName(Cruise),"//",getName(Cruise),"_CTD.csv"),
                header = TRUE,
                stringsAsFactors = FALSE)

#second row contains header formats, remove it along with any blank lines on end
tr <- subset(tr,!is.na(Transect))
ctd <- subset(ctd,!is.na(CTD))

#Strata bounds
str <- read.csv(file = paste0(".//Data//",getName(Cruise),"//",getName(Cruise),"_Strata.csv"),
                header = TRUE,
                stringsAsFactors = FALSE)


#open connection to acoustic DB
channel <- odbcConnect(getCode(Cruise))

sql <- "DELETE FROM FSS_Transect"
ret <- sqlQuery(channel,sql)

for (i in 1:nrow(tr)){
  sql <- paste0("INSERT INTO FSS_Transect(Survey_Code,Transect,Stratum,Start_Date,Start_Time,Start_Lat_Deg,",
                "Start_Lat_Min,Start_Lon_Deg,Start_Lon_Min,End_Date,End_Time,End_Lat_Deg,End_Lat_Min,",
                "End_Lon_Deg,End_Lon_Min) VALUES('",CruiseCode,"','",tr[i,1],"','",tr[i,2],"','",tr[i,3],"','",tr[i,4],
                "',",tr[i,5],",",tr[i,6],",",tr[i,7],",",tr[i,8],",'",tr[i,9],"','",tr[i,10],
                "',",tr[i,11],",",tr[i,12],",",tr[i,13],",",tr[i,14],")")
  ret <- sqlQuery(channel,sql)
}

sql <- "DELETE FROM FSS_CTD"
ret <- sqlQuery(channel,sql)

for (i in 1:nrow(ctd)){

  sql <- "INSERT INTO FSS_CTD(Survey_Code,CTD,CTD_Date,CTD_Time"
  if (!ctd[i,4]=="") {sql <- paste0(sql,",Depth")}
  sql <- paste0(sql,",Lat_Deg,Lat_Min,Lon_Deg,Lon_Min) VALUES(")
  sql <- paste0(sql,"'",CruiseCode,"',",ctd[i,1],",'",ctd[i,2],"','",ctd[i,3],"'")
  if (!ctd[i,4]=="") {sql <- paste0(sql,",",ctd[i,4])}
  sql <- paste0(sql,",",ctd[i,5],",",ctd[i,6],",",ctd[i,7],",",ctd[i,8],")")  

  ret <- sqlQuery(channel,sql)
  
}

sql <- "DELETE FROM FSS_Stratum"
ret <- sqlQuery(channel,sql)

for (i in 1:nrow(str)){
  sql <- paste0("INSERT INTO FSS_Stratum(Survey_Code,Stratum,ICES,Lat,Lon) VALUES ('",
                str[i,1],"','",str[i,2],"','",str[i,3],"',",str[i,4],",",str[i,5],")")
  
  ret <- sqlQuery(channel,sql)
  
}


#close the connection
odbcClose(channel)
