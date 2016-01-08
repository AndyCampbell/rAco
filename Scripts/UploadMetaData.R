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
Cruise <- loadCruise(CruiseName = "NWHAS2015", SpeciesName = "Herring")
#Cruise <- loadCruise(CruiseName = "CSHAS2015", SpeciesName = "Herring")
#Cruise <- loadCruise(CruiseName = "CSHAS2014", SpeciesName = "Herring")

summary(Cruise)
CruiseCode <- getCode(Cruise)

#EV acoustic data
#open connection to acoustic DB
channel <- odbcConnect(getCode(Cruise))

evfiles <- list.files(path = paste0(".//Data//",getName(Cruise),"//Echoview"), pattern="*.csv")

sql <- "DELETE FROM FSS_NASC"
ret <- sqlQuery(channel,sql)

for (f in evfiles){

  ev <- read.csv(file = paste0(".//Data//",getName(Cruise),"//Echoview//",f),
                 header = TRUE,
                 stringsAsFactors = FALSE)
  
  if (nrow(ev)>0){
    
    Transect <- strsplit(strsplit(f,"\\.")[[1]][1],"_")[[1]][1]
    #strip off the 'T' 
    Transect <- substr(Transect,2,nchar(Transect))
    
    Strata <- strsplit(strsplit(f,"\\.")[[1]][1],"_")[[1]][2]
    #Strata is a number in the DB so still have to do the workaround (E replaced by 9 and D replaced by 8)
    Strata <- gsub("E","9",Strata)
    Strata <- gsub("D","8",Strata)
    
    cat(Transect,Strata,"\n")
    
    for (i in 1:nrow(ev)){

      #interpreted cols
      #start latitude
      Latitude <- ev[i,16]
      #Start_E_or_W
      if (ev[i,17]<0) {Start_E_or_W <- "W"} else {Start_E_or_W <- "E"}
      #Longitude
      Longitude <- abs(ev[i,17])
      #End Latitude
      EndLatitude <- ev[i,18]
      #End_E_or_W
      if (ev[i,19]<0) {End_E_or_W <- "W"} else {End_E_or_W <- "E"}
      #EndLongitude
      EndLongitude <- abs(ev[i,19])
      #time
      tm <- Sys.time()
      
      Start_E_or_W <- 
      sql <- paste0("INSERT INTO FSS_NASC(Cruise_Code,Strata,Transect,Region_ID,Region_name,Region_class,",
                    "Process_ID,Interval,Layer,Depth,PRC_NASC,Dist_S,Dist_E,VL_start,VL_end,Time_S,Date_E,",
                    "Time_E,Lat_S,Lon_S,Lat_E,Lon_E,Latitude,Start_E_or_W,Longitude,EndLatitude,End_E_or_W,",
                    "EndLongitude,MI_DateLoaded,FileName) VALUES ('",
                    CruiseCode,"',",Strata,",",Transect,",",ev[i,1],",'",trim(ev[i,2]),"','",trim(ev[i,3]),
                    "',",ev[i,4],",",ev[i,5],",",ev[i,6],",",ev[i,7],",",ev[i,8],",",ev[i,9],",",ev[i,10],
                    ",",ev[i,11],",",ev[i,12],",'",ev[i,13],"',",ev[i,14],",'",ev[i,15],"',",ev[i,16],",",
                    ev[i,17],",",ev[i,18],",",ev[i,19],",",Latitude,",'",Start_E_or_W,"',",Longitude,",",
                    EndLatitude,",'",End_E_or_W,"',",EndLongitude,",'",tm,"','",f,"')")
      ret <- sqlQuery(channel,sql)
    }
  }
}


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
