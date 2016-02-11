#read survey data from Access database and write new objects into a RData file
#An ODBC driver is required as connection is made using RODBC

library(RODBC)
library(dplyr)

#clear memory
rm(list=ls())
gc()

#load in libraries, function definitions, plotting data
source("./Scripts/Init.r")

#load Cruise data file
#Cruise <- loadCruise(CruiseName = "CSHAS2015", SpeciesName = "Herring")
Cruise <- fLoadCruise(CruiseName = "CSHAS2015_Adaptive", SpeciesName = "Herring")
#Cruise <- loadCruise(CruiseName = "CSHAS2014", SpeciesName = "Herring")
#Cruise <- fLoadCruise(CruiseName = "NWHAS2015", SpeciesName = "Herring")
#Cruise <- loadCruise(CruiseName = "COM01_2011", SpeciesName = "Boarfish")   #2011 boarfish daylight hours
#Cruise <- loadCruise(CruiseName = "COM02_2011", SpeciesName = "Boarfish")   #2011 boarfish 24hrs


summary(Cruise)
CruiseCode <- getCode(Cruise)
CruiseName <- getName(Cruise)

op.file <- paste0(".\\Data\\",getName(Cruise),"\\",getName(Cruise),".RData")

saved.objs <- c()

#open connection to acoustic DB
channel <- odbcConnect(getCode(Cruise))
    
#sql <- "SELECT FSS_Hauls.* FROM FSS_HAULS INNER JOIN FSS_EVENTS ON FSS_HAULS.HAULNO = FSS_EVENTS.EVENTNO WHERE FSS_EVENTS.EVENTTYPE = 'Haul'"

#Haul info
# sql <- paste("SELECT h.HaulNo as HaulNo,h.Validity as Valid,h.ICES_Rectangle as ICES,h.ShotLatitude as ShotLat,",
#              "h.Shot_E_or_W as ShotEW,h.ShotLongitude as ShotLon,h.HaulLatitude as HaulLat,h.Haul_E_or_w as HaulEW,",
#              "h.HaulLongitude as HaulLon,h.Shot_Depth as ShotDepth,h.Haul_Depth as HaulDepth,e.Date as ShootDate,",
# 		 "e.Time as ShootTime,e.EndDate as HaulDate,e.EndTime as HaulTime FROM FSS_HAULS h INNER JOIN FSS_EVENTS e ",
#              "ON h.haulid = cint(e.eventno) WHERE h.SurveyName = '",
#              CruiseCode,"'"," AND e.eventtype = 'Haul' ORDER BY HaulNo",sep="")

sql <- paste("SELECT h.HaulNo as HaulNo,h.Validity as Valid,h.ICES_Rectangle as ICES,h.ShotLatitude as ShotLat,",
             "h.Shot_E_or_W as ShotEW,h.ShotLongitude as ShotLon,h.HaulLatitude as HaulLat,h.Haul_E_or_w as HaulEW,",
             "h.HaulLongitude as HaulLon,h.Shot_Depth as ShotDepth,h.Haul_Depth as HaulDepth,e.Date as ShootDate,",
             "e.Time as ShootTime,e.EndDate as HaulDate,e.EndTime as HaulTime FROM FSS_HAULS h INNER JOIN FSS_EVENTS e ",
             "ON h.haulno = cint(e.eventno) WHERE h.SurveyName = '",
             CruiseName,"'"," AND e.eventtype = 'Haul' ORDER BY HaulNo",sep="")

Hauls <- sqlQuery(channel,sql);

#additional information stored in table tblHaulinfo
sql <- paste("SELECT [Haul number] as HaulNo, [Mean depth] as TrawlDepth, [Speed thru water] as TowSpeed, [Marks of wire shot] as Wirelength ",
             "FROM tblHaulinfo WHERE [Cruise Code] = '",CruiseCode,"'", sep="")

HaulInfo <- sqlQuery(channel,sql);

#join the data sources
Hauls <- dplyr::inner_join(Hauls, HaulInfo, by="HaulNo")

if (nrow(Hauls)>0) {
  #validity
  Hauls$Valid <- Hauls$Valid=="v" | Hauls$Valid=="V"
  Hauls$Valid[is.na(Hauls$Valid)] <- FALSE
  Hauls$ShotLon[Hauls$ShotEW=="W"] <- -1*Hauls$ShotLon[Hauls$ShotEW=="W"]
  Hauls$HaulLon[Hauls$HaulEW=="W"] <- -1*Hauls$HaulLon[Hauls$HaulEW=="W"]
  
  #format times
  Hauls$ShootTime[nchar(Hauls$ShootTime)==3] <- paste("0",Hauls$ShootTime[nchar(Hauls$ShootTime)==3],sep="")
  Hauls$HaulTime[nchar(Hauls$HaulTime)==3] <- paste("0",Hauls$HaulTime[nchar(Hauls$HaulTime)==3],sep="")
  #insert colon separator
  Hauls$ShootTime <- paste(substr(Hauls$ShootTime,1,2),":",substr(Hauls$ShootTime,3,4),sep="")
  Hauls$HaulTime <- paste(substr(Hauls$HaulTime,1,2),":",substr(Hauls$HaulTime,3,4),sep="")
  
  Hauls$ShotDateTime <- paste(as.character(Hauls$ShootDate)," ",as.character(Hauls$ShootTime),sep="")
  Hauls$HaulDateTime <- paste(as.character(Hauls$HaulDate)," ",as.character(Hauls$HaulTime),sep="")
  
  Hauls$STime <- as.POSIXlt(strptime(Hauls$ShotDateTime,"%d/%m/%Y %H:%M"))
  Hauls$HTime <- as.POSIXlt(strptime(Hauls$HaulDateTime,"%d/%m/%Y %H:%M"))
  Hauls$SLat <- Hauls$ShotLat
  Hauls$SLon <- Hauls$ShotLon
  Hauls$SLon[Hauls$ShotEW=="W"] <- -1*Hauls$SLon[Hauls$ShotEW=="W"]
  Hauls$HLat <- Hauls$HaulLatitude
  Hauls$HLon <- Hauls$HaulLongitude
  Hauls$HLon[Hauls$Haul_E_or_W=="W"] <- -1*Hauls$HLon[Hauls$Haul_E_or_W=="W"]

  #cols to drop
  drops <- c("ShotEW","HaulEW","ShootTime","HaulTime","ShootDate","HaulDate","HTime","SLat","SLon")
  Hauls <- Hauls[,!(names(Hauls) %in% drops)]

  head(Hauls)
  
  saved.objs <- c(saved.objs, "Hauls")
}

#haul samples
#sql <- paste("SELECT HaulNo, [Species name], [Sample Weight], [Sub-Sample Weight], ",
#             "[Total Weight] FROM FSS_SAMPLES WHERE SurveyName = '",
#             CruiseCode,"'" ," ORDER BY HaulNo",sep="")

sql <- paste("SELECT HaulNo, [Species name], [Sample Weight], [Sub-Sample Weight], ",
             "[Total Weight] FROM FSS_SAMPLES WHERE SurveyName = '",
             CruiseName,"'" ," ORDER BY HaulNo",sep="")

Samples <- sqlQuery(channel,sql)

if (nrow(Samples)>0) {
  names(Samples)<-c("HaulNo","SpeciesName","SampleWeight","SubSampleWeight","TotalWeight")
  Samples$SpeciesName <- toupper(Samples$SpeciesName)
  #change SCOMBERUS SCOMBRUS to SCOMBER SCOMBRUS
  Samples$SpeciesName[Samples$SpeciesName=='SCOMBERUS SCOMBRUS']<-'SCOMBER SCOMBRUS'
  
  head(Samples)
  
  saved.objs <- c(saved.objs, "Samples")
  
}

    
#Length-Freq info
#sql <- paste("SELECT HaulNo,[Species name],[Length class],[Sub-sample frequency] ",
#             "FROM FSS_LENGTHFREQ WHERE [Sub-sample frequency]>0 AND SurveyName = '",
#             CruiseCode,"'"," ORDER BY HaulNo,[Species name],[Length class]",sep="")

sql <- paste("SELECT HaulNo,[Species name],[Length class],[Sub-sample frequency] ",
             "FROM FSS_LENGTHFREQ WHERE [Sub-sample frequency]>0 AND SurveyName = '",
             CruiseName,"'"," ORDER BY HaulNo,[Species name],[Length class]",sep="")

LF <- sqlQuery(channel,sql)

if (nrow(LF)) {
  names(LF) <- c("HaulNo","SpeciesName","LengthClass","SubSampleFrequency")
  LF$SpeciesName <- toupper(LF$SpeciesName)
  #change SCOMBERUS SCOMBRUS to SCOMBER SCOMBRUS
  LF$SpeciesName[LF$SpeciesName=='SCOMBERUS SCOMBRUS']<-'SCOMBER SCOMBRUS'
  
  head(LF)
  
  saved.objs <- c(saved.objs, "LF")
}

    
#Aged data
#sql <- paste("SELECT HaulNo,[Species name],[Index],[Length class],",
#             "[Sex],[Gonad maturity],[Total body weight],[Primary age estimate] FROM ",
#             "FSS_AGEDFISH WHERE SurveyName = '",
#            CruiseCode,"'"," ORDER BY HaulNo,[Species name],[Index]",sep="")

sql <- paste("SELECT HaulNo,[Species name],[Index],[Length class],",
             "[Sex],[Gonad maturity],[Total body weight],[Primary age estimate] FROM ",
             "FSS_AGEDFISH WHERE SurveyName = '",
             CruiseName,"'"," ORDER BY HaulNo,[Species name],[Index]",sep="")

Ages <- sqlQuery(channel,sql)

if (nrow(Ages)>0) {
  names(Ages) <- c("HaulNo","SpeciesName","Index","LenClass","Sex","Maturity","Weight","Age")
  Ages$SpeciesName <- toupper(Ages$SpeciesName)
  
  #change SCOMBERUS SCOMBRUS to SCOMBER SCOMBRUS
  Ages$SpeciesName[Ages$SpeciesName=='SCOMBERUS SCOMBRUS']<-'SCOMBER SCOMBRUS'
  
  #Sex of f or F is set to 1
  #Sex of m or M is set to 2
  #Sex of i or I is set to 0
  Ages$Sex <- as.character(Ages$Sex)
  Ages$Sex[toupper(Ages$Sex)=="F"] <- '1'
  Ages$Sex[toupper(Ages$Sex)=="M"] <- '2'
  Ages$Sex[toupper(Ages$Sex)=="I"] <- '0'
  
  Ages$Sex <- as.integer(Ages$Sex)
  
  head(Ages)
  
  saved.objs <- c(saved.objs, "Ages")
  
}

#SA
sql <- "SELECT Strata,Transect,Region_class,
Depth,time_S,Date_E,PRC_NASC,Lat_S,Lon_S,Start_E_or_W,
Lat_E,Lon_E,End_E_or_W FROM FSS_NASC ORDER BY Strata, Transect, time_S"

SA <- sqlQuery(channel,sql)

#remove any quotation marks from Region_class

if (nrow(SA)>0){saved.objs <- c(saved.objs, "SA")}

#Transects from table FSS_Transect
sql <- paste0("SELECT Transect, Stratum, Start_Date, Start_Time, Start_Lat_Deg, Start_Lat_Min,",
              "Start_Lon_Deg, Start_Lon_Min, End_Date, End_Time, End_Lat_Deg, End_Lat_Min,",
              "End_Lon_Deg, End_Lon_Min FROM FSS_Transect WHERE Survey_Code = '",
              CruiseCode,"' ORDER BY Transect")

DBTransects <- sqlQuery(channel,sql)

if (!is.null(nrow(DBTransects))) {
  
  #no factors required
  for (c in 1:ncol(DBTransects)){
    if (is.factor(DBTransects[,c])) {
      DBTransects[,c]<-levels(DBTransects[,c])[DBTransects[,c]]
    }
  }
  
  if (nrow(DBTransects)>0){saved.objs <- c(saved.objs, "DBTransects")}
}

#CTDs from table FSS_CTD
sql <- paste0("SELECT CTD, CTD_Date, CTD_Time, Depth, Lat_Deg, Lat_Min,",
              "Lon_Deg, Lon_Min FROM FSS_CTD WHERE Survey_Code = '",
              CruiseCode,"' ORDER BY CTD")

DBCTDs <- sqlQuery(channel,sql,stringsAsFactors=FALSE)

if (!is.null(nrow(DBCTDs))) {
  if (nrow(DBCTDs)>0){saved.objs <- c(saved.objs, "DBCTDs")}
}

#Strata from table FSS_Stratum
sql <- paste0("SELECT Stratum, ICES, Lat, Lon FROM FSS_Stratum WHERE Survey_Code = '",
              CruiseCode,"' ORDER BY Stratum_ID")

DBStrata <- sqlQuery(channel, sql)

if (!is.null(nrow(DBStrata))) {
  
  #no factors required
  for (c in 1:ncol(DBStrata)){
    if (is.factor(DBStrata[,c])) {
      DBStrata[,c]<-levels(DBStrata[,c])[DBStrata[,c]]
    }
  }
  
  if (nrow(DBStrata)>0){saved.objs <- c(saved.objs, "DBStrata")}
}

#close the connection
odbcClose(channel)

if (!is.null(saved.objs)) {
  save(list = saved.objs, file = op.file)
}

