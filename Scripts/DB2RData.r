#read survey data from Access database and write new objects into a RData file
#An ODBC driver is required as connection is made using RODBC

library(RODBC)

#CruiseCode
CruiseCode <- "CE13015"
#output filename
op.file <- "CSHAS2013.RData"


#open connection to acoustic DB
channel <- odbcConnect(CruiseCode);
    
sql <- "SELECT FSS_Hauls.* FROM FSS_HAULS INNER JOIN FSS_EVENTS ON FSS_HAULS.HAULNO = FSS_EVENTS.EVENTNO WHERE FSS_EVENTS.EVENTTYPE = 'Haul'"

#Haul info
sql <- paste("SELECT h.HaulNo as HaulNo,h.Validity as Valid,h.ICES_Rectangle as ICES,h.ShotLatitude as ShotLat,",
             "h.Shot_E_or_W as ShotEW,h.ShotLongitude as ShotLon,h.HaulLatitude as HaulLat,h.Haul_E_or_w as HaulEW,",
             "h.HaulLongitude as HaulLon,h.Shot_Depth as ShotDepth,h.Haul_Depth as HaulDepth,e.Date as ShootDate,",
		 "e.Time as ShootTime,e.EndDate as HaulDate,e.EndTime as HaulTime FROM FSS_HAULS h INNER JOIN FSS_EVENTS e ",
             "ON h.haulid = cint(e.eventno) WHERE h.SurveyName = '",
             CruiseCode,"'"," AND e.eventtype = 'Haul' ORDER BY HaulNo",sep="")
    
Hauls <- sqlQuery(channel,sql);

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

    
#haul samples
sql <- paste("SELECT HaulNo, [Species name], [Sample Weight], [Sub-Sample Weight], ",
             "[Total Weight] FROM FSS_SAMPLES WHERE SurveyName = '",
             CruiseCode,"'" ," ORDER BY HaulNo",sep="")

Samples <- sqlQuery(channel,sql)
names(Samples)<-c("HaulNo","SpeciesName","SampleWeight","SubSampleWeight","TotalWeight")
Samples$SpeciesName <- toupper(Samples$SpeciesName)
#change SCOMBERUS SCOMBRUS to SCOMBER SCOMBRUS
Samples$SpeciesName[Samples$SpeciesName=='SCOMBERUS SCOMBRUS']<-'SCOMBER SCOMBRUS'
    
#Length-Freq info
sql <- paste("SELECT HaulNo,[Species name],[Length class],[Sub-sample frequency] ",
             "FROM FSS_LENGTHFREQ WHERE [Sub-sample frequency]>0 AND SurveyName = '",
             CruiseCode,"'"," ORDER BY HaulNo,[Species name],[Length class]",sep="")

LF <- sqlQuery(channel,sql)
names(LF) <- c("HaulNo","SpeciesName","LengthClass","SubSampleFrequency")
LF$SpeciesName <- toupper(LF$SpeciesName)
#change SCOMBERUS SCOMBRUS to SCOMBER SCOMBRUS
LF$SpeciesName[LF$SpeciesName=='SCOMBERUS SCOMBRUS']<-'SCOMBER SCOMBRUS'
    
#Aged data
sql <- paste("SELECT HaulNo,[Species name],[Index],[Length class],",
             "[Sex],[Gonad maturity],[Total body weight],[Primary age estimate] FROM ",
             "FSS_AGEDFISH WHERE SurveyName = '",
             CruiseCode,"'"," ORDER BY HaulNo,[Species name],[Index]",sep="")

Ages <- sqlQuery(channel,sql)
names(Ages) <- c("HaulNo","SpeciesName","Index","LenClass","Sex","Maturity","Weight","Age")
Ages$SpeciesName <- toupper(Ages$SpeciesName)
#change SCOMBERUS SCOMBRUS to SCOMBER SCOMBRUS
Ages$SpeciesName[Ages$SpeciesName=='SCOMBERUS SCOMBRUS']<-'SCOMBER SCOMBRUS'

#SA
sql <- "SELECT Strata,Transect,Region_class,
Depth,time_S,Date_E,PRC_NASC,Lat_S,Lon_S,Start_E_or_W,
Lat_E,Lon_E,End_E_or_W FROM FSS_NASC ORDER BY Strata, Transect, time_S";

SA <- sqlQuery(channel,sql);
 
#close the connection
odbcClose(channel);

head(Hauls)
head(Samples)
head(LF)
head(Ages)
head(SA)

save(list=c("Hauls","Samples","LF","Ages","SA"),file=op.file)
