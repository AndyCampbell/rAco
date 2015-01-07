#Acoustic Analysis Functions

# Acoustic Cruise CTD/Haul/Transect/Strata Data file.
# This will be replaced when the data is read in from DB/text files

loadCruise <- function(CruiseName,SpeciesName) {

  #load cruise details from flat file and create cruise object
  fCruise <- paste("./Data/",CruiseName,"/",CruiseName,"_",SpeciesName,".dat",sep="")
  
  if (!file.exists(fCruise)){
    cat("No cruise data file found\n")
    return(NULL)
  }
  
  #read in cruise data 
  chrCruise <- scan(file=fCruise,what="character",sep="\n",quiet=TRUE)
  
  #cruise code
  if (!sum((toupper(substr(chrCruise,1,13))==toupper("Cruise Code::"))==1)){
    cat("Cannot find unique \"Cruise Code\" line in cruise data file\n")
    return(NULL)
  } else {
    code <- unlist(strsplit(chrCruise[toupper(substr(chrCruise,1,13))==toupper("Cruise Code::")],"::"))[2]  
  }
  
  #cruise name
  if (!sum((toupper(substr(chrCruise,1,13))==toupper("Cruise Name::"))==1)){
    cat("Cannot find unique \"Cruise Name\" line in cruise data file\n")
    return(NULL)
  } else {
    name <- unlist(strsplit(chrCruise[toupper(substr(chrCruise,1,13))==toupper("Cruise Name::")],"::"))[2]  
  }
  
  #description
  if (!sum((toupper(substr(chrCruise,1,6))==toupper("Desc::"))==1)){
    cat("Cannot find unique \"Desc\" line in cruise data file\n")
    return(NULL)
  } else {
    desc <- unlist(strsplit(chrCruise[toupper(substr(chrCruise,1,6))==toupper("Desc::")],"::"))[2]  
  }

  #vessel
  if (!sum((toupper(substr(chrCruise,1,8))==toupper("Vessel::"))==1)){
    cat("Cannot find unique \"Vessel\" line in cruise data file\n")
    return(NULL)
  } else {
    vessl <- unlist(strsplit(chrCruise[toupper(substr(chrCruise,1,8))==toupper("Vessel::")],"::"))[2]  
  }
  
  #start date
  if (!sum((toupper(substr(chrCruise,1,12))==toupper("Start Date::"))==1)){
    cat("Cannot find unique \"Start Date\" line in cruise data file\n")
    return(NULL)
  } else {
    stdt <- unlist(strsplit(chrCruise[toupper(substr(chrCruise,1,12))==toupper("Start Date::")],"::"))[2]  
  }

  #end date
  if (!sum((toupper(substr(chrCruise,1,10))==toupper("End Date::"))==1)){
    cat("Cannot find unique \"End Date\" line in cruise data file\n")
    return(NULL)
  } else {
    eddt <- unlist(strsplit(chrCruise[toupper(substr(chrCruise,1,10))==toupper("End Date::")],"::"))[2]  
  }
  
  #target common
  if (!sum((toupper(substr(chrCruise,1,23))==toupper("Target Species Common::"))==1)){
    cat("Cannot find unique \"Target Species Common\" line in cruise data file\n")
    return(NULL)
  } else {
    tgtc <- unlist(strsplit(chrCruise[toupper(substr(chrCruise,1,23))==toupper("Target Species Common::")],"::"))[2]  
  }

  #target scientific
  if (!sum((toupper(substr(chrCruise,1,27))==toupper("Target Species Scientific::"))==1)){
    cat("Cannot find unique \"Target Species Scientific\" line in cruise data file\n")
    return(NULL)
  } else {
    tgts <- unlist(strsplit(chrCruise[toupper(substr(chrCruise,1,27))==toupper("Target Species Scientific::")],"::"))[2]  
  }
  
  #create the cruise object via a call to the class constructor
  cruise(code=code,name=name,desc=desc,vessel=vessl,
         start_date=as.POSIXlt(strptime(stdt,"%Y-%m-%d %H:%M:%S")),
         end_date=as.POSIXlt(strptime(eddt, "%Y-%m-%d %H:%M:%S")),
         target_common=tgtc, target_scientific=tgts)

}

#loadMarkTypes <- function(CruiseCode){
loadMarkTypes <- function(CruiseName,SpeciesName){

  #load cruise details from flat file and create cruise object
  fCruise <- paste("./Data/",CruiseName,"/",CruiseName,"_",SpeciesName,".dat",sep="")
  
  if (!file.exists(fCruise)){
    cat("No cruise data file found\n")
    return(NULL)
  }
  
  #read in cruise data 
  chrCruise <- scan(file=fCruise,what="character",sep="\n",quiet=TRUE)

  #cruise code
  if (!sum((toupper(substr(chrCruise,1,13))==toupper("Cruise Code::"))==1)){
    cat("Cannot find unique \"Cruise Code\" line in cruise data file\n")
    return(NULL)
  } else {
    CruiseCode <- unlist(strsplit(chrCruise[toupper(substr(chrCruise,1,13))==toupper("Cruise Code::")],"::"))[2]  
  }
  
  #number of mark types defined
  num_mt <- sum((toupper(substr(chrCruise,1,11))==toupper("Mark Type::"))==1)
  
  if (num_mt==0) {
    cat("No mark type information read\n")
    return(NULL)
  }
  
  #initialise return object
  ret <- vector("list",num_mt)
  
  #find the position of the mark type lines
  mt_delim <- which(toupper(substr(chrCruise,1,11))==toupper("Mark Type::"))
  #add position of final line
  mt_delim <- c(mt_delim,length(chrCruise))
  
  for (mt in 1:num_mt){
    #Mark Type Name
    Mark_Type <- unlist(strsplit(chrCruise[mt_delim[mt]],"::"))[2]  

    #NASC Name - mandatory
    if (!sum((toupper(substr(chrCruise[mt_delim[mt]:mt_delim[mt+1]],1,11))==toupper("NASC Name::"))==1)){
      cat(paste("Cannot find NASC Name for Mark Type",Mark_Type,"\n"))
      return(NULL)
    } else {
      NASC_Name <- unlist(strsplit(chrCruise[mt_delim[mt]:mt_delim[mt+1]][toupper(substr(chrCruise[mt_delim[mt]:mt_delim[mt+1]],1,11))==toupper("NASC Name::")],"::"))[2]  
    }

    #Species Common - mandatory
    if (!sum((toupper(substr(chrCruise[mt_delim[mt]:mt_delim[mt+1]],1,16))==toupper("Species Common::"))==1)){
      cat(paste("Cannot find Common Species name for Mark Type",Mark_Type,"\n"))
      return(NULL)
    } else {
      Spe_Common <- unlist(strsplit(chrCruise[mt_delim[mt]:mt_delim[mt+1]][toupper(substr(chrCruise[mt_delim[mt]:mt_delim[mt+1]],1,16))==toupper("Species Common::")],"::"))[2]  
    }

    #Species Scientific
    if (!sum((toupper(substr(chrCruise[mt_delim[mt]:mt_delim[mt+1]],1,20))==toupper("Species Scientific::"))==1)){
      cat(paste("Cannot find Scientific Species name for Mark Type",Mark_Type,"\n"))
      return(NULL)
    } else {
      Spe_Sci <- unlist(strsplit(chrCruise[mt_delim[mt]:mt_delim[mt+1]][toupper(substr(chrCruise[mt_delim[mt]:mt_delim[mt+1]],1,20))==toupper("Species Scientific::")],"::"))[2]  
    }
    
    #Include (in abundance/biomass calculations) - mandatory
    if (!sum((toupper(substr(chrCruise[mt_delim[mt]:mt_delim[mt+1]],1,9))==toupper("Include::"))==1)){
      cat(paste("Cannot find Include flag value for Mark Type",Mark_Type,"\n"))
      return(NULL)
    } else {
      Include <- unlist(strsplit(chrCruise[mt_delim[mt]:mt_delim[mt+1]][toupper(substr(chrCruise[mt_delim[mt]:mt_delim[mt+1]],1,9))==toupper("Include::")],"::"))[2]  
      Include <- toupper(Include)=="TRUE"
    }

    #Haul assignment type - mandatory if include is true
    if ((!sum((toupper(substr(chrCruise[mt_delim[mt]:mt_delim[mt+1]],1,22))==toupper("Haul Assignment Type::"))==1)) & Include){
      cat(paste("Cannot find haul assignment type for Mark Type",Mark_Type,"\n"))
      return(NULL)
    } else {
      HaulAssType <- unlist(strsplit(chrCruise[mt_delim[mt]:mt_delim[mt+1]][toupper(substr(chrCruise[mt_delim[mt]:mt_delim[mt+1]],1,22))==toupper("Haul Assignment Type::")],"::"))[2]  
    }
    
    #Mixed - other species in the mix
    if (sum((toupper(substr(chrCruise[mt_delim[mt]:mt_delim[mt+1]],1,7))==toupper("Mixed::"))==1)){
      Mixed <- unlist(strsplit(chrCruise[mt_delim[mt]:mt_delim[mt+1]][toupper(substr(chrCruise[mt_delim[mt]:mt_delim[mt+1]],1,7))==toupper("Mixed::")],"::"))[2]  
    } else {
      Mixed <- ""
    }
    
    #Assigned Hauls - mandatory if include is true
    if ((!sum((toupper(substr(chrCruise[mt_delim[mt]:mt_delim[mt+1]],1,7))==toupper("Hauls::"))==1)) & Include){
      cat(paste("Cannot find assigned hauls for Mark Type",Mark_Type,"\n"))
      return(NULL)
    } else {
      Hauls <- unlist(strsplit(chrCruise[mt_delim[mt]:mt_delim[mt+1]][toupper(substr(chrCruise[mt_delim[mt]:mt_delim[mt+1]],1,7))==toupper("Hauls::")],"::"))[2]  
    }
    
    names(ret)[mt] = Mark_Type
    ret[[mt]] = marktype(name = Mark_Type,
                         cruise_code = CruiseCode,
                         NASC_name = NASC_Name,
                         species = Spe_Sci,
                         include = Include,
                         haul_assignment = HaulAssType,
                         hauls = unlist(strsplit(Hauls,",")),
                         mixed_with = (if (nchar(Mixed)>0) {unlist(strsplit(Mixed,","))} else {NA_character_}))
                             
  }
  
  ret  
  
#   if (CruiseCode=="CE11008"){
#     return(
#       list("Definitely Herring" = marktype(name = "Definitely Herring",
#                                            cruise_code = CruiseCode,
#                                            NASC_name = "Def Herring",
#                                            species = "CLUPEA HERRANGUS",
#                                            include = TRUE,
#                                            haul_assignment = "NEAREST",
#                                            hauls = c("5","6","7","8","9","10","14","18")),
#            "Probably Herring" = marktype(name = "Probably Herring",
#                                          cruise_code = CruiseCode,
#                                          NASC_name = "Probably Herring",
#                                          species = "CLUPEA HERRANGUS",
#                                          include = TRUE,
#                                          haul_assignment = "NEAREST",
#                                          hauls = c("5","6","7","8","9","10","14","18")),
#            "Herring In A Mix" = marktype(name = "Herring In A Mix",
#                                          cruise_code = CruiseCode,
#                                          NASC_name = "Herring in a mix",
#                                          species = "CLUPEA HERRANGUS",
#                                          include = TRUE,
#                                          haul_assignment = "NEAREST",
#                                          mixed_with = c("Scomber scombrus"),
#                                          hauls = c("1","4","12","17","23")),
#            "Possibly Herring" = marktype(name = "Possibly Herring",
#                                          cruise_code = CruiseCode,
#                                          NASC_name = "Possibly Herring",
#                                          species = "CLUPEA HERRANGUS",
#                                          include = FALSE,
#                                          haul_assignment = "NEAREST",
#                                          hauls = c("5","6","7","8","9","10","14","18")),
#            "Boarfish" = marktype(name = "Boarfish",
#                                  cruise_code = CruiseCode,
#                                  NASC_name = "Def Boarfish",
#                                  species = "CAPROS APER",
#                                  include = TRUE,
#                                  haul_assignment = "NEAREST",
#                                  hauls = c("17","26"))))
#   }
#   
#   if (CruiseCode=="CE12009"){
#     return(
#       list("Definitely Herring" = marktype(name = "Definitely Herring",
#                                            cruise_code = CruiseCode,
#                                            NASC_name = "Def Herring",
#                                            species = "CLUPEA HERRANGUS",
#                                            include = TRUE,
#                                            haul_assignment = "NEAREST",
#                                            hauls = c("1","4","5","7","9","17","18","19","22","23","24")),
#            "Probably Herring" = marktype(name = "Probably Herring",
#                                          cruise_code = CruiseCode,
#                                          NASC_name = "Probably Herring",
#                                          species = "CLUPEA HERRANGUS",
#                                          include = TRUE,
#                                          haul_assignment = "NEAREST",
#                                          hauls = c("1","4","5","7","9","17","18","19","22","23","24")),
#            "Herring In A Mix" = marktype(name = "Herring In A Mix",
#                                          cruise_code = CruiseCode,
#                                          NASC_name = "Herring in a mix",
#                                          species = "CLUPEA HERRANGUS",
#                                          include = TRUE,
#                                          haul_assignment = "NEAREST",
#                                          mixed_with = c("Scomber scombrus"),
#                                          hauls = c("8","12","16","22")),
#            "Possibly Herring" = marktype(name = "Possibly Herring",
#                                          cruise_code = CruiseCode,
#                                          NASC_name = "Possibly Herring",
#                                          species = "CLUPEA HERRANGUS",
#                                          include = FALSE,
#                                          haul_assignment = "NEAREST",
#                                          hauls = c("1","4","5","7","9","17","18","19","22","23","24")),
#            "Boarfish" = marktype(name = "Boarfish",
#                                  cruise_code = CruiseCode,
#                                  NASC_name = "Def Boarfish",
#                                  species = "CAPROS APER",
#                                  include = TRUE,
#                                  haul_assignment = "NEAREST",
#                                  hauls = c("17","26"))))
#   }
#   
#   if (CruiseCode=="CE13009"){
#     return(
#       list("Definitely Herring" = marktype(name = "Definitely Herring",
#                                            cruise_code = CruiseCode,
#                                            NASC_name = "Def Herring",
#                                            species = "CLUPEA HERRANGUS",
#                                            include = TRUE,
#                                            haul_assignment = "NEAREST",
#                                            hauls = c("3","4","5","7","8","14","17","18","19","21")),
#            "Probably Herring" = marktype(name = "Probably Herring",
#                                          cruise_code = CruiseCode,
#                                          NASC_name = "Probably Herring",
#                                          species = "CLUPEA HERRANGUS",
#                                          include = TRUE,
#                                          haul_assignment = "NEAREST",
#                                          hauls = c("3","4","5","7","8","14","17","18","19","21")),
#            "Herring In A Mix" = marktype(name = "Herring In A Mix",
#                                          cruise_code = CruiseCode,
#                                          NASC_name = "Herring in a mix",
#                                          species = "CLUPEA HERRANGUS",
#                                          include = TRUE,
#                                          haul_assignment = "NEAREST",
#                                          mixed_with = c("Scomber scombrus","Capros Aper"),
#                                          hauls = c("6","9","15")),
#            "Possibly Herring" = marktype(name = "Possibly Herring",
#                                          cruise_code = CruiseCode,
#                                          NASC_name = "Possibly Herring",
#                                          species = "CLUPEA HERRANGUS",
#                                          include = FALSE,
#                                          haul_assignment = "NEAREST",
#                                          hauls = c("3","4","5","7","8","14","17","18","19","21")),
#            "Boarfish" = marktype(name = "Boarfish",
#                                  cruise_code = CruiseCode,
#                                  NASC_name = "Def Boarfish",
#                                  species = "CAPROS APER",
#                                  include = TRUE,
#                                  haul_assignment = "NEAREST",
#                                  hauls = c("9","15"))))
#   }
#   
#   if (CruiseCode=="CE13015"){
#     #Celtic Sea Herring 2013
#     return(
#       list("Definitely Herring" = marktype(name = "Definitely Herring",
#                                            cruise_code = CruiseCode,
#                                            NASC_name = "Def Herring",
#                                            species = "CLUPEA HERRANGUS",
#                                            include = TRUE,
#                                            haul_assignment = "NEAREST",
#                                            hauls = c("12","16","17","18","20")),
#            "Probably Herring" = marktype(name = "Probably Herring",
#                                          cruise_code = CruiseCode,
#                                          NASC_name = "Probably Herring",
#                                          species = "CLUPEA HERRANGUS",
#                                          include = TRUE,
#                                          haul_assignment = "NEAREST",
#                                          hauls = c("12","16","17","18","20")),
#            "Herring In A Mix" = marktype(name = "Herring In A Mix",
#                                          cruise_code = CruiseCode,
#                                          NASC_name = "Herring in a mix",
#                                          species = "CLUPEA HERRANGUS",
#                                          include = TRUE,
#                                          haul_assignment = "NEAREST",
#                                          mixed_with = c("Scomber scombrus","Sprattus sprattus"),
#                                          hauls = c("12","16","17","18","20")),
#            "Possibly Herring" = marktype(name = "Possibly Herring",
#                                          cruise_code = CruiseCode,
#                                          NASC_name = "Possibly Herring",
#                                          species = "CLUPEA HERRANGUS",
#                                          include = FALSE,
#                                          haul_assignment = "NEAREST",
#                                          hauls = c("12","16","17","18","20"))))    
#   }
#   
#   if (CruiseCode=="CE14010"){
#     return(
#       list("Definitely Herring" = marktype(name = "Definitely Herring",
#                                            cruise_code = CruiseCode,
#                                            NASC_name = "Def Herring",
#                                            species = "CLUPEA HERRANGUS",
#                                            include = TRUE,
#                                            haul_assignment = "NEAREST",
#                                            hauls = c("6","8","9","10","13","16","20")),
#            "Probably Herring" = marktype(name = "Probably Herring",
#                                          cruise_code = CruiseCode,
#                                          NASC_name = "Probably Herring",
#                                          species = "CLUPEA HERRANGUS",
#                                          include = TRUE,
#                                          haul_assignment = "NEAREST",
#                                          hauls = c("6","8","9","10","13","16","20")),
#            "Herring In A Mix" = marktype(name = "Herring In A Mix",
#                                          cruise_code = CruiseCode,
#                                          NASC_name = "Herring in a mix",
#                                          species = "CLUPEA HERRANGUS",
#                                          include = TRUE,
#                                          haul_assignment = "NEAREST",
#                                          mixed_with = c("Scomber scombrus","Capros Aper"),
#                                          hauls = c("25")),
#            "Possibly Herring" = marktype(name = "Possibly Herring",
#                                          cruise_code = CruiseCode,
#                                          NASC_name = "Possibly Herring",
#                                          species = "CLUPEA HERRANGUS",
#                                          include = FALSE,
#                                          haul_assignment = "NEAREST",
#                                          hauls = c("6","8","9","10","13","16","20")),
#            "Boarfish" = marktype(name = "Boarfish",
#                                  cruise_code = CruiseCode,
#                                  NASC_name = "Def Boarfish",
#                                  species = "CAPROS APER",
#                                  include = TRUE,
#                                  haul_assignment = "NEAREST",
#                                  hauls = c("7","11","19"))))
#   }
#   
#   #celtic sea herring 2012
#   if (CruiseCode=="CE12014"){
#     return(
#       list("Definitely Herring" = marktype(name = "Definitely Herring",
#                                            cruise_code = CruiseCode,
#                                            NASC_name = "Def Herring",
#                                            species = "CLUPEA HERRANGUS",
#                                            include = TRUE,
#                                            haul_assignment = "NEAREST",
#                                            hauls = c("2","8","9","10","13","14","15","16","17","18","19","20")),
#            "Probably Herring" = marktype(name = "Probably Herring",
#                                          cruise_code = CruiseCode,
#                                          NASC_name = "Probably Herring",
#                                          species = "CLUPEA HERRANGUS",
#                                          include = TRUE,
#                                          haul_assignment = "NEAREST",
#                                          hauls = c("2","8","9","10","13","14","15","16","17","18","19","20")),
#            "Herring In A Mix" = marktype(name = "Herring In A Mix",
#                                          cruise_code = CruiseCode,
#                                          NASC_name = "Herring in a mix",
#                                          species = "CLUPEA HERRANGUS",
#                                          include = TRUE,
#                                          haul_assignment = "NEAREST",
#                                          mixed_with = c("Scomber scombrus","Sprattus sprattus"),
#                                          hauls = c("8","14")),
#            "Possibly Herring" = marktype(name = "Possibly Herring",
#                                          cruise_code = CruiseCode,
#                                          NASC_name = "Possibly Herring",
#                                          species = "CLUPEA HERRANGUS",
#                                          include = FALSE,
#                                          haul_assignment = "NEAREST",
#                                          hauls = c("2","8","9","10","13","14","15","16","17","18","19","20"))))
#   }
#   
#   
#   if (CruiseCode=="BFAS2013"){
#     return(
#       list("Definitely Boarfish" = marktype(name = "Definitely Boarfish",
#                                             cruise_code = CruiseCode,
#                                             NASC_name = "Def Boarfish",
#                                             species = "CAPROS APER",
#                                             include = TRUE,
#                                             haul_assignment = "NEAREST",
#                                             hauls = c("1","3","4","6","8","9","11","13","21","22","24","25","26","27","28","30","35","36")),
#            "Probably Boarfish" = marktype(name = "Probably Boarfish",
#                                           cruise_code = CruiseCode,
#                                           NASC_name = "Prob Boarfish",
#                                           species = "CAPROS APER",
#                                           include = TRUE,
#                                           haul_assignment = "NEAREST",
#                                           hauls = c("1","3","4","6","8","9","11","13","21","22","24","25","26","27","28","30")),
#            "Boarfish in a Mix" = marktype(name = "Boarfish in a Mix",
#                                           cruise_code = CruiseCode,
#                                           NASC_name = "Boarfish in a Mix",
#                                           species = "CAPROS APER",
#                                           include = TRUE,
#                                           haul_assignment = "NEAREST",
#                                           hauls = c("35","36")),
#            "Possibly Boarfish" = marktype(name = "Possibly Boarfish",
#                                           cruise_code = CruiseCode,
#                                           NASC_name = "Poss Boarfish",
#                                           species = "CAPROS APER",
#                                           include = FALSE,
#                                           haul_assignment = "NEAREST",
#                                           hauls = c("1","3","4","6","8","9","11","13","21","22","24","25","26","27","28","30","35","36"))))
#   }
  
  
  
}


loadCTDs <- function(CruiseCode,CruiseName) {

  fCTD <- paste("./Data/",CruiseName,"/",CruiseName,"_CTD.csv",sep="")
  
  if (!file.exists(fCTD)) {
    cat("No CTD data file found\n")
    return(NULL)
  }
  
  #load in CTD information from csv file
  df.ctd <- read.csv(
    file = fCTD,
    header = TRUE,
    colClasses=c(rep("numeric",3)));
  
  ret <- c();
  
  for (i in 1:nrow(df.ctd)){
    
    #create transect object
    ret <- c(ret,
             ctdstation(lat=df.ctd$Lat[i],
                        lon=df.ctd$Lon[i],
                        code=as.character(df.ctd$Station[i]),
                        cruise_code=CruiseCode));
  }
  
  names(ret) <- df.ctd$Station;
  
  ret;
  
}

loadHauls <- function(CruiseCode) {
  
    ret<-c()
    
    for (h in 1:nrow(Hauls)){
    
      #species list
      spe <- vector("list",length=length(Samples$HaulNo[Samples$HaulNo==Hauls$HaulNo[h]]))
      names(spe) <- toupper(Samples$SpeciesName[Samples$HaulNo==Hauls$HaulNo[h]])
      
      fill <- vector("list",length=9)
      names(fill) <- c("samp.wgt","sub.samp.wgt","tot.wgt","len.class","num.at.len",
                       "length","weight","age","maturity")
      
      if (length(spe)>0) {
        
        for (s in 1:length(spe)){
          fill$samp.wgt<-Samples$SampleWeight[Samples$HaulNo==Hauls$HaulNo[h] & Samples$SpeciesName==names(spe)[s]]
          fill$sub.samp.wgt<-Samples$SubSampleWeight[Samples$HaulNo==Hauls$HaulNo[h] & Samples$SpeciesName==names(spe)[s]]
          fill$tot.wgt<-Samples$TotalWeight[Samples$HaulNo==Hauls$HaulNo[h] & Samples$SpeciesName==names(spe)[s]]
          fill$len.class<-LF$LengthClass[LF$HaulNo==Hauls$HaulNo[h] & LF$SpeciesName==names(spe)[s]]
          fill$num.at.len<-LF$SubSampleFrequency[LF$HaulNo==Hauls$HaulNo[h] & LF$SpeciesName==names(spe)[s]]
          fill$length<-Ages$LenClass[Ages$HaulNo==Hauls$HaulNo[h] & Ages$SpeciesName==names(spe)[s]]
          fill$weight<-Ages$Weight[Ages$HaulNo==Hauls$HaulNo[h] & Ages$SpeciesName==names(spe)[s]]
          fill$age<-Ages$Age[Ages$HaulNo==Hauls$HaulNo[h] & Ages$SpeciesName==names(spe)[s]]
          fill$maturity<-Ages$Maturity[Ages$HaulNo==Hauls$HaulNo[h] & Ages$SpeciesName==names(spe)[s]]
          spe[[s]]<-fill
        }
      }      
      
      #create new haul object
#      tmp <- haul(code=as.character(Hauls$HaulNo[h]),
#                  cruise_code = CruiseCode,
#                  valid = Hauls$Valid[h],
#                  shoot_wp = waypoint(Hauls$ShotLat[h],Hauls$ShotLon[h],Hauls$ShotDateTime[h]),
#                  haul_wp = waypoint(Hauls$HaulLat[h],Hauls$HaulLon[h],Hauls$HaulDateTime[h]),
#                  species = spe)

      #create new haul object
      tmp <- haul(code=as.character(Hauls$HaulNo[h]),
                  cruise_code = CruiseCode,
                  valid = Hauls$Valid[h],
                  shoot_wp = waypoint(lat=Hauls$ShotLat[h],
                                      lon=Hauls$ShotLon[h],
                                      time=as.POSIXlt(strptime(Hauls$ShotDateTime[h],"%d/%m/%Y %H:%M"))),
                  haul_wp = waypoint(lat=Hauls$HaulLat[h],
                                     lon=Hauls$HaulLon[h],
                                     time=as.POSIXlt(strptime(Hauls$HaulDateTime[h],"%d/%m/%Y %H:%M"))),
                  species = spe)
      
      ret<-c(ret,tmp)
      #ret[h] <- tmp
      
    }
    
    names(ret)<-unlist(lapply(ret,getCode))
    
    ret
    
  #}
  
  
#   if (CruiseCode=="CE14010"){
#     
#     #file created from 32bit R
#     load(file="C:\\Dev\\Acoustics\\AcoS4\\Data\\NWHAS2014\\NWHAS2014.RData")
#     
#     ret<-c()
#     
#     if (nrow(Hauls)>0){
#       
#       for (h in 1:nrow(Hauls)){
#         
#         #species list
#         spe <- vector("list",length=length(Samples$HaulNo[Samples$HaulNo==Hauls$HaulNo[h]]))
#         names(spe) <- toupper(Samples$SpeciesName[Samples$HaulNo==Hauls$HaulNo[h]])
#         
#         fill <- vector("list",length=9)
#         names(fill) <- c("samp.wgt","sub.samp.wgt","tot.wgt","len.class","num.at.len",
#                          "length","weight","age","maturity")
#         
#         if (length(spe)>0) {
#           
#           for (s in 1:length(spe)){
#             fill$samp.wgt<-Samples$SampleWeight[Samples$HaulNo==Hauls$HaulNo[h] & Samples$SpeciesName==names(spe)[s]]
#             fill$sub.samp.wgt<-Samples$SubSampleWeight[Samples$HaulNo==Hauls$HaulNo[h] & Samples$SpeciesName==names(spe)[s]]
#             fill$tot.wgt<-Samples$TotalWeight[Samples$HaulNo==Hauls$HaulNo[h] & Samples$SpeciesName==names(spe)[s]]
#             fill$len.class<-LF$LengthClass[LF$HaulNo==Hauls$HaulNo[h] & LF$SpeciesName==names(spe)[s]]
#             fill$num.at.len<-LF$SubSampleFrequency[LF$HaulNo==Hauls$HaulNo[h] & LF$SpeciesName==names(spe)[s]]
#             fill$length<-Ages$LenClass[Ages$HaulNo==Hauls$HaulNo[h] & Ages$SpeciesName==names(spe)[s]]
#             fill$weight<-Ages$Weight[Ages$HaulNo==Hauls$HaulNo[h] & Ages$SpeciesName==names(spe)[s]]
#             fill$age<-Ages$Age[Ages$HaulNo==Hauls$HaulNo[h] & Ages$SpeciesName==names(spe)[s]]
#             fill$maturity<-Ages$Maturity[Ages$HaulNo==Hauls$HaulNo[h] & Ages$SpeciesName==names(spe)[s]]
#             spe[[s]]<-fill
#           }
#         }
#         
#         #create new haul object
#         tmp <- haul(code=as.character(Hauls$HaulNo[h]),
#                     cruise_code = CruiseCode,
#                     valid = Hauls$Valid[h],
#                     shoot_wp = waypoint(Hauls$SLat[h],Hauls$SLon[h],Hauls$STime[h]),
#                     haul_wp = waypoint(Hauls$HLat[h],Hauls$HLon[h],Hauls$HTime[h]),
#                     species = spe)
#         
#         ret<-c(ret,tmp)
#         
#       }
#       
#       names(ret)<-unlist(lapply(ret,getCode))
#       
#     }
#     
#   }
#   
#   return(ret)
  
}


loadTransects <- function(CruiseCode,CruiseName){
  
#   df.tran <- read.csv(
#     file = paste("./Data/",CruiseName,"/",CruiseName,"_Transect_S_E.csv",sep=""),
#     header = TRUE,
#     colClasses=c(rep("character",4),
#                  rep("numeric",4),
#                  rep("character",2),
#                  rep("numeric",4)));

  df.tran <- read.csv(
    file = paste("./Data/",CruiseName,"/",CruiseName,"_Transect.csv",sep=""),
    header = TRUE,
    colClasses=c(rep("character",4),
                 rep("numeric",2),
                 rep("character",2),
                 rep("numeric",2)));
  
  ret <- c();
  
  for (i in 1:nrow(df.tran)){
    
    #start time
    st.time <- as.POSIXlt(strptime(paste(df.tran$S_Date[i],df.tran$S_Time[i],sep=" "),"%d/%m/%Y %H:%M"));
    #end time
    end.time <- as.POSIXlt(strptime(paste(df.tran$E_Date[i],df.tran$E_Time[i],sep=" "),"%d/%m/%Y %H:%M"));
    
    #create transect object
#     ret <- c(ret,
#              transect(code = as.character(df.tran$Trans[i]),
#                       stratum_code = df.tran$Str[i],
#                       cruise_code = CruiseCode,
#                       start_pos = geopoint(lat=df.tran$S_LatD[i]+df.tran$S_LatM[i]/60.0,
#                                            lon=df.tran$S_LonD[i]-df.tran$S_LonM[i]/60.0),
#                       end_pos = geopoint(lat=df.tran$E_LatD[i]+df.tran$E_LatM[i]/60.0,
#                                          lon=df.tran$E_LonD[i]-df.tran$E_LonM[i]/60.0),
#                       start_time = st.time,
#                       end_time = end.time));
    
    ret <- c(ret,
             transect(code = as.character(df.tran$Trans[i]),
                      stratum_code = df.tran$Str[i],
                      cruise_code = CruiseCode,
                      start_pos = geopoint(lat=df.tran$S_Lat[i],lon=df.tran$S_Lon[i]),
                      end_pos = geopoint(lat=df.tran$E_Lat[i],lon=df.tran$E_Lon[i]),
                      start_time = st.time,
                      end_time = end.time));
  }
  
  names(ret) <- df.tran$Trans;
  
  return(ret);
  
}

loadStrata <- function(CruiseCode,CruiseName){
  
#   df.strata <- read.csv(
#     file = paste("./Data/",CruiseName,"/",CruiseName,"_Strata.csv",sep=""),
#     header = TRUE,
#     colClasses=c(rep("character",3),
#                  rep("numeric",2),
#                  rep("character",1)));

  df.strata <- read.csv(
    file = paste("./Data/",CruiseName,"/",CruiseName,"_Strata.csv",sep=""),
    header = TRUE,
    colClasses=c(rep("character",3),
                 rep("numeric",2)));
  
  ret <- c();
  
  #unique strata codes
  stratacodes <- unique(df.strata$Stratum);
  
  for (i in 1:length(stratacodes)){
    
    ret <- c(ret,
             stratum(code = stratacodes[i],
                     cruise_code = CruiseCode,
                     type = "TO DO",
                     boundary_lat = df.strata$Lat[df.strata$Stratum == stratacodes[i]],
                     boundary_lon = df.strata$Lon[df.strata$Stratum == stratacodes[i]],
                     ICESarea = unique(df.strata$ICES[df.strata$Stratum == stratacodes[i]]))
    );
    
  }
  
  names(ret) <- stratacodes;
  return(ret);
  
}

loadSpeciesDetails <- function(CruiseCode){

  ret <-
    list(
       "Herring" = targetspecies(species="Clupea Herrangus",common_name="Herring",LF_bin_size=0.5,
                                 ts_a=20,ts_b=-71.2,ts_LFint=0.5,imm_codes=as.character(c(1,2)),
                                 mat_codes=as.character(c(3,4,5,6)),spt_codes=as.character(c(7,8)),
                                 est_abd=TRUE,est_by_age=TRUE,est_by_mat=TRUE),
      "Sprat" = targetspecies(species="Sprattus sprattus",common_name="Sprat",LF_bin_size=0.5,
                              imm_codes="",mat_codes="",spt_codes="",
                              ts_a=20,ts_b=-71.2,ts_LFint=0.5,est_abd=FALSE,est_by_age=FALSE,
                              est_by_mat=FALSE),
      "Scad" = targetspecies(species="Trachurus trachurus",common_name="Scad",LF_bin_size=1.0,
                             imm_codes="",mat_codes="",spt_codes="",
                             ts_a=20,ts_b=-67.5,ts_LFint=1.0,est_abd=FALSE,est_by_age=FALSE,
                             est_by_mat=FALSE),
      "Mackerel" = targetspecies(species="Scomber scombrus",common_name="Mackerel",LF_bin_size=1.0,
                                 imm_codes="",mat_codes="",spt_codes="",
                                 ts_a=20,ts_b=-84.9,ts_LFint=1.0,est_abd=FALSE,est_by_age=FALSE,
                                 est_by_mat=FALSE),
      "Pilchard" = targetspecies(species="Sardina pilchardus",common_name="Sardine",LF_bin_size=1.0,
                                 imm_codes="",mat_codes="",spt_codes="",
                                 ts_a=20,ts_b=-66.4,ts_LFint = 1.0,est_abd=FALSE,est_by_age=FALSE,
                                 est_by_mat=FALSE),
      "Boarfish" = targetspecies(species="Capros aper",common_name="Boarfish",LF_bin_size=0.5,
                                 imm_codes=c("Immature"),mat_codes=c("Mature"),spt_codes=c("Spent"),
                                 ts_a=20,ts_b=-66.2,ts_LFint=0.5,est_abd=FALSE,est_by_age=FALSE,
                                 est_by_mat=FALSE),
      "Whiting" = targetspecies(species="Merlangus merlangus",common_name="Whiting",LF_bin_size=1,
                                imm_codes="",mat_codes="",spt_codes="",
                                ts_a=0,ts_b=0,ts_LFint=0,est_abd=FALSE,est_by_age=FALSE,
                                est_by_mat=FALSE),
      "Grey Gurnard" = targetspecies(species="Eutrigla Gurnardus",common_name="Grey Gurnard",LF_bin_size=1,
                                     imm_codes="",mat_codes="",spt_codes="",
                                     ts_a=0,ts_b=0,ts_LFint=0,est_abd=FALSE,est_by_age=FALSE,
                                     est_by_mat=FALSE),
      "Jellyfish" = targetspecies(species="Aurelia sp.",common_name="Jellyfish",LF_bin_size=1,
                                     imm_codes="",mat_codes="",spt_codes="",
                                     ts_a=0,ts_b=0,ts_LFint=0,est_abd=FALSE,est_by_age=FALSE,
                                     est_by_mat=FALSE),
      "Monk" = targetspecies(species="Lophius Budegassa",common_name="Monk",LF_bin_size=1,
                                  imm_codes="",mat_codes="",spt_codes="",
                                  ts_a=0,ts_b=0,ts_LFint=0,est_abd=FALSE,est_by_age=FALSE,
                                  est_by_mat=FALSE),
      "Hake" = targetspecies(species="Merluccius merluccius",common_name="Hake",LF_bin_size=1,
                             imm_codes="",mat_codes="",spt_codes="",
                             ts_a=0,ts_b=0,ts_LFint=0,est_abd=FALSE,est_by_age=FALSE,
                             est_by_mat=FALSE),
      "Argentine" = targetspecies(species="Argentina sphyrena",common_name="Argentine",LF_bin_size=1,
                             imm_codes="",mat_codes="",spt_codes="",
                             ts_a=0,ts_b=0,ts_LFint=0,est_abd=FALSE,est_by_age=FALSE,
                             est_by_mat=FALSE),    
      "Red Gurnard" = targetspecies(species="ASPITRIGLIA CUCULUS",common_name="Red Gurnard",LF_bin_size=1,
                                     imm_codes="",mat_codes="",spt_codes="",
                                     ts_a=0,ts_b=0,ts_LFint=0,est_abd=FALSE,est_by_age=FALSE,
                                     est_by_mat=FALSE),
      "Norway Pout" = targetspecies(species="TRISOPTERUS ESMARKII",common_name="Norway Pout",LF_bin_size=1,
                                    imm_codes="",mat_codes="",spt_codes="",
                                    ts_a=0,ts_b=0,ts_LFint=0,est_abd=FALSE,est_by_age=FALSE,
                                    est_by_mat=FALSE),
      "Horse Mackerel" = targetspecies(species="Trachurus trachurus",common_name="Horse Mackerel",LF_bin_size=1.0,
                                 imm_codes="",mat_codes="",spt_codes="",
                                 ts_a=0,ts_b=0,ts_LFint=0,est_abd=FALSE,est_by_age=FALSE,
                                 est_by_mat=FALSE),
      "Poor Code" = targetspecies(species="TRISOPTERUS MINUTUS",common_name="Poor Cod",LF_bin_size=1.0,
                                       imm_codes="",mat_codes="",spt_codes="",
                                       ts_a=0,ts_b=0,ts_LFint=0,est_abd=FALSE,est_by_age=FALSE,
                                       est_by_mat=FALSE),
      "Ling" = targetspecies(species="Molva Molva",common_name="Ling",LF_bin_size=1.0,
                                  imm_codes="",mat_codes="",spt_codes="",
                                  ts_a=0,ts_b=0,ts_LFint=0,est_abd=FALSE,est_by_age=FALSE,
                                  est_by_mat=FALSE),
      "Blue Whiting" = targetspecies(species="MICROMESISTIUS POUTASSOU",common_name="Blue Whiting",LF_bin_size=1,
                                imm_codes="",mat_codes="",spt_codes="",
                                ts_a=0,ts_b=0,ts_LFint=0,est_abd=FALSE,est_by_age=FALSE,
                                est_by_mat=FALSE),
      "Squid" = targetspecies(species="ALL SQUID",common_name="Squid",LF_bin_size=1,
                                     imm_codes="",mat_codes="",spt_codes="",
                                     ts_a=0,ts_b=0,ts_LFint=0,est_abd=FALSE,est_by_age=FALSE,
                                     est_by_mat=FALSE)
    )
  
  
  #for Boarfish surveys make boarfish the estimated species
  if (CruiseCode=='BFAS2013') {
    ret[['Herring']]@est_abd=FALSE
    ret[['Boarfish']]@est_abd=TRUE
    ret[['Boarfish']]@est_by_age=TRUE
    ret[['Boarfish']]@est_by_mat=TRUE
  }
  
  ret
  
}


Area<-function(corners){
  
  #cat(corners[[1]],corners[[2]],"\n")
  # Doonan
  # find area of a polygon
  # corners$x, $y corners of polygon
  #  Area(list(x=c(0,2,3,4,4),y=c(0,2,1,2,0))) -->5
  #
  if(length(corners$x)<3 | length(corners$y)<3 ) {
    print("ERROR Area(): need 3 or more points ************")
    return(NULL)
  }
  if(length(corners$x) != length(corners$y) ) {
    print("ERROR Area(): points in corners$y not equal to corners$x ************")
    return(NULL)
  }
  
  TotArea<-0
  SubArea<-0
  n<-length(corners$x)
  I0<-0
  
  for(j in c(1:(n))) {
    if(!is.na(corners$y[j]) & !is.na(corners$x[j]) & I0 ==0) I0<-j
  }
  
  for(j in c(I0:(n))) {
    i<-j
    i1<-j+1
    if (i1>n)i1<-I0 
    if(is.na(corners$y[i1]) | is.na(corners$x[i1])) i1<-I0
    # still problems if 2 NA's in a row
    
    if(!is.na(corners$y[i]) & !is.na(corners$x[i])){
      SubArea<-SubArea + corners$x[i]*corners$y[i1]-
        corners$x[i1]*corners$y[i]
    } else {
      TotArea<-TotArea + abs(SubArea)
      SubArea<-0
      I0<-i1
    }
  }
  TotArea<-(TotArea + abs(SubArea))/2
  return(TotArea)
}

AreaLongLat<-function(Long,Lat){
  # find area of polygon of lat,long in decimal form (eg 174.5 = 174deg. 30 min)
  #  km^2
  # if only Long then assume a list (x,y)
  #
  # eg  for SurveyBoundry we get 646.7414  (cf 647.3 for rstn on frc)
  #            (BonNonSub()  2.6% too high? - some small errors?)
  #  x<-matrix(scan("SurveyBoundry"),ncol=4,byrow=T)
  #  AreaLongLat(x[,3]+x[,4]/60,x[,1]+x[,2]/60)
  
  if(missing(Lat))Poly<-Long else Poly<-list(x=Long,y=Lat)
  
  # convert to Km
  Tna<-!is.na(Poly$x)
  Poly$x[Tna]<-Poly$x[Tna]* (111.41*cos(pi/180*Poly$y[Tna])-
    0.01*cos(3*pi/180*Poly$y[Tna]))
  Tna<-!is.na(Poly$y)
  Poly$y[Tna]<-Poly$y[Tna]* (111.14-
    0.56*cos(2*pi/180*Poly$y[Tna]))
  return(Area(Poly))
}


ToKm<-function(Pt1y,Pt1x,Pt2y,Pt2x){
  #Pt=c(-Lat,Long)
  # converts 2 -lat,longs into a km dist.
  #  1.852 km/nmile
  Pt1y<-as.double(Pt1y)
  Pt1x<-as.double(Pt1x)
  Pt2x<-as.double(Pt2x)
  Pt2y<-as.double(Pt2y)
  a1<-111.14-.28*(cos(-2*Pt1y*pi/180)+cos(-2*Pt2y*pi/180))
  a0<-55.71*(cos(-Pt1y*pi/180)+cos(-Pt2y*pi/180))-.25*(cos(3*Pt1y*pi/180)+cos(3*Pt2y*pi/180))
  dist<-(a1*(Pt2y-Pt1y))^2+(a0*(Pt2x-Pt1x))^2
  #browser()
  xxx<-dist>0 & !is.na(dist)
  dist[xxx]<- dist[xxx]^0.5
  
  #dist[dist>0]<- dist[dist>0]^0.5
  return(dist)
}

LWRegression <- function(species){
  
  #carry out a length-weight regression for the species supplied
  #returns a 2-element numeric
  
  l<-c(); #lengths
  w<-c(); #weights
  
  for (i in 1:length(Hauls)){
    ret <- getLW(Hauls[[i]],species);
    l <- c(l,ret$len);
    w <- c(w,ret$wgt);
  }
  
  if (length(l)>0) {
    
    fit <- lm(log(w)~log(l));
  
    return(list("a" = exp(fit$coef[1]),
                "b" = fit$coef[2],
                "df" = summary(fit)$df[2],
                "rsq" = summary(fit)$adj.r.squared,
                "len" = as.vector(l),
                "wgt" = as.vector(w)));
  } 
  
  return(list());
  
}

makeKey <- function(len,len.range,len.int,dat,dat.range,dat.int,propagate=T){
  
  #make a key
  #len - vector of lengths
  #len.range - the range for the length dimension of the key
  #len.int - the bin interval for the length dimension of the key
  #dat - vector of data (ages or maturities)
  #dat.range - the range for the data dimension of the key
  #dat.int - the bin interval for the data dimension of the key
  #propagate - propagate the key out to the limits from the nearest data point?
  
  #data vectors must be of the same length
  if (length(len) != length(dat)) stop("Length and data vectors must be of the same length\n");
  
  #construct the length vector, and issue a warning if any of the lengths are outwith the specified range for the key
  keyrow <- seq(from = len.range[1],to = len.range[2],by = len.int);
  
  if (any(len<len.range[1] | len>len.range[2])){
    cat("Some lengths are outside the specified range for the key\n");
  }

  if (sum(!(len %in% keyrow))>0){
    #there are lengths in the dataset that do not match the rows of the key
    cat("Some lengths do not match the key rows\n")
  }
    
  #construct the data vector, and issue a warning if any of the data points are outwith the specified range for the key
  keycol <- seq(from = dat.range[1],to = dat.range[2],by = dat.int);
  
  if (any(dat<dat.range[1] | dat>dat.range[2])){
    cat("Some data points are outside the specified range for the key\n");
  }

  
  #construct the matrix that will hold the key
  key <- matrix(0,nrow=length(keyrow),ncol=length(keycol),
                dimnames=list(as.character(keyrow),as.character(keycol)));
    
  for (i in 1:length(keycol)) {
        
    df<-table(len[dat==keycol[i]]);
    
    if (sum(df)>0) {
      key[match(rownames(df),rownames(key),0),as.character(keycol[i])]<-df;
    }
    
  }
  
  #sum across the length class...
  ssum <- apply(key,1,sum);
  
  #and normalise
  key[ssum>0,] <- sweep(key[ssum>0,],1,ssum[ssum>0],"/");
  
  #propagate into the empty rows if required
  if(propagate){
    
    #first non zero row
    first<-match(rownames(key)[rowSums(key)>0][1],rownames(key),0);
    #last
    last<-match(rev(rownames(key)[rowSums(key)>0])[1],rownames(key),0);

    #if (first > 1) {
    #  for (i in 1:(first-1)) {
    #    key[i,]<-key[first,];
    #  }
    #}
         
    #if (last < nrow(key)) {
    #  for (i in (last+1):nrow(key)){
    #    key[i,]<-key[last,];
    #  }
    #}
    
    #now for empty rows in the middle of the key
    #empty rows
    #empty.rows <- c(1:nrow(key))[apply(key,1,sum)==0];
    #non.empty.rows <- c(1:nrow(key))[apply(key,1,sum)>0];
    
    #for (i in empty.rows){
    #  key[i,]<-key[non.empty.rows[sort.list(abs(non.empty.rows-i))[1]],];
    #}
           
  }
       
  return(key);
  
}


displayKey <- function(key,round=2){
  #display the supplied key
  #with the precision supplied
  
  #round the data
  key<-round(key,round);
  #substitute - for zeros for readability
  key[key==0] <- "-";
  #display the key by casting it to a data frame
  data.frame(key);
  
}

applyKey <- function(data,key,length.offset=0.25) {
  #apply the supplied key to the data
  #data is a vector at length
  
  #apply the length offset to the rownames of the key
  rownames(key) = as.character(as.numeric(rownames(key)) + length.offset);
  
  #TO DO
  #key should contain a row with data that sums to 1 for each of the column names of the data
  
  #perform matrix multiplication to apply key (using only rows of key that match data column names)
  ret<-data%*%as.matrix(key[match(names(data),rownames(key)),])
  
  ret <- ret/sum(ret);
  ret <- ret*sum(data);
  
  return(ret[1,]);
  
}


writeData <- function(dat,parent,type){
  #output the data, parent is the directory (Results folder),type is either "transect" or "stratum"

  #file names
  #abundance at age
  f.abd.by.age <- paste("AbdByAgeBy",type,".dat",sep="");
  #abundance at length
  f.abd.by.len <- paste("AbdByLenBy",type,".dat",sep="");
  #abundance at maturity code
  f.abd.by.mat.code <- paste("AbdByMatCodeBy",type,".dat",sep="");
  #abundance at maturity stage
  f.abd.by.mat.stage <- paste("AbdByMatStageBy",type,".dat",sep="");
  #biomass at age
  f.bio.by.age <- paste("BioByAgeBy",type,".dat",sep="");
  #biomass at length
  f.bio.by.len <- paste("BioByLenBy",type,".dat",sep="");
  #biomass at maturity code
  f.bio.by.mat.code <- paste("BioByMatCodeBy",type,".dat",sep="");
  #biomass at maturity stage
  f.bio.by.mat.stage <- paste("BioByMatStageBy",type,".dat",sep="");
  
  #file headers
  h.age <- type;
  h.len <- type;
  h.mat.stage <- type;
  h.mat.code <- type;
  
  #ages,lengths,matcodes,matstages
  ages<-c()
  lens<-c()
  matcodes<-c()
  matstages<-c()
  
  #loop over species
  for (spe in seq(length=length(dat[[1]]))){  
    spe_name <- names(dat[[1]])[spe];
    #loop over mark types
    for (mt in seq(length=length(dat[[1]][[spe_name]]))){
      mt_name <- names(dat[[1]][[spe_name]])[mt];
      #loop over transects to get the ranges for each of the output
      for (tr in seq(length(dat))){
        
        ages<-unique(c(ages,
                names(dat[[tr]][[spe]][[mt]][['Abd at Age']]),
                names(dat[[tr]][[spe]][[mt]][['Abd at Age']])));

        lens<-unique(c(lens,
                names(dat[[tr]][[spe]][[mt]][['Abd at Len']]),
                names(dat[[tr]][[spe]][[mt]][['Abd at Len']])));
        
        matcodes<-unique(c(matcodes,
                    names(dat[[tr]][[spe]][[mt]][['Abd at MatCode']]),
                    names(dat[[tr]][[spe]][[mt]][['Abd at MatCode']])));
        
        matstages<-unique(c(matstages,
                    names(dat[[tr]][[spe]][[mt]][['Abd at MatStage']]),
                    names(dat[[tr]][[spe]][[mt]][['Abd at MatStage']])));
        
      }
    }
  }

  #sort the data
  ages <- sort(as.numeric(ages));
  lens <- sort(as.numeric(lens));
  matcodes <- sort(as.numeric(matcodes));
  matstages <- sort(matstages);
  
  #construct the headers
  h.age <- c(h.age, as.character(ages));
  h.len <- c(h.len, as.character(lens));
  h.mat.stage <- c(h.mat.stage, matstages);
  h.mat.code <- c(h.mat.code, as.character(matcodes));

  #loop over species
  for (spe in seq(length=length(dat[[1]]))){  
    spe_name <- names(dat[[1]])[spe];
    #loop over mark types
    for (mt in seq(length=length(dat[[1]][[spe_name]]))){
      mt_name <- names(dat[[1]][[spe_name]])[mt];
      
      #create op folder
      out.dir<-paste(parent,spe_name,"/",mt_name,"/",sep="");
      dir.create(out.dir,recursive=TRUE,showWarnings=FALSE);
      
      #write headers
      write(h.age,ncolumns=length(h.age),file=paste(out.dir,f.abd.by.age,sep=""),sep="\t");
      write(h.age,ncolumns=length(h.age),file=paste(out.dir,f.bio.by.age,sep=""),sep="\t");
      write(h.len,ncolumns=length(h.len),file=paste(out.dir,f.abd.by.len,sep=""),sep="\t");
      write(h.len,ncolumns=length(h.len),file=paste(out.dir,f.bio.by.len,sep=""),sep="\t");
      write(h.mat.stage,ncolumns=length(h.mat.stage),file=paste(out.dir,f.abd.by.mat.stage,sep=""),sep="\t");
      write(h.mat.stage,ncolumns=length(h.mat.stage),file=paste(out.dir,f.bio.by.mat.stage,sep=""),sep="\t");
      write(h.mat.code,ncolumns=length(h.mat.code),file=paste(out.dir,f.abd.by.mat.code,sep=""),sep="\t");
      write(h.mat.code,ncolumns=length(h.mat.code),file=paste(out.dir,f.bio.by.mat.code,sep=""),sep="\t");
      
      #write data
      for (o in seq(length=length(dat))){
        write(c(names(dat)[o],as.vector(dat[[o]][[spe]][[mt]][['Abd at Age']])),
              ncolumns=length(h.age),
              file=paste(out.dir,f.abd.by.age,sep=""),
              append=TRUE,sep="\t");
        write(c(names(dat)[o],as.vector(dat[[o]][[spe]][[mt]][['Bio at Age']])),
              ncolumns=length(h.age),
              file=paste(out.dir,f.bio.by.age,sep=""),
              append=TRUE,sep="\t");
        write(c(names(dat)[o],as.vector(dat[[o]][[spe]][[mt]][['Abd at Len']])),
              ncolumns=length(h.len),
              file=paste(out.dir,f.abd.by.len,sep=""),
              append=TRUE,sep="\t");
        write(c(names(dat)[o],as.vector(dat[[o]][[spe]][[mt]][['Bio at Len']])),
              ncolumns=length(h.len),
              file=paste(out.dir,f.bio.by.len,sep=""),
              append=TRUE,sep="\t");
        write(c(names(dat)[o],as.vector(dat[[o]][[spe]][[mt]][['Abd at MatStage']])),
              ncolumns=length(h.mat.stage),
              file=paste(out.dir,f.abd.by.mat.stage,sep=""),
              append=TRUE,sep="\t");
        write(c(names(dat)[o],as.vector(dat[[o]][[spe]][[mt]][['Bio at MatStage']])),
              ncolumns=length(h.mat.stage),
              file=paste(out.dir,f.bio.by.mat.stage,sep=""),
              append=TRUE,sep="\t");
        write(c(names(dat)[o],as.vector(dat[[o]][[spe]][[mt]][['Abd at MatCode']])),
              ncolumns=length(h.mat.code),
              file=paste(out.dir,f.abd.by.mat.code,sep=""),
              append=TRUE,sep="\t");
        write(c(names(dat)[o],as.vector(dat[[o]][[spe]][[mt]][['Bio at MatCode']])),
              ncolumns=length(h.mat.code),
              file=paste(out.dir,f.bio.by.mat.code,sep=""),
              append=TRUE,sep="\t");        
      }
      
    }
    #all mark types
    #create op folder
    out.dir<-paste(parent,spe_name,"/","All","/",sep="");
    dir.create(out.dir,recursive=TRUE,showWarnings=FALSE);
    
    #write headers
    write(h.age,ncolumns=length(h.age),file=paste(out.dir,f.abd.by.age,sep=""),sep="\t");
    write(h.age,ncolumns=length(h.age),file=paste(out.dir,f.bio.by.age,sep=""),sep="\t");
    write(h.len,ncolumns=length(h.len),file=paste(out.dir,f.abd.by.len,sep=""),sep="\t");
    write(h.len,ncolumns=length(h.len),file=paste(out.dir,f.bio.by.len,sep=""),sep="\t");
    write(h.mat.stage,ncolumns=length(h.mat.stage),file=paste(out.dir,f.abd.by.mat.stage,sep=""),sep="\t");
    write(h.mat.stage,ncolumns=length(h.mat.stage),file=paste(out.dir,f.bio.by.mat.stage,sep=""),sep="\t");
    write(h.mat.code,ncolumns=length(h.mat.code),file=paste(out.dir,f.abd.by.mat.code,sep=""),sep="\t");
    write(h.mat.code,ncolumns=length(h.mat.code),file=paste(out.dir,f.bio.by.mat.code,sep=""),sep="\t");
    
  }
  
}

writeOPLine <- function(x,file,append=TRUE){
  write(x,file,append=append)
}

formatOPLine <- function(x,ages,numfmt,subzero,tex=FALSE,total=TRUE,colCount,skipb4Total=0){
  #format a line of output based on the required ages
  #if tex = TRUE the line is for inclusion in a TEX table,
  #otherwise it's a tab separated string
  #if total is TRUE a total is appended
  #if subzero is supplied, replace any zero values (not totals) with this
  #if a colCount is supplied, pad the line out to this number of columns, using the subzero symbol if provided
  #if a non zero skipb4Total value is supplied, skip this number of columns before inserting the total
  #(only tested when total=TRUE)
  
  ret <- ""
  tot <- 0
  
  for (a in ages){
    if (!is.null(x[as.character(a)])) {
      if (!is.na(x[as.character(a)])) {
        tot <- tot + x[as.character(a)]
        if ((x[as.character(a)]==0) & (!missing(subzero))) {
          ret <- paste(ret,subzero,sep=",")
        } else {
          ret <- paste(ret,sprintf(numfmt,x[as.character(a)]),sep=",")
        }
      } else {
        if (!missing(subzero)) {
          ret <- paste(ret,subzero,sep=",")
        } else {
          ret <- paste(ret,sprintf(numfmt,0),sep=",")
        }
      }
    } else {
      if (!missing(subzero)) {
        ret <- paste(ret,subzero,sep=",")
      } else {
        ret <- paste(ret,sprintf(numfmt,0),sep=",")
      }
    }
  }
  
  #add total
  if (total){
    if (skipb4Total>0){
      ret <- paste(ret,paste(rep("-",skipb4Total),collapse=","),sprintf(numfmt,tot),sep=",")
    } else {
      ret <- paste(ret,sprintf(numfmt,tot),sep=",")
    }
  } else {
    ret <- paste(ret,"-",sep=",")
  }
  
  #check column count, if supplied
  if (!missing(colCount)) {
    if (colCount>length(unlist(strsplit(ret,",")))) {
      if (!missing(subzero)){
        ret <- paste(ret,paste(rep(subzero,colCount-length(unlist(strsplit(ret,",")))),collapse=","),sep=",")
      } else {
        ret <- paste(ret,paste(rep("",colCount-length(unlist(strsplit(ret,",")))),collapse=","),sep=",")
      }
    }
  }
  
  ret
  
}

combineLFs <- function(lIn){
  
  #combine a number of length frequencies
  #input is a list
  
  #remove any NULLs
  t<-Filter(Negate(is.null),lIn)
  
  #vector of lengths
  len <-  sort(unique(unlist(lapply(t,names))))
  
  #cat(len,"\n")
  
  #create return object
  ret <- vector("numeric",length=length(len))
  ret <- rep(0,length(len))
  names(ret) <- len

  #loop over LFs, accumulating the summed values in ret
  for (i in 1:length(t)){
    
    LF <- t[[i]]
    
    #cat(LF,"\n")
    
    for (j in 1:length(LF)){
      ret[[names(LF)[j]]] <- ret[[names(LF)[j]]] + LF[j]
    }
  } 
  
  #adjust lengths by 0.25
  unadjusted <- as.numeric(names(ret))
  adjusted <- unadjusted - 0.25
  
  names(ret) <- adjusted
  
  ret
  
}

aggregateSA <- function(SA,MarkTypes) {
  
  #SA - the raw SA data
  
  #calculate the mid position of mark
  SA$Mid.Lat <- (SA$Lat_S + SA$Lat_E)/2;
  SA$Mid.Lon <- (SA$Lon_S + SA$Lon_E)/2;
  
  #calculate the school length
  if(nrow(SA)>0) {
    
    for (i in 1:nrow(SA)) {
      
      #Ian Doonan's distance function
      SA$SchoolLength[i] <- ToKm(SA$Lat_S[i],SA$Lon_S[i],SA$Lat_E[i],SA$Lon_E[i]);
      
      #SA$SchoolLength[i] <- distVincentyEllipsoid(c(SA$Lon_S[i],SA$Lat_S[i]),
      #                                            c(SA$Lon_E[i],SA$Lat_E[i]))/1000;
      
      SA$Tx[i] <- 1;
    }
  }
  
  #unique mark types
  MTs <- unique(SA$Region_class);
  
  #return data
  ret <- c();
  
  #loop over mark types
  for (j in 1:(length(levels(MTs)))) {
    
    #subset for this mark type
    SA_MT <- subset(SA,SA$Region_class == levels(MTs)[j]);
    
    #assemble a data frame... (converting school length to nmi)
    dfSA <- data.frame(Strata = SA_MT$Strata,
                       Transect = SA_MT$Transect,
                       Time = SA_MT$time_S,
                       Date = SA_MT$Date_E,
                       SA = SA_MT$PRC_NASC,
                       Lat = SA_MT$Mid.Lat,
                       Lon = SA_MT$Mid.Lon,
                       School.Length = SA_MT$SchoolLength/1.852);
    
    #and aggregate SA values for the same schools
    dfSA.agg <- aggregate(dfSA$SA,by=list(dfSA$Strata,
                                          dfSA$Transect,
                                          dfSA$Time,
                                          dfSA$Date,
                                          dfSA$Lat,
                                          dfSA$Lon,
                                          dfSA$School.Length),FUN=sum);
    
    #assign names
    names(dfSA.agg)<-c("Strata","Transect","Time","Date","Lat","Lon","School.Length","SA");
    
    #preallocate a list of length nrow(dfSA.agg)
    lSA <- vector("list",nrow(dfSA.agg));
    
    for (i in 1:nrow(dfSA.agg)){
      
      lSA[[i]] <- mark(cruise_code = getCode(Cruise),
                       stratum_code = as.character(dfSA.agg$Strata[i]),
                       transect_code = as.character(dfSA.agg$Transect[i]),
                       school_length = dfSA.agg$School.Length[i],
                       cell_length = 1,
                       NASC = dfSA.agg$SA[i],
                       position = waypoint(lat = dfSA.agg$Lat[i],
                                           lon = dfSA.agg$Lon[i],
                                           time = as.POSIXlt(strptime(paste(dfSA.agg$Date[i],dfSA.agg$Time[i],sep=" "),"%Y%m%d %H:%M:%OS"))),
                       marktype_name = levels(MTs)[j])
      
    }
    
    #append to return object
    ret <- c(ret,lSA);
    
    
  }

  #return only marks with matching marktypes
  #ret[which(lapply(ret,getMarkType)%in%unlist(lapply(MarkTypes,getNASCName)))]
  ret[which(toupper(lapply(ret,getMarkType))%in%unlist(toupper(lapply(MarkTypes,getNASCName))))]
  
}
