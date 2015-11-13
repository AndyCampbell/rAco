#Andy Campbell, Marine Inst, Galway
#15/11/2012 V0.1
#11/07/2013 V0.2 NWHAS2013 improvements
#04/07/2014 V0.4 NWHAS2014 improvements
#10/11/2014 test with CSHAS2013
#12/02/2014 moved into github
#13/11/2015 CSHAS2015

#BEFORE proceeding...
#ensure the following files are available (in repository)
#******.RData (created using DB2RData.r)
#******_Cruise.dat
#******_CTD.csv
#******_Strata.csv
#******_Transect.csv
#******_Species.dat

#clear memory
rm(list=ls())
gc()

#load in libraries, function definitions, plotting data
source("./Scripts/Init.r")

#load Cruise data file

Cruise <- loadCruise(CruiseName = "CSHAS2015", SpeciesName = "Herring")
#Cruise <- loadCruise(CruiseName = "BWAS2015", SpeciesName = "Blue Whiting")
#Cruise <- loadCruise(CruiseName = "NWHAS2013", SpeciesName = "Herring")
#Cruise <- loadCruise(CruiseName = "NWHAS2013", SpeciesName = "Boarfish")
#Cruise <- loadCruise(CruiseName = "NWHAS2014", SpeciesName = "Herring")
#Cruise <- loadCruise(CruiseName = "NWHAS2014", SpeciesName = "Boarfish")
#Cruise <- loadCruise(CruiseName = "NWHAS2014", SpeciesName = "Sprat")
#Cruise <- loadCruise(CruiseName = "CSHAS2013", SpeciesName = "Herring")
#Cruise <- loadCruise(CruiseName = "CSHAS2013", SpeciesName = "Sprat")
#Cruise <- loadCruise(CruiseName = "NWHAS2015", SpeciesName = "Herring")
#Cruise <- loadCruise(CruiseName = "COM01_2011", SpeciesName = "Boarfish")   #boarfish 2011, excluding midnight-4am
#Cruise <- loadCruise(CruiseName = "COM02_2011", SpeciesName = "Boarfish")   #boarfish 2011, 24hrs



#print cruise summary to the screen
summary(Cruise)

#Access DB data (already saved in .RData file using 32-bit R)
#see script DB2RData.r in this project
#loads initial data for Hauls, Samples, LF, Ags, SA
load(paste("./Data/",getName(Cruise),"/",getName(Cruise),".RData",sep=""))


#load vessel track if it's available
if (file.exists(paste("./RData/Track_", getName(Cruise), ".rda", sep = ""))) {
  load(file = paste("./RData/Track_", getName(Cruise), ".rda", sep = ""))
  cat("Track data read in\n")
}

#CTD data
#If object DBCTDs exists then data has been read from table FSS_CTD from the access DB
#otherwise, assume that the csv has been supplied
CTDs <- fLoad_CTDs(getCode(Cruise), getName(Cruise),
                   DBCTDs = {if (exists("DBCTDs")){DBCTDs} else {NULL}})
cat(length(CTDs), "CTDs read in\n")

#Transect data.
#If object DBtransects exists then they have been read in from table FSS_Transect from the DB
#otherwise, assume the csv has been supplied
Transects <- fLoad_Transects(getCode(Cruise), getName(Cruise),
                             DBTransects = {if (exists("DBTransects")){DBTransects} else {NULL}})
cat(length(Transects), "transects read in\n")

#Strata data
Strata <- fLoad_Strata(getCode(Cruise), getName(Cruise),
                       DBStrata = {if (exists("DBStrata")){DBStrata} else {NULL}})
cat(length(Strata), "strata read in\n")

#Species Details (target strength, maturity codes)
Species <- fLoad_Species_Details(getCode(Cruise))
cat(length(Species), "species read in\n")

#Mark Types
MarkTypes <- fLoad_Mark_Types(CruiseName = getName(Cruise), SpeciesName = getTargetCommon(Cruise))
cat(length(MarkTypes), "mark types read in\n")

#report of the region classes in the SA data
if (exists("SA")) {table(SA$Region_class)}

#format Haul data
Hauls <- fLoad_Hauls(getCode(Cruise))
cat(length(Hauls), "hauls read in\n")

#process SA data for the appropriate mark types
SA <- fAggregate_SA(SA = {if (exists("SA")){SA}else{NULL}}, MarkTypes = MarkTypes)

#check this matches with mark types listed above. Missing mark types may indicate that the NASC_name of 
#the MarkTypes needs updating.
#Numbers may not match since data has been aggregated.
table(unlist(lapply(SA,getMarkType)))

#create plots directory
plots.dir <- paste(getwd(), "/Plots/", getName(Cruise), "/", getTargetCommon(Cruise), "/", sep="")

#create the output directory, suppress the showWarnings so that no warning
#is issued if the folder already exists
dir.create(plots.dir, recursive=TRUE, showWarnings=FALSE);

#generate some survey level plots
source("./Scripts/Plots.r")

#Only proceed past here when strata plots have been examined
#to check transects and strata bounds

#assign a transect to each registration on the basis of its proximity
#will be checked against transect assigned previously when data was
#uploaded to the DB
SA<-lapply(SA,assignTransect,tran=Transects)

#create QC plots directory
qcplots.dir<-paste(getwd(),"/QCPlots/",getName(Cruise),"/",getTargetCommon(Cruise),"/",sep="");
#create the output directory, suppress the showWarnings so that no warning
#is issued if the folder already exists
dir.create(qcplots.dir,recursive=TRUE,showWarnings=FALSE);

#loop over marks, report any that are closer to an alternative transect
#to that stored in the database, generate plot containing transects in question
#and the location of the mark
for (i in 1:length(SA)){

  if (SA[[i]]@transect_code!=SA[[i]]@nearest_transect) {
    
    #if transects are in the same strata, just plot the strata
    #with the transects and mark.
    cat("check mark index",i,"of type",SA[[i]]@marktype_name,"\n")
    cat("Assigned transect_code",SA[[i]]@transect_code,"in stratum",
        getStratumCode(Transects[[which(lapply(Transects,getCode)==SA[[i]]@transect_code)]]),
        "but closer to",SA[[i]]@nearest_transect,"in stratum",
        getStratumCode(Transects[[which(lapply(Transects,getCode)==SA[[i]]@nearest_transect)]]),"\n")
    
    
    
    #on transect 
    on.transect<-which(lapply(Transects,getCode)==SA[[i]]@transect_code)
    #nearest transect
    near.transect<-which(lapply(Transects,getCode)==SA[[i]]@nearest_transect)
    
    #find the strata to plot from the transects assigned to the mark
    in.strata<-which(lapply(Strata,getCode)==Transects[[on.transect]]@stratum_code)
    
     plot(Cruise,
          strata=c(Strata[[in.strata]]),
          transects=c(Transects[[on.transect]],Transects[[near.transect]]),
          sa=c(SA[[i]]),
          filename=paste(qcplots.dir,"//SA",as.character(i),".png",sep=""));
    
    }
}

#Check QC plots before proceeding 


#length-weight analysis for each species
#loop over species, setting the LW parameters from a regression analysis
for (i in seq(length=length(Species))){
  #LW regression
  Species[[i]] <- setLW(Species[[i]],LWRegression(getName(Species[[i]])));
  plot(Species[[i]],
       filename=paste("./Plots/",getName(Cruise),"/",getTargetCommon(Cruise),"/LW_",Species[[i]]@species,".png",sep=""));
}

#Create and initialise structures to store the results of the analysis
#and create the folders
#subset species to get those to be estimated for abundance
#est.Species<-Species[lapply(Species,getEstAbd)==TRUE]
est.Species <- Species[which(toupper(unlist(lapply(Species,getTargetCommon)))==toupper(getTargetCommon(Cruise)))]

#results data structure
out <- vector("list",length(est.Species));

#stratum results
res.by.stratum <- vector("list",length(Strata));
names(res.by.stratum) <- unlist(lapply(Strata,getCode));

#transect results
res.by.transect <- vector("list",length(Transects));
names(res.by.transect) <- unlist(lapply(Transects,getCode));

#initialise output lists for each species
for(ii in 1:length(res.by.stratum)){
  res.by.stratum[[names(res.by.stratum)[ii]]]<-vector("list",length(est.Species))
}
for(ii in 1:length(res.by.transect)){
  res.by.transect[[names(res.by.transect)[ii]]]<-vector("list",length(est.Species))
}

#create results directory
results.dir<-paste(getwd(),"/Results/",getName(Cruise),"/",sep="");
#create the output directory, suppress the showWarnings so that no warning
#is issued if the folder already exists
dir.create(results.dir,recursive=TRUE,showWarnings=FALSE);

#loop over species to process - should only be a single species
for (spe in seq(length=length(est.Species))){
  
  #species name
  spe_name <- getName(est.Species[[spe]]);
  spe_commonname <- getTargetCommon(est.Species[[spe]]);

  #create results directory for this species
  spe.dir<-paste(results.dir,spe_commonname,"/",sep="");
  #this call doesn't need recursive=TRUE as the parent directory should already exist
  dir.create(spe.dir,recursive=FALSE,showWarnings=FALSE);
  
  #create an object for the results for this species
  names(out)[spe] <- spe_name;

  #assign species name for each results list
  for(ii in 1:length(res.by.stratum)){
    names(res.by.stratum[[names(res.by.stratum)[ii]]])[spe]<-spe_name;
  }
  for(ii in 1:length(res.by.transect)){
    names(res.by.transect[[names(res.by.transect)[ii]]])[spe]<-spe_name;
  }
  
  cat("Processing Species: ",spe_name,"\n",sep="");
  
  #create ALK if required
  if (getEstByAge(est.Species[[spe]])) {
    
    #check if ALK is to be read in from separate csv 

    if (getName(Cruise) %in% c('BFAS2013','BWAS2015','COM01_2011')) {
      
      cat("reading in ALK\n")
      
      #read ALK from file
      ALK <- read.table(paste0("./DATA/",getName(Cruise),"/ALK.csv"),T,skip=1,row.names=1,sep=",",fill=T)
      colnames(ALK)<-as.character(seq(1,15))
      
    } else {

      cat("generating ALK\n")
      
      #range of length classes in LF data
      LF.Range <- range(unlist(lapply(Hauls,getLFRange,species=spe_name)),na.rm=TRUE);
      
      #TO DO - find a way to eliminate this loop below
      ll<-c();
      aa<-c();
      
      for (i in 1:length(Hauls)){
        la <- getLA(Hauls[[i]],species=spe_name);
        ll<-c(ll,la$len);
        aa<-c(aa,la$age);
      }
      
      ALK <- makeKey(len = ll,
                     len.range = c(floor(range(ll,LF.Range)[1]),ceiling(range(ll,LF.Range)[2])),
                     len.int = 0.5,
                     dat = aa,
                     dat.range = range(aa),
                     dat.int = 1,
                     propagate = T);
      }
    
  } else {ALK <- NULL}
  
  #create MLK if required
  if (getEstByMat(est.Species[[spe]])) {

    #check if MLK is to be read in from separate csv 
    
    if (getName(Cruise) %in% c('BFAS2013','BWAS2015','COM01_2011')) {
      
      #read MLK from file
      MLK <- read.table(paste0("./DATA/",getName(Cruise),"/MLK.csv"),T,skip=1,row.names=1,sep=",",fill=T)
      colnames(MLK) <- c("Immature","Mature","Spent")
      #colnames(MLK) <- c(as.character(seq(1,8)))
      
    } else {
      
      ll<-c();
      mm<-c();
      
      for (i in 1:length(Hauls)){
        lm <- getLM(Hauls[[i]],species=spe_name);
        ll<-c(ll,lm$len);
        mm<-c(mm,lm$mat);
      }
      
      MLK <- makeKey(len = ll,
                     len.range = c(floor(range(ll,LF.Range)[1]),ceiling(range(ll,LF.Range)[2])),
                     len.int = 0.5,
                     dat = mm,
                     dat.range = range(mm),
                     dat.int = 1,
                     propagate = T);

    }
    
    #displayKey(MLK);

    #retrieve maturity codes and stages
    mat_codes <- c(getImmatureCodes(est.Species[[spe]]),
                   getMatureCodes(est.Species[[spe]]),
                   getSpentCodes(est.Species[[spe]]));

    names(mat_codes)<-as.character(mat_codes);
    
    mat_stages <- list("Immature" = getImmatureCodes(est.Species[[spe]]),
                       "Mature" = getMatureCodes(est.Species[[spe]]),
                       "Spent" = getSpentCodes(est.Species[[spe]]));    
    
  } else {MLK <- NULL}
  
  
   #subset marktypes to process for this species
   spe.MarkTypes <- MarkTypes[toupper(lapply(MarkTypes,getSpecies))==toupper(getName(est.Species[[spe]]))] 
 
   #create results directory for all marktypes to be included in the estimate
   #mt.dir<-paste(spe.dir,"All","/",sep="");
   #dir.create(mt.dir,recursive=TRUE,showWarnings=FALSE);
   
   #loop over the marktypes defined for this species
   for (mt in seq(length=length(spe.MarkTypes))){
 
    mt_name <- getNASCName(spe.MarkTypes[[mt]]);

    #create results directory for this marktype
    #mt.dir<-paste(spe.dir,mt_name,"/",sep="");
    #dir.create(mt.dir,recursive=TRUE,showWarnings=FALSE);
        
     #SA data
     mtSA <- SA[toupper(lapply(SA,getMarkType))==toupper(getNASCName(spe.MarkTypes[[mt]]))];
     
     cat("Processing Mark Type: ",getName(spe.MarkTypes[[mt]])," (",getNASCName(spe.MarkTypes[[mt]]),") with ",length(mtSA)," marks\n",sep="");
     
     #for each acoustic mark assign the appropriate haul, length frequency and acoustic 
     #cross section
     for (i in seq(length=length(mtSA))){
       
       #assign the appropriate Haul to each mark
       mtSA[[i]]<-setHaulCode(mtSA[[i]],assignHaul(spe.MarkTypes[[mt]],pos=getPosition(mtSA[[i]])));
       
       #assign the LF to the mark
       mtSA[[i]]<-setLF(mtSA[[i]],getLFProp(haul.code=getHaulCode(mtSA[[i]]),
                                            species=getName(est.Species[[spe]]),
                                            mix.species=getMixedSpecies(spe.MarkTypes[[mt]])))
       
       #calculate the acoustic backscatter cross-section
       mtSA[[i]]<-setCS(mtSA[[i]],Species,spe_name);
     }
    
    #loop over transects, select SA records and calculate transect mean
    cat("Looping over transects\n")
    for (tr in lapply(Transects,getCode)){      
        
      #transect name
      tr_name <- getCode(Transects[[tr]]);
      
      #marks for current transect
      #mk<-mtSA[lapply(mtSA,getTransectCode)==getCode(Transects[[tr]])];
      mk <- mtSA[lapply(mtSA,getTransectCode)==tr];

      if (length(mk)>0){

         #get abundances,biomass for these marks
         abd <- unlist(lapply(mk, abundance, target = spe_name))
         bio <- unlist(lapply(mk, biomass, LW = getLW(est.Species[[spe]]), target=spe_name))
         
         #and cell length
         cell <- unlist(lapply(mk,getCellLength))   
         cells <- unlist(lapply(mk,getCellLengths))
         
         mean_abd <- tapply(abd*cells,names(abd),sum)/getTrackLength_nm(Transects[[tr]])
         mean_bio <- tapply(bio*cells,names(abd),sum)/getTrackLength_nm(Transects[[tr]])
         
         mn_abd <- as.numeric(mean_abd)
         names(mn_abd) <- names(mean_abd)
         mn_bio <- as.numeric(mean_bio)
         names(mn_bio) <- names(mean_bio)
         
         Transects[[tr]] <- setNumMarks(Transects[[tr]], getName(spe.MarkTypes[[mt]]), length(mk))
         Transects[[tr]] <- setAbdAtLen(Transects[[tr]], getName(spe.MarkTypes[[mt]]), mn_abd)
         Transects[[tr]] <- setBioAtLen(Transects[[tr]], getName(spe.MarkTypes[[mt]]), mn_bio)
         
          if (!is.null(ALK)){
            Transects[[tr]]<-setAbdAtAge(Transects[[tr]],getName(spe.MarkTypes[[mt]]),applyKey(mn_abd[mn_bio>0],ALK));
            Transects[[tr]]<-setBioAtAge(Transects[[tr]],getName(spe.MarkTypes[[mt]]),applyKey(mn_bio[mn_bio>0],ALK));
          }
          if (!is.null(MLK)){
            Transects[[tr]]<-setAbdAtMat(Transects[[tr]],getName(spe.MarkTypes[[mt]]),applyKey(mn_abd[mn_abd>0],MLK));
            Transects[[tr]]<-setBioAtMat(Transects[[tr]],getName(spe.MarkTypes[[mt]]),applyKey(mn_bio[mn_bio>0],MLK));
          }
      }
      
    }#end loop over transects

    
    #Loop over strata, select the appropriate transects and calculate the stratum 
    #results
    cat("Looping over strata\n")
    for (i in seq(length=length(Strata))){
      
      #stratum name
      str_name <- getCode(Strata[[i]]);
      
      #retrieve list of transects in this stratum
      tr <- Transects[lapply(Transects,getStratumCode)==str_name];
      
      if (length(tr)>0) {

        #mean abundances/biomass by transect
        labd<-lapply(tr,getMeanAbundance,marktype=getName(spe.MarkTypes[[mt]]))
        lbio<-lapply(tr,getMeanBiomass,marktype=getName(spe.MarkTypes[[mt]]))
        #variances
        vabd<-var(as.vector(unlist(labd)))
        vbio<-var(as.vector(unlist(lbio)))

        #weighting for variances
        #track lengths
        llen<-as.vector(unlist(lapply(tr,getTrackLength_nm)))
        wVar <- sum(llen^2)/sum(llen)^2         
                
        #calculate mean abundance for the stratum
        #this is already done in the setAbdByLen method - but just want to compare
        new_abd <- sum(as.vector(unlist(labd))*as.vector(unlist(llen)))/sum(as.vector(unlist(llen)))     
        new_bio <- sum(as.vector(unlist(lbio))*as.vector(unlist(llen)))/sum(as.vector(unlist(llen)))     
        
        
        #record number of transects
        #res.by.stratum[[str_name]][[spe_name]][[mt_name]][["Num Transects"]]<-length(tr);        
        
        #get abundances,biomass for these transects
        #abd<-unlist(lapply(tr,getAbdAtLen,name=getName(spe.MarkTypes[[mt]])));
        #bio<-unlist(lapply(tr,getBioAtLen,name=getName(spe.MarkTypes[[mt]])));
        #abd<-lapply(tr,getAbdAtLen,name=getName(spe.MarkTypes[[mt]]));
        abd<-lapply(tr,getAbdAtLen,marktypes=getName(spe.MarkTypes[[mt]]));
        #bio<-lapply(tr,getBioAtLen,name=getName(spe.MarkTypes[[mt]]));
        bio<-lapply(tr,getBioAtLen,marktypes=getName(spe.MarkTypes[[mt]]));
        
        if (sum(unlist(abd))>0) {

           #and cell lengths
           #cells<-unlist(lapply(tr,getCellLengths,name=getName(spe.MarkTypes[[mt]])));
           cells<-lapply(tr,getCellLengths,name=getName(spe.MarkTypes[[mt]]));
           
           #combine the abundances to stratum level
           #multiply by cell length
           for(j in 1:length(abd)){
             abd[[j]]<-abd[[j]]*cells[[j]]
             bio[[j]]<-bio[[j]]*cells[[j]]
           }
           
           #unique length classes sorted
           lc<-sort(unique(as.numeric(unlist(lapply(abd,names)))))
           #vector to hold summed abundances
           sum.abd<-vector("numeric",length=length(lc))
           sum.bio<-vector("numeric",length=length(lc))
           names(sum.abd)<-lc
           names(sum.bio)<-lc
           for (j in 1:length(lc)){
             sum.abd[as.character(lc[j])]<-sum(unlist(lapply(abd,'[',as.character(lc[j]))),na.rm=TRUE)
             sum.bio[as.character(lc[j])]<-sum(unlist(lapply(bio,'[',as.character(lc[j]))),na.rm=TRUE)
           }

           mean_abd <- sum.abd/getTrackLength_nm(Strata[[i]]);
           mean_bio <- sum.bio/getTrackLength_nm(Strata[[i]]);

           Strata[[str_name]]<-setAbdAtLen(Strata[[str_name]],getName(spe.MarkTypes[[mt]]),mean_abd[mean_abd>0]);
           Strata[[str_name]]<-setBioAtLen(Strata[[str_name]],getName(spe.MarkTypes[[mt]]),mean_bio[mean_bio>0]);
           
           if (!is.null(ALK)) {
             Strata[[str_name]]<-setAbdAtAge(Strata[[str_name]],getName(spe.MarkTypes[[mt]]),applyKey(mean_abd[mean_abd>0],ALK));
             Strata[[str_name]]<-setBioAtAge(Strata[[str_name]],getName(spe.MarkTypes[[mt]]),applyKey(mean_bio[mean_bio>0],ALK));
           }

           if (!is.null(MLK)) {
             Strata[[str_name]]<-setAbdAtMat(Strata[[str_name]],getName(spe.MarkTypes[[mt]]),applyKey(mean_abd[mean_abd>0],MLK));       
             Strata[[str_name]]<-setBioAtMat(Strata[[str_name]],getName(spe.MarkTypes[[mt]]),applyKey(mean_bio[mean_bio>0],MLK));          
          }
          
        } #end sum(abd)>0 condition
      } #end length(tr)>0 condition
    } #end loop over strata    

#     #calculate survey totals
#     Survey.abun <- c();
#     Survey.bio <- c();
#     
#     for (i in 1:length(Strata)){
#       Survey.abun <- c(Survey.abun,getArea(Strata[[i]])*getAbdAtLen(Strata[[i]]));
#       Survey.bio <- c(Survey.bio,getArea(Strata[[i]])*getBioAtLen(Strata[[i]]));
#     }
#     
#     #remove zeros
#     Survey.abun <- Survey.abun[Survey.abun>0];
#     Survey.bio <- Survey.bio[Survey.bio>0];
#     
#     #group by length
#     Survey.abun <- tapply(Survey.abun,names(Survey.abun),sum);
#     Survey.bio <- tapply(Survey.bio,names(Survey.bio),sum);
#     
#     #totals
#     sum(Survey.abun);
#     sum(Survey.bio);    
#     
#     cat("Abun=",max(sum(Survey.abun),0),"\n");
   
  }#end loop over marktypes


  #calculate variances
  cat("Calculating variances\n")
  source("./Scripts/Variances.r");

  #save species results

  save(list=c("Cruise","Transects","Strata","spe.MarkTypes","MarkTypes","Species","ALK","MLK","CVatAge","CVAbdSur","CVBioSur","Hauls","Samples"),
       file=paste(spe.dir,spe_commonname,".RData",sep=""))

}#end loop species


