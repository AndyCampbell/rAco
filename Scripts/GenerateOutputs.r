#new output generation script

#clear memory, except for Cruise object (just to be sure everything is clean)
rm(list=setdiff(ls(),"Cruise"));
gc();

source("./Scripts/Init.r");

cruiseName <- getName(Cruise)
Estspecies <- getTargetCommon(Cruise)

#load in the processed data
load(paste("./Results/",cruiseName,"/",Estspecies,"/",Estspecies,".RData",sep=""))

#create a directory in which to save the tabular output
tables.dir<-paste(getwd(),"/Tables/",getName(Cruise),"/",sep="");
dir.create(tables.dir,recursive=TRUE,showWarnings=FALSE);
tables.dir<-paste(tables.dir,Estspecies,"/",sep="");
dir.create(tables.dir,recursive=FALSE,showWarnings=FALSE);
plots.dir<-paste(getwd(),"/Plots/",getName(Cruise),"/",getTargetCommon(Cruise),"/",sep="");

#the following are generated (not in all cases)

#Biomass at age by strata and ICES division (table1)
#Abundance at age by strata and ICES division (table2)
#Biomass at maturity by strata and ICES division (table3)
#Abundance at maturity by strata and ICES division (table4)
#Biomass and number of schools by strata and mark type (table5)
#Catch composition by haul
#Length at age as abundance and biomass


#ages to be reported
#rep.Ages <- seq(range(unlist(lapply(Strata,getAgeRange)))[1],range(unlist(lapply(Strata,getAgeRange)))[2],by=1)
#use the ALK
rep.Ages <- as.numeric(colnames(ALK))
#marktypes to be reported
rep.MTs <- names(lapply(spe.MarkTypes,getIncludeInEstimates))[unlist(lapply(spe.MarkTypes,getIncludeInEstimates))]

#table headers
headers<-c(paste("Strata",paste(rep.Ages,collapse=","),"Total",sep=","),
           paste("Strata",paste(rep.Ages,collapse=","),"Total",sep=","),
           paste("Strata","Immature","Mature","Spent","Total",sep=","),
           paste("Strata","Immature","Mature","Spent","Total",sep=","),
           paste("Stratum","No Tran","No Sch","Def Sch","Mix Sch","Prob Sch","% Zeros","Def Bio","Mix Bio","Prob Bio","Biomass (000's t)","SSB (000's t)","Abund (mills)",sep=","),
           paste("No.","Date","Time","Lat","Lon","Target Depth (m)","Bottom Depth (m)","Bulk Catch (kg)","Sampled Catch (kg)",sep=","))

if (getEstByAge(Species[[Estspecies]])) {
  headers <- c(headers,
               paste("Length (cm)","Age 0","(rings) 1",paste(seq(2,rep.Ages[length(rep.Ages)-1]),collapse=","),"Abund. (mills)","Biomass (kt)","Mn Wgt (g)",sep=","))
}       

#Abundance at age by strata Report
#only for aged species
#----------------------------------------------------------------------

if (getEstByAge(Species[[Estspecies]])) {

  AbdAtAge <- lapply(Strata,getAbdAtAge,marktypes=rep.MTs)
  #total
  AbdAtAge[['Total']] <- Reduce("+",Filter(Negate(is.null),AbdAtAge))
  #percentages
  AbdAtAge[['%']] <- 100.0*AbdAtAge[['Total']]/sum(AbdAtAge[['Total']])
  #CVs
  AbdAtAge[['CV']] <- CVatAge
  
  if (substring(cruiseName,1,4)=="BFAS") {
    #Boarfish summaries
    cat(" \n")
  } else if (substring(cruiseName,1,5)=="NWHAS") {
    if (Estspecies=='Herring') {
      #North West Herring summaries  
      #VIaN only
      VIaN <- Strata[lapply(Strata,getICESarea)=="VIaN"]
      VIaN.AbdAtAge <- lapply(VIaN,getAbdAtAge,marktypes=rep.MTs)
      AbdAtAge[['VIaN']] <- Reduce("+",Filter(Negate(is.null),VIaN.AbdAtAge))
      #VIaS only
      VIaS <- Strata[lapply(Strata,getICESarea)=="VIaS"]
      VIaS.AbdAtAge <- lapply(VIaS,getAbdAtAge,marktypes=rep.MTs)
      AbdAtAge[['VIaS']] <- Reduce("+",Filter(Negate(is.null),VIaS.AbdAtAge))
      #VIIb only
      VIIb <- Strata[lapply(Strata,getICESarea)=="VIIb"]
      VIIb.AbdAtAge <- lapply(VIIb,getAbdAtAge,marktypes=rep.MTs)
      AbdAtAge[['VIIb']] <- Reduce("+",Filter(Negate(is.null),VIIb.AbdAtAge))
    }
  } else if (substring(cruiseName,1,5)=="CSHAS") {
    #Celtic Sea Herring summaries
    cat("\n")
  }
  
  op<-lapply(AbdAtAge,formatOPLine,ages=rep.Ages,numfmt="%.2f")
  #the total column for the CV should be the CV assocated with the survey abundance (CVAbdSur)
  op[['CV']]<-sub(rev(unlist(strsplit(op[['CV']],",")))[1],CVAbdSur,op[['CV']])
  
  #add name to start of list
  for (i in 1:length(op)){op[[i]]<-paste(names(op)[i],op[[i]],sep="")}
  #write header
  writeOPLine(headers[1],file=paste(tables.dir,"AbdAtAge.csv",sep=""),append=FALSE)
  #write details
  lapply(op,writeOPLine,file=paste(tables.dir,"AbdAtAge.csv",sep=""))

}


#Biomass at age by strata report
#----------------------------------------------------------------------------
if (getEstByAge(Species[[Estspecies]])) {
  
  BioAtAge <- lapply(Strata,getBioAtAge,marktypes=rep.MTs)
  #total
  BioAtAge[['Total']] <- Reduce("+",Filter(Negate(is.null),BioAtAge))
  #percentages
  BioAtAge[['%']] <- 100.0*BioAtAge[['Total']]/sum(BioAtAge[['Total']])
  
  if (substring(cruiseName,1,4)=="BFAS") {
    #Boarfish summaries
    cat(" \n")
  } else if (substring(cruiseName,1,5)=="NWHAS") {
    #North West Herring
    #VIaN only
    VIaN <- Strata[lapply(Strata,getICESarea)=="VIaN"]
    VIaN.BioAtAge <- lapply(VIaN,getBioAtAge,marktypes=rep.MTs)
    BioAtAge[['VIaN']] <- Reduce("+",Filter(Negate(is.null),VIaN.BioAtAge))
    #VIaS only
    VIaS <- Strata[lapply(Strata,getICESarea)=="VIaS"]
    VIaS.BioAtAge <- lapply(VIaS,getBioAtAge,marktypes=rep.MTs)
    BioAtAge[['VIaS']] <- Reduce("+",Filter(Negate(is.null),VIaS.BioAtAge))
    #VIIb only
    VIIb <- Strata[lapply(Strata,getICESarea)=="VIIb"]
    VIIb.BioAtAge <- lapply(VIIb,getBioAtAge,marktypes=rep.MTs)
    BioAtAge[['VIIb']] <- Reduce("+",Filter(Negate(is.null),VIIb.BioAtAge))
  } else if (substring(cruiseName,1,5)=="CSHAS"){
    #Celtic Sea Herring
    cat("\n")
  }
  
  op<-lapply(BioAtAge,formatOPLine,ages=rep.Ages,numfmt="%.1f")
  #add name to start of list
  for (i in 1:length(op)){op[[i]]<-paste(names(op)[i],op[[i]],sep="")}
  #write header
  writeOPLine(headers[2],file=paste(tables.dir,"BioAtAge.csv",sep=""),append=FALSE)
  #write details
  lapply(op,writeOPLine,file=paste(tables.dir,"BioAtAge.csv",sep=""))

}

#Abundance at maturity by strata Report
#----------------------------------------------------------------------

if (getEstByMat(Species[[Estspecies]])) {
  
  #no groups specified for boarfish as maturity key was input separately
  if (substring(cruiseName,1,4)=="BFAS") {
  
    AbdAtMat <- lapply(Strata,getAbdAtMat,marktypes=rep.MTs)
  
    #total
    AbdAtMat[['Total']] <- Reduce("+",Filter(Negate(is.null),AbdAtMat))
    #percentages
    AbdAtMat[['%']] <- 100.0*AbdAtMat[['Total']]/sum(AbdAtMat[['Total']])
    
  } else if (substring(cruiseName,1,5)=="NWHAS") {
    #North West Herring
    
    AbdAtMat <- lapply(Strata,getAbdAtMat,marktypes=rep.MTs,matgroups=list("Immature"=getImmatureCodes(Species[[Estspecies]]),
                                                                           "Mature"=getMatureCodes(Species[[Estspecies]]),
                                                                           "Spent"=getSpentCodes(Species[[Estspecies]])))
    #total
    AbdAtMat[['Total']] <- Reduce("+",Filter(Negate(is.null),AbdAtMat))
    #percentages
    AbdAtMat[['%']] <- 100.0*AbdAtMat[['Total']]/sum(AbdAtMat[['Total']])
  
    #VIaN only
    VIaN <- Strata[lapply(Strata,getICESarea)=="VIaN"]
    VIaN.AbdAtMat <- lapply(VIaN,getAbdAtMat,marktypes=rep.MTs,matgroups=list("Immature"=getImmatureCodes(Species[[Estspecies]]),
                                                                              "Mature"=getMatureCodes(Species[[Estspecies]]),
                                                                              "Spent"=getSpentCodes(Species[[Estspecies]])))
    AbdAtMat[['VIaN']] <- Reduce("+",Filter(Negate(is.null),VIaN.AbdAtMat))
    #VIaS only
    VIaS <- Strata[lapply(Strata,getICESarea)=="VIaS"]
    VIaS.AbdAtMat <- lapply(VIaS,getAbdAtMat,marktypes=rep.MTs,matgroups=list("Immature"=getImmatureCodes(Species[[Estspecies]]),
                                                                              "Mature"=getMatureCodes(Species[[Estspecies]]),
                                                                              "Spent"=getSpentCodes(Species[[Estspecies]])))
    AbdAtMat[['VIaS']] <- Reduce("+",Filter(Negate(is.null),VIaS.AbdAtMat))
    #VIIb only
    VIIb <- Strata[lapply(Strata,getICESarea)=="VIIb"]
    VIIb.AbdAtMat <- lapply(VIIb,getAbdAtMat,marktypes=rep.MTs,matgroups=list("Immature"=getImmatureCodes(Species[[Estspecies]]),
                                                                              "Mature"=getMatureCodes(Species[[Estspecies]]),
                                                                              "Spent"=getSpentCodes(Species[[Estspecies]])))
    AbdAtMat[['VIIb']] <- Reduce("+",Filter(Negate(is.null),VIIb.AbdAtMat))
    
  } else if (substring(cruiseName,1,5)=="CSHAS"){
    #Celtic Sea Herring  
    AbdAtMat <- lapply(Strata,getAbdAtMat,marktypes=rep.MTs,matgroups=list("Immature"=getImmatureCodes(Species[[Estspecies]]),
                                                                           "Mature"=getMatureCodes(Species[[Estspecies]]),
                                                                           "Spent"=getSpentCodes(Species[[Estspecies]])))
    #total
    AbdAtMat[['Total']] <- Reduce("+",Filter(Negate(is.null),AbdAtMat))
    #percentages
    AbdAtMat[['%']] <- 100.0*AbdAtMat[['Total']]/sum(AbdAtMat[['Total']])
    
  }
  
  op<-lapply(AbdAtMat,formatOPLine,ages=c("Immature","Mature","Spent"),numfmt="%.2f")
  #add name to start of list
  for (i in 1:length(op)){op[[i]]<-paste(names(op)[i],op[[i]],sep="")}
  #write header
  writeOPLine(headers[3],file=paste(tables.dir,"AbdAtMat.csv",sep=""),append=FALSE)
  #write details
  lapply(op,writeOPLine,file=paste(tables.dir,"AbdAtMat.csv",sep=""))

}


#Biomass at maturity by strata Report
#----------------------------------------------------------------------

if (getEstByAge(Species[[Estspecies]])) {
  
  #no groups specified for boarfish as maturity key was input separately
  if (substring(cruiseName,1,4)=="BFAS") {
    
    BioAtMat <- lapply(Strata,getBioAtMat,marktypes=rep.MTs)
  
    #total
    BioAtMat[['Total']] <- Reduce("+",Filter(Negate(is.null),BioAtMat))
    #percentages
    BioAtMat[['%']] <- 100.0*BioAtMat[['Total']]/sum(BioAtMat[['Total']])
    
  } else if (substring(cruiseName,1,5)=="NWHAS") { 
  
    BioAtMat <- lapply(Strata,getBioAtMat,marktypes=rep.MTs,matgroups=list("Immature"=getImmatureCodes(Species[[Estspecies]]),
                                                                           "Mature"=getMatureCodes(Species[[Estspecies]]),
                                                                           "Spent"=getSpentCodes(Species[[Estspecies]])))
    
    #total
    BioAtMat[['Total']] <- Reduce("+",Filter(Negate(is.null),BioAtMat))
    #percentages
    BioAtMat[['%']] <- 100.0*BioAtMat[['Total']]/sum(BioAtMat[['Total']])
    
    #VIaN only
    VIaN <- Strata[lapply(Strata,getICESarea)=="VIaN"]
    VIaN.BioAtMat <- lapply(VIaN,getBioAtMat,marktypes=rep.MTs,matgroups=list("Immature"=getImmatureCodes(Species[[Estspecies]]),
                                                                              "Mature"=getMatureCodes(Species[[Estspecies]]),
                                                                              "Spent"=getSpentCodes(Species[[Estspecies]])))
    BioAtMat[['VIaN']] <- Reduce("+",Filter(Negate(is.null),VIaN.BioAtMat))
    #VIaS only
    VIaS <- Strata[lapply(Strata,getICESarea)=="VIaS"]
    VIaS.BioAtMat <- lapply(VIaS,getBioAtMat,marktypes=rep.MTs,matgroups=list("Immature"=getImmatureCodes(Species[[Estspecies]]),
                                                                              "Mature"=getMatureCodes(Species[[Estspecies]]),
                                                                              "Spent"=getSpentCodes(Species[[Estspecies]])))
    BioAtMat[['VIaS']] <- Reduce("+",Filter(Negate(is.null),VIaS.BioAtMat))
    #VIIb only
    VIIb <- Strata[lapply(Strata,getICESarea)=="VIIb"]
    VIIb.BioAtMat <- lapply(VIIb,getBioAtMat,marktypes=rep.MTs,matgroups=list("Immature"=getImmatureCodes(Species[[Estspecies]]),
                                                                              "Mature"=getMatureCodes(Species[[Estspecies]]),
                                                                              "Spent"=getSpentCodes(Species[[Estspecies]])))
    BioAtMat[['VIIb']] <- Reduce("+",Filter(Negate(is.null),VIIb.BioAtMat))
    
  } else if (substring(cruiseName,1,5)=="CSHAS") { 
  
    BioAtMat <- lapply(Strata,getBioAtMat,marktypes=rep.MTs,matgroups=list("Immature"=getImmatureCodes(Species[[Estspecies]]),
                                                                           "Mature"=getMatureCodes(Species[[Estspecies]]),
                                                                           "Spent"=getSpentCodes(Species[[Estspecies]])))
    
    #total
    BioAtMat[['Total']] <- Reduce("+",Filter(Negate(is.null),BioAtMat))
    #percentages
    BioAtMat[['%']] <- 100.0*BioAtMat[['Total']]/sum(BioAtMat[['Total']])
    
  }
  
  op<-lapply(BioAtMat,formatOPLine,ages=c("Immature","Mature","Spent"),numfmt="%.1f")
  #add name to start of list
  for (i in 1:length(op)){op[[i]]<-paste(names(op)[i],op[[i]],sep="")}
  #write header
  writeOPLine(headers[4],file=paste(tables.dir,"BioAtMat.csv",sep=""),append=FALSE)
  #write details
  lapply(op,writeOPLine,file=paste(tables.dir,"BioAtMat.csv",sep=""))

}


#Schools, Biomass and Abundance by strata Report
#----------------------------------------------------------------------
op <- lapply(Strata,summary,visible=FALSE,report=TRUE)
#total
t<-lapply(lapply(op,strsplit,split=","),"[[",1)
op$Total <- paste("Total",
                  sum(as.numeric(lapply(t,"[[",2))),
                  sum(as.numeric(lapply(t,"[[",3))),
                  sum(as.numeric(lapply(t,"[[",4))),
                  sum(as.numeric(lapply(t,"[[",5))),
                  sum(as.numeric(lapply(t,"[[",6))),
                  round((sum(as.numeric(lapply(t,"[[",2))*as.numeric(lapply(t,"[[",7))))/sum(as.numeric(lapply(t,"[[",2)))),
                  sum(as.numeric(lapply(t,"[[",8))),
                  sum(as.numeric(lapply(t,"[[",9))),
                  sum(as.numeric(lapply(t,"[[",10))),
                  sum(as.numeric(lapply(t,"[[",11))),
                  sum(as.numeric(lapply(t,"[[",12))),
                  sum(as.numeric(lapply(t,"[[",13))),
                  sep=",")
#CV

#write headers
writeOPLine(headers[5],file=paste(tables.dir,"Strata.csv",sep=""),append=FALSE)
#writeOPLine(headers[6],file=paste(tables.dir,"Strata.csv",sep=""))
#write details
lapply(op,writeOPLine,file=paste(tables.dir,"Strata.csv",sep=""))


#Catch Composition
#----------------------------------------------------------------------
op <- lapply(Hauls,summary,visible=FALSE,report=TRUE)

#find top 4 species name (for header)
top4 <- sort(unlist(lapply(lapply(Species,getName),function(x){sum(lapply(Samples,"[",Samples$SpeciesName==x)$TotalWeight)})),decreasing=TRUE)
if (length(top4)>4){
  t4Names <- names(top4)[1:4]
} else {
  t4Names <- names(top4)
}

writeOPLine(paste(headers[6],paste(t4Names[1],"%",sep=" "),paste(t4Names[2],"%",sep=" "),
                  paste(t4Names[3],"%",sep=" "),paste(t4Names[4],"%",sep=" "),"Others %",sep=","),
            file=paste(tables.dir,"HaulComp.csv",sep=""),append=FALSE)
#write details
lapply(op,writeOPLine,file=paste(tables.dir,"HaulComp.csv",sep=""))


#Abundance at Age as abundance and biomass
#----------------------------------------------------------------------
if (getEstByAge(Species[[Estspecies]])) {  
  AbdAtLen <- combineLFs(lapply(Strata,getAbdAtLen,marktypes=rep.MTs))
  LenAtAge <- lapply(seq_along(AbdAtLen),function(i) AbdAtLen[i]*ALK[names(AbdAtLen)[i],])
  names(LenAtAge) <- names(AbdAtLen)
  op<-lapply(LenAtAge,formatOPLine,ages=rep.Ages,numfmt="%.2f",subzero="-")

  #biomass 
  BioAtLen <- combineLFs(lapply(Strata,getBioAtLen,marktypes=rep.MTs))
  
  #add name to start of list, biomass to end
  for (i in 1:length(op)){
    op[[i]]<-paste(names(op)[i],op[[i]],",",sprintf("%.2f",BioAtLen[names(op)[i]]),sep="")
  }
  
  writeOPLine(headers[7],file=paste(tables.dir,"LenAtAge.csv",sep=""),append=FALSE)
  #write details
  lapply(op,writeOPLine,file=paste(tables.dir,"LenAtAge.csv",sep=""))
}

#11/10/2014
#summary by mark type
op <- lapply(Strata,summary,visible=FALSE,report=FALSE)

bio.mt <- lapply(op,"[[",9) #total biomass
abd.mt <- lapply(op,"[[",10)  #total abundance
ssb.mt <- lapply(op,"[[",11)  #ssb
ssb_abd.mt <- lapply(op,"[[",16)  #ssb

#remove strata names
names(bio.mt)<-NULL
bio.mt<-unlist(bio.mt)
names(abd.mt)<-NULL
abd.mt<-unlist(abd.mt)
names(ssb.mt)<-NULL
ssb.mt<-unlist(ssb.mt)
names(ssb_abd.mt)<-NULL
ssb_abd.mt<-unlist(ssb_abd.mt)

#remove zeros
bio.mt <- bio.mt[bio.mt>0]
abd.mt <- abd.mt[abd.mt>0]
ssb.mt <- ssb.mt[ssb.mt>0]
ssb_abd.mt <- ssb_abd.mt[ssb_abd.mt>0]

#write main header
writeOPLine(paste(Estspecies,",Millions,Biomass(t),% contribution",sep=""),
            file=paste(tables.dir,"SpeciesByMarkType.csv",sep=""),append=FALSE)
#write subhead for total estimate
writeOPLine("Total estimate",file=paste(tables.dir,"SpeciesByMarkType.csv",sep=""))
#loop over mark types
for (mt in unique(names(bio.mt))){
  lop <- mt
  #abundance
  lop <- paste(lop,round(sum(abd.mt[names(abd.mt)==mt])),sep=",")
  #biomass
  lop <- paste(lop,round(sum(bio.mt[names(bio.mt)==mt])),sep=",")
  #% contribution (of biomass)
  lop <- paste(lop,sprintf("%.1f",100.0*sum(bio.mt[names(bio.mt)==mt])/sum(bio.mt)),sep=",")
  writeOPLine(lop,file=paste(tables.dir,"SpeciesByMarkType.csv",sep=""))  
}
#total estimate
lop <- "Total estimate"
#abundance
lop <- paste(lop,round(sum(abd.mt)),sep=",")
#biomass
lop <- paste(lop,round(sum(bio.mt)),sep=",")
#% contribution
lop <- paste(lop,sprintf("%.1f",100.0*sum(bio.mt/sum(bio.mt))),sep=",")
writeOPLine(lop,file=paste(tables.dir,"SpeciesByMarkType.csv",sep=""))



#blank line
writeOPLine("",file=paste(tables.dir,"SpeciesByMarkType.csv",sep=""))
#write subhead for SSB estimate
writeOPLine("SSB estimate",file=paste(tables.dir,"SpeciesByMarkType.csv",sep=""))
#loop over mark types
for (mt in unique(names(ssb.mt))){
  lop <- mt
  #abundance
  lop <- paste(lop,round(sum(ssb_abd.mt[names(ssb_abd.mt)==mt])),sep=",")
  #biomass
  lop <- paste(lop,round(sum(ssb.mt[names(ssb.mt)==mt])),sep=",")
  #% contribution (of biomass)
  lop <- paste(lop,sprintf("%.1f",100.0*sum(ssb.mt[names(ssb.mt)==mt])/sum(ssb.mt)),sep=",")
  writeOPLine(lop,file=paste(tables.dir,"SpeciesByMarkType.csv",sep=""))  
}
#total estimate
lop <- "SSB estimate"
#abundance
lop <- paste(lop,round(sum(ssb_abd.mt)),sep=",")
#biomass
lop <- paste(lop,round(sum(ssb.mt)),sep=",")
#% contribution
lop <- paste(lop,sprintf("%.1f",100.0*sum(ssb.mt/sum(ssb.mt))),sep=",")
writeOPLine(lop,file=paste(tables.dir,"SpeciesByMarkType.csv",sep=""))

#05/01/2015
#results by SR
plot(Cruise,
     filename = paste(plots.dir,"//",getName(Cruise),"_BioBySR.png",sep=""),
     strata = Strata,
     srBoundaries = TRUE,
     printBio = TRUE)
