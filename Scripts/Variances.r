#Survey abundance and biomass variance/cv calculations

#weighting for strata
#those strata with fewer transects get a higher weighting and thus contribute
#proportionally more to the overall variance
#also, those with a bigger area
strata.wgt<-vector("numeric",length=length(Strata))
strata.area<-vector("numeric",length=length(Strata))
strata.areasq<-vector("numeric",length=length(Strata))
names(strata.wgt)<-unlist(lapply(Strata,getCode))
names(strata.areasq)<-unlist(lapply(Strata,getCode))
names(strata.area)<-unlist(lapply(Strata,getCode))

#loop over strata and calculate weighting factor
for (str in lapply(Strata,getCode)) {
  
  #area
  strata.area[str] <- getArea(Strata[[str]])
  #area squared
  strata.areasq[str] <- (getArea(Strata[[str]]))^2
  
  #retrieve the transects for this strata
  tr <- Transects[lapply(Transects,getStratumCode)==str]

  if (length(tr)>0) {
    #transect lengths, used to calculate a weighting factor 
    llen<-as.vector(unlist(lapply(tr,getTrackLength_nm)))
    #weighting factor
    strata.wgt[str] <- sum(llen^2)/sum(llen)^2      
  }
  
}

#initialise survey total variance,mean abundance & biomass
vAbdSur <- 0
vBioSur <- 0
AbdSur <- 0
BioSur <- 0

for (mt in lapply(spe.MarkTypes,getName)){
  
  #initialise mark type total variance,mean abundance & biomass
  vAbdTot<-0
  vBioTot<-0
  mnAbdTot<-0
  mnBioTot<-0
  
  #loop over strata
  for (str in lapply(Strata,getCode)) {
    
    #retrieve the transects for this strata
    tr <- Transects[lapply(Transects,getStratumCode)==str]
    
    #using the transect variance and the length of the transect
    #calculate the strata variance, weight it accordingly and add
    #it to the total variance 
    
    if (length(tr)>0) {
      
      #calculate abundances/biomass variances using transect mean abundance
      #and biomass
      vabd<-var(as.vector(unlist(lapply(tr,getMeanAbundance,mt))))
      vbio<-var(as.vector(unlist(lapply(tr,getMeanBiomass,mt))))
      
      #transect lengths, used to calculate a weighting factor 
      #llen<-as.vector(unlist(lapply(tr,getTrackLength_nm)))
      #wvar <- sum(llen^2)/sum(llen)^2      
      
      #strata Area
      #area <- getArea(Strata[[str]])
      
      #strata mean abundance and biomass
      mnabd<-getMeanAbundance(Strata[[str]],mt)
      mnbio<-getMeanBiomass(Strata[[str]],mt)
      
      #if (!is.na(vabd)) {vAbdTot <- vAbdTot + vabd*wvar*area*area}
      #if (!is.na(vbio)) {vBioTot <- vBioTot + vbio*wvar*area*area}             
      if (!is.na(vabd)) {vAbdTot <- vAbdTot + vabd*strata.wgt[str]*strata.areasq[str]}
      if (!is.na(vbio)) {vBioTot <- vBioTot + vbio*strata.wgt[str]*strata.areasq[str]}             
      #if (!is.na(mnabd)) {mnAbdTot <- mnAbdTot + mnabd*area}
      #if (!is.na(mnbio)) {mnBioTot <- mnBioTot + mnbio*area}
      if (!is.na(mnabd)) {mnAbdTot <- mnAbdTot + mnabd*strata.area[str]}
      if (!is.na(mnbio)) {mnBioTot <- mnBioTot + mnbio*strata.area[str]}
    } 
    
  }
  
  #cvs for each mark type
  cv.abd = 100*sqrt(vAbdTot)/mnAbdTot;
  cv.bio = 100*sqrt(vBioTot)/mnBioTot;
  
  #cat("vAbdTot,vBioTot=",vAbdTot,vBioTot,"\n")
  #cat("cv.abd,cv.bio=",mt,cv.abd,cv.bio,"\n")
  
  #update totals so cv can be calculated at survey level
  if (!is.nan(mnAbdTot)) {AbdSur <- AbdSur + mnAbdTot}
  if (!is.nan(mnBioTot)) {BioSur <- BioSur + mnBioTot}
  if (!is.nan(mnAbdTot) & !is.nan(cv.abd)) {vAbdSur <- vAbdSur + (mnAbdTot*cv.abd/100)^2}
  if (!is.nan(mnBioTot) & !is.nan(cv.bio)) {vBioSur <- vBioSur + (mnBioTot*cv.bio/100)^2}
  
} #end loop over mark types

#Survey CV on abundance /biomass
CVAbdSur <- 100*sqrt(vAbdSur)/AbdSur
CVBioSur <- 100*sqrt(vBioSur)/BioSur


#variance in numbers at age

#initialise totals
#variance
vAgeTot<-0
#numbers
nAgeTot<-0

#loop over mark types
for (mt in lapply(spe.MarkTypes,getName)){

  #get numbers at age for all transects
  NatAge<-lapply(Transects,getAbdAtAge,mt)
  
  #declare a matrix to store variance at age per Stratum
  #TO DO -remove hardcoded limit on num of age classes
  #Va<-matrix(0,nrow=length(Strata),ncol=12)
  Va<-matrix( 0, nrow=length(Strata), ncol = (if (is.null(ALK)) {1} else {ncol(ALK)}))
  
  #name each row according to stratum code
  rownames(Va)<-lapply(Strata,getCode)
  
  #calculate variances - TO DO clean this up
  for (i in 1:nrow(Va)){  #loop over strata
    
    #extract numbers at age for transects on this stratum
    t<-NatAge[names(Transects[lapply(Transects,getStratumCode)==rownames(Va)[i]])]
    
    for (j in 1:ncol(Va)){  #loop over ages
      dat<-c()
      if (length(t)>1) {
        for (k in 1:length(t)) {
          if (!is.null(t[[k]][j])) {
            dat<-c(dat,t[[k]][j])
          } else {
            dat<-c(dat,0)
          }
        }
        
        #calculate varaince
        if (!is.null(dat)){Va[i,j]<-var(dat)}
        
      }
    }
  }     #loop over strata
  
  #apply weightings to variance for strata
  for (str in lapply(Strata,getCode)) {Va[str,] <- Va[str,]*strata.wgt[str]*strata.areasq[str]}
  
  #get total abundance at age for this marktype
  TotAbs<-vector("numeric",length = (if (is.null(ALK)) {1} else {ncol(ALK)}))
  
  for (i in 1:length(Strata)){
    t<-getAbdAtAge(Strata[[i]],mt)
    #if (!is.null(t)) {TotAbs <- TotAbs + t*getArea(Strata[[i]])}
    if (!is.null(t)) {TotAbs <- TotAbs + t}
  }

  #total variance by age
  Vat<-apply(Va,2,sum,na.rm=TRUE)
  
  #calculate cv at age
  cvs <- 100*apply(Va,2,sum,na.rm=TRUE)^0.5/TotAbs
  
  SD <- TotAbs*cvs/100
  #set the standard deviation for 0 results to 0
  SD[TotAbs==0] <- 0
  
  #update totals
  vAgeTot <- vAgeTot + SD^2
  nAgeTot <- nAgeTot + TotAbs  

} #end loop over marktypes


#final cv
#calculate final CVs as root(var)/mean
CVatAge<-100*(vAgeTot^0.5/nAgeTot)




