#Acoustic Analysis plots

#mark types to include (for SA data)
mt<-lapply(MarkTypes,getNASCName)

#plot Cruise
plot(Cruise,
     strata = Strata,
     transects = Transects,
     ctds = (if (exists("CTDs")) {CTDs} else {NULL}),
     hauls = (if (exists("Hauls")) {Hauls} else {NULL}),
     sa = (if (exists("SA")) {SA[which(toupper(lapply(SA,getMarkType))%in%toupper(unlist(mt)))]} else {NULL}),
     sasummary = TRUE,
     filename = paste(plots.dir,"//",getName(Cruise),".png",sep=""),
     leg.pos = "topleft")

#plot locations of CTD casts
plot(Cruise,
     transects=Transects,
     ctds=(if (exists("CTDs")) {CTDs} else {NULL}),
     filename=paste(plots.dir,"//",getName(Cruise),"_CTD.png",sep=""),
     srboundaries=TRUE,
     bathycontours=c(250,300,400,500))

#plot locations of hauls
plot(Cruise,
     transects=Transects,
     hauls=Hauls,
     filename=paste(plots.dir,"//",getName(Cruise),"_Haul.png",sep=""),
     srboundaries=TRUE,
     bathycontours=c(250,300,400,500))

#plots of marktypes
for (p in 1:length(mt)){
  
  f.op <- paste(plots.dir,"//",getName(Cruise),"_",sub(" ","",mt[[p]]),".png",sep="")  
  
  plot(Cruise,
       transects=Transects,
       sa=SA[toupper(lapply(SA,getMarkType))==toupper(mt[[p]])],
       filename=f.op,
       srboundaries=TRUE,
       bathycontours=c(250,300,400,500),
       leg.pos="bottomleft")
}

#plot strata
for(i in 1:length(Strata)) {
  plot(Strata[[i]],
       filename=paste(plots.dir,"//Strata_",as.character(getCode(Strata[[i]])),".png",sep=""),
       transects = Transects,
       sa=(if (exists("SA")) {SA[which(lapply(SA,getMarkType)%in%unlist(mt))]} else {NULL}),
       Track=(if (exists("Track")) {Track} else {NULL}));
}