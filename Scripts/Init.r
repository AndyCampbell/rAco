#Initialization

#R libraries
#classes and methods for spatial data
library(sp);
#spherical trigonometry
library(geosphere);

#source class definitions/ functions
#source(".//Source//Geography.r"); - use functions in geosphere package instead
#source(".//Source//Doonan.r");

#package classes
#source("./Source/GenericS4.r");
#source("./Source/GeoPointS4.r");
#source("./Source/WayPointS4.r");
#source("./Source/CTDStationS4.r");
#source("./Source/CruiseS4.r");
#source("./Source/TargetSpeciesS4.r");
#source("./Source/HaulS4.r");
#source("./Source/MarkS4.r");
#source("./Source/TransectS4.r");
#source("./Source/StratumS4.r");
#source("./Source/MarkTypeS4.r");
#source("./Source/Constructors.r")
library(rAcoS4)

#other functions
source(".//Scripts//AcoFunctions.r");

#coastline
load(file="./RData/AcoS4_coast.rda")

#bathymetry
load(file="./RData/Bathymetry.rda");
