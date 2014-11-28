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
source("./Source/GenericS4.r");
source("./Source/GeoPointS4.r");
source("./Source/WayPointS4.r");
source("./Source/CTDStationS4.r");
source("./Source/CruiseS4.r");
source("./Source/TargetSpeciesS4.r");
source("./Source/HaulS4.r");
source("./Source/MarkS4.r");
source("./Source/TransectS4.r");
source("./Source/StratumS4.r");
source("./Source/MarkTypeS4.r");
source("./Source/Constructors.r")

#other functions
source(".//Scripts//AcoFunctions.r");

# if (cruiseName == "NWHAS2011") {
#   Cruise <- cruise(code="CE11008",
#                   name="NWHAS2011",
#                   desc="North-West Herring Acoustic Survey, 2011",
#                   vessel="Celtic Explorer",
#                   start_date=as.POSIXlt(strptime("2011-06-19 00:00:00","%Y-%m-%d %H:%M:%S")),
#                   end_date=as.POSIXlt(strptime("2011-07-07 23:59:59", "%Y-%m-%d %H:%M:%S")));
# } else if (cruiseName == "NWHAS2012") {
#   Cruise <- cruise(code="CE12009",
#                    name="NWHAS2012",
#                    desc="North-West Herring Acoustic Survey, 2012",
#                    vessel="Celtic Explorer",
#                    start_date=as.POSIXlt(strptime("2012-06-21 00:00:00","%Y-%m-%d %H:%M:%S")),
#                    end_date=as.POSIXlt(strptime("2012-07-10 23:59:59", "%Y-%m-%d %H:%M:%S")));
# } else if (cruiseName == "NWHAS2013") {
#   Cruise <- cruise(code="CE13009",
#                   name="NWHAS2013",
#                   desc="North-West Herring Acoustic Survey, 2013",
#                   vessel="Celtic Explorer",
#                   start_date=as.POSIXlt(strptime("2013-06-22 00:00:00","%Y-%m-%d %H:%M:%S")),
#                   end_date=as.POSIXlt(strptime("2013-07-11 23:59:59", "%Y-%m-%d %H:%M:%S")));
# } else if (cruiseName == "NWHAS2014") {
#   Cruise <- cruise(code="CE14010",
#                   name="NWHAS2014",
#                   desc="North-West Herring Acoustic Survey, 2014",
#                   vessel="Celtic Explorer",
#                   start_date=as.POSIXlt(strptime("2013-06-24 00:00:00","%Y-%m-%d %H:%M:%S")),
#                   end_date=as.POSIXlt(strptime("2013-07-11 23:59:59", "%Y-%m-%d %H:%M:%S")));
# } else if (cruiseName == "BFAS2013") {
#   Cruise <- cruise(code="BFAS2013",
#                    name="BFAS2013",
#                    desc="Boarfish Acoustic Survey, 2013",
#                    vessel="F.V. Felucca",
#                    start_date=as.POSIXlt(strptime("2013-06-30 00:00:00","%Y-%m-%d %H:%M:%S")),
#                    end_date=as.POSIXlt(strptime("2013-07-28 23:59:59", "%Y-%m-%d %H:%M:%S")));  
# } else {
#   cat("No cruise details available. Open script init.r and enter them\n")
# }


#coastline
load(file="./RData/AcoS4_coast.rda")

#bathymetry
load(file="./RData/Bathymetry.rda");
