setwd("C:\\Dev\\Projects\\CafModelingFlexCropping\\CafModelingFlexCropping_R\\Working")

library(rgdal)
library(plyr)
library(dplyr)
library(geojsonio)

# Read result data from FlexCropping run
sc.raw <- read.csv("../Input/season_sc_20170227.dat", sep = "\t")

# Merge by location and rotation, calculate mean and stdev of the merged groups
sc <- ddply(sc.raw, .(location, rotation), summarize, yield.mean=mean(yield), yield.sd=sd(yield))

# Calc coefficient of variation for yield, and convert yield to lb/ac
sc["yield.cv"] <- sc["yield.sd"] / sc["yield.mean"] * 100
sc["yield.mean.lb.ac"] <- sc["yield.mean"] * 8921.79

# Read original geocoord file - used to extract more precise geocoords
geodata <- read.dbf("../Input/aec_dryland.dbf")

# Reverse engineer location string from geodata
geodata.loc <- geodata
geodata.loc$Latitude.slim <- format(round(geodata$Latitude, digits=2), nsmall = 2)
geodata.loc$Longitude.slim <- format(round(geodata$Longitude * -1, digits=2), nsmall = 2)
geodata.loc["location"] <- paste(geodata.loc$Latitude.slim, "N", geodata.loc$Longitude.slim, "W", sep = "")

# Merge by location column
sc.precise <- merge(x = sc, y = geodata.loc[, c("Latitude", "Longitude", "location")], by = "location", all.x = TRUE)

# Clean up data
sc.precise["location"] <- NULL
sc.precise["rotation"] <- NULL
sc.precise["yield.mean"] <- NULL
sc.precise["yield.sd"] <- NULL

sc.precise$yield.cv <- round(sc.precise$yield.cv, digits = 2)
sc.precise$yield.mean.lb.ac <- round(sc.precise$yield.mean.lb.ac, digits = 2)

names(sc.precise) <- c("YieldCoefficientVariation_Percent", "YieldMean_PoundPerAcre", "Latitude_DecimalDegrees", "Longitude_DecimalDegrees")

# convert to spatial data
WGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
sc.precise.wgs <- SpatialPoints(sc.precise[,4:3], proj4string = WGS84)
sc.precise.wgs <- SpatialPointsDataFrame(sc.precise.wgs, sc.precise)

# Clean up
sc.precise.wgs@data$Latitude_DecimalDegrees <- NULL
sc.precise.wgs@data$Longitude_DecimalDegrees <- NULL

# Write the file
date.today <- format(Sys.Date(), "%Y%m%d")
write.path.geojson <- paste("../Output/FlexCroppingSpringCanola20180201_", date.today, ".geojson", sep = "")
writeOGR(sc.precise.wgs, write.path.geojson, layer="FlexCroppingSpringCanola20180201", driver="GeoJSON", overwrite_layer = T)
