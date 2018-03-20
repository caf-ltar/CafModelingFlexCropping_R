setwd("C:\\Dev\\Projects\\CafModelingFlexCropping\\CafModelingFlexCropping_R\\Working")

library(rgdal)
library(plyr)
library(dplyr)
library(geojsonio)
library(raster)
library(foreign)

# Takes a dataframe of specific format (see code) with geocoordinates in WGS84 and creates a geoTIFF in WGS84 
# df = dataframe with columns: Rotation, YieldCoefficientVariation_Percent, YieldMean_PoundPerAcre, Latitude_DecimalDegrees, Longitude_DecimalDegrees, and geocoords in WGS84
# baseFilename = Final filename will be: {baseFilename}_{"cv" or "yield"}_{date}.tif
generateGeoTiff <- function(df, baseFilename) {
  # convert to spatial data
  #UTMZone11N <- CRS("+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  WGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  df.wgs <- SpatialPoints(df[,4:3], proj4string = WGS84)
  df.wgs <- SpatialPointsDataFrame(df.wgs, df)
  
  # Clean up
  df.wgs@data$Latitude_DecimalDegrees <- NULL
  df.wgs@data$Longitude_DecimalDegrees <- NULL
  
  # Prepare to write files
  date.today <- format(Sys.Date(), "%Y%m%d")
  write.path.geotiff.cv <- paste("../Output/", baseFilename, "_cv_", date.today, ".tif", sep = "")
  write.path.geotiff.yield <- paste("../Output/", baseFilename, "_yield_", date.today, ".tif", sep = "")
  write.path.csv <- paste("../Output/", baseFilename, "_", date.today, ".csv", sep = "")
  
  # Write CSV
  write.csv(df, file = write.path.csv, row.names = F)
  
  # Write tiffs
  df.wgs.yield <- df.wgs
  df.wgs.yield@data$YieldCoefficientVariation_Percent <- NULL
  
  df.wgs.cv <- df.wgs
  df.wgs.cv@data$YieldMean_PoundPerAcre <- NULL
  
  cv <- rasterFromXYZ(df.wgs.cv)
  projection(cv) <- WGS84
  plot(cv)
  
  yield <- rasterFromXYZ(df.wgs.yield)
  projection(yield) <- WGS84
  plot(yield)
  
  
  writeRaster(cv, write.path.geotiff.cv, overwrite=T)
  writeRaster(yield, write.path.geotiff.yield, overwrite=T)
}


# Read result data from FlexCropping run
raw <- read.csv("../Input/season_all_20180320.dat", sep = "\t")

# Merge by location and rotation, calculate mean and stdev of the merged groups
#sc <- ddply(sc.raw, .(location, rotation), summarize, yield.mean=mean(yield), yield.sd=sd(yield))
df <- ddply(raw, .(location, rotation), summarize, yield.mean=mean(yield), yield.sd=sd(yield))

# Calc coefficient of variation for yield, and convert yield to lb/ac
#sc["yield.cv"] <- sc["yield.sd"] / sc["yield.mean"] * 100
#sc["yield.mean.lb.ac"] <- sc["yield.mean"] * 8921.79
df["yield.cv"] <- df["yield.sd"] / df["yield.mean"] * 100
df["yield.mean.lb.ac"] <- df["yield.mean"] * 8921.79

# Read original geocoord file - used to extract more precise geocoords
geodata <- read.dbf("../Input/aec_dryland.dbf")

# Reverse engineer location string from geodata
geodata.loc <- geodata
geodata.loc$Latitude.slim <- format(round(geodata$Latitude, digits=2), nsmall = 2)
geodata.loc$Longitude.slim <- format(round(geodata$Longitude * -1, digits=2), nsmall = 2)
geodata.loc["location"] <- paste(geodata.loc$Latitude.slim, "N", geodata.loc$Longitude.slim, "W", sep = "")

# Merge by location column
#sc.precise <- merge(x = sc, y = geodata.loc[, c("Latitude", "Longitude", "location")], by = "location", all.x = TRUE)
df.precise <- merge(x = df, y = geodata.loc[, c("Latitude", "Longitude", "location")], by = "location", all.x = TRUE)

# Clean up data
df.precise["location"] <- NULL
df.precise["yield.mean"] <- NULL
df.precise["yield.sd"] <- NULL

df.precise$yield.cv <- round(df.precise$yield.cv, digits = 2)
df.precise$yield.mean.lb.ac <- round(df.precise$yield.mean.lb.ac, digits = 2)

names(df.precise) <- c("Rotation", "YieldCoefficientVariation_Percent", "YieldMean_PoundPerAcre", "Latitude_DecimalDegrees", "Longitude_DecimalDegrees")

# Split by rotation
sc <- df.precise[df.precise$Rotation == "sC-wW",]
sc$Rotation <- NULL
sp <- df.precise[df.precise$Rotation == "sP-wW",]
sp$Rotation <- NULL
sw <- df.precise[df.precise$Rotation == "sW-wW",]
sw$Rotation <- NULL

# Create GeoTIFF
generateGeoTiff(sc, "FlexCroppingSpringCanola20180201")
generateGeoTiff(sp, "FlexCroppingSpringPea20180201")
generateGeoTiff(sw, "FlexCroppingSpringWheat20180201")
