setwd("C:\\Dev\\Projects\\CafModelingFlexCropping\\CafModelingFlexCropping_R\\Working")

library(rgdal)
library(plyr)
library(dplyr)
library(geojsonio)
library(raster)
library(foreign)

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
UTMZone11N <- CRS("+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
WGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
# sc.precise.wgs <- SpatialPoints(sc.precise[,4:3], proj4string = WGS84)
# sc.precise.wgs <- SpatialPointsDataFrame(sc.precise.utm, sc.precise)
# sc.precise.utm <- spTransform(sc.precise.wgs, UTMZone11N)

sc.precise.utm <- SpatialPoints(sc.precise[,4:3], proj4string = UTMZone11N)
sc.precise.utm <- SpatialPointsDataFrame(sc.precise.utm, sc.precise)

# Clean up
sc.precise.utm@data$Latitude_DecimalDegrees <- NULL
sc.precise.utm@data$Longitude_DecimalDegrees <- NULL

# Prepare to write stuff
date.today <- format(Sys.Date(), "%Y%m%d")
#write.path.geojson <- paste("../Output/FlexCroppingSpringCanola20180201_", date.today, ".geojson", sep = "")
write.path.geotiff <- paste("../Output/FlexCroppingSpringCanola20180201_", date.today, ".tif", sep = "")
write.path.csv <- paste("../Output/FlexCroppingSpringCanola20180201_", date.today, ".csv", sep = "")

# Write CSV
write.csv(sc.precise, file = write.path.csv)

# Write the file
sc.precise.utm.yield <- sc.precise.utm
sc.precise.utm.yield@data$YieldCoefficientVariation_Percent <- NULL

sc.precise.utm.cv <- sc.precise.utm
sc.precise.utm.cv@data$YieldMean_PoundPerAcre <- NULL



#writeOGR(sc.precise.wgs, write.path.geojson, layer="FlexCroppingSpringCanola20180201", driver="GeoJSON", overwrite_layer = T)
gridded(sc.precise.utm.cv) <- TRUE
cv <- rasterFromXYZ(sc.precise.utm.cv)
projection(cv) <- UTMZone11N
plot(cv)


writeRaster(r, write.path.geotiff, overwrite=T)

rr <- rasterFromXYZ(sc.precise)
projection(rr) <- WGS84
plot(rr)

writeRaster(rr, write.path.geotiff)


#https://gis.stackexchange.com/questions/79062/how-to-make-raster-from-irregular-point-data-without-interpolation
points <- SpatialPoints(sc.precise[,4:3], proj4string = WGS84)
pixels <- SpatialPixelsDataFrame(points, points@data)
r.test <- raster(pixels[,'z'])


# https://www.rdocumentation.org/packages/sp/versions/1.2-7/topics/SpatialPixels
library(sp)
sp <- sc.precise
gt <- GridTopology()
spg <- GridTopology(sp, 4000)


# http://r-sig-geo.2731867.n2.nabble.com/problem-with-gridded-td5817220.html
library(sp)
sp <- sc.precise
coordinates(sp) <- ~Longitude_DecimalDegrees+Latitude_DecimalDegrees
class(sp)
plot(sp)
points2grid(sp, tolerance = 0.01)
plot(sp)

proj4string(sp) <- WGS84
sp.utm <- spTransform(sp, CRSobj = UTMZone11N)


points2grid(sp.utm, tolerance = 0.961154)
sp.utm.r <- rasterFromXYZ(xyz = sp.utm, crs = UTMZone11N)

plot(sp.utm.r)
writeRaster(sp.r, write.path.geotiff, overwrite=T)

# TODO: Try stacked raster: http://rspatial.org/spatial/rst/8-rastermanip.html