setwd("C:\\Dev\\Projects\\CafModelingFlexCropping\\CafModelingFlexCropping_R\\Working")

library(ggplot2)
library(ggmap)
library(rgdal)
library(plyr)
library(mapproj)

# TODO: Create a function that converts spatialdataframe to DF

# Read in boundary file, convert to WGS84
boundary <- readOGR("../Input/region_boundary", "Boundary")
WGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
boundary.wgs84 <- spTransform(boundary, WGS84)

# Convert boundary to dataframe for ggplot
boundary.wgs84@data$id <- rownames(boundary.wgs84@data)
boundary.wgs84.points <- fortify(boundary.wgs84)
boundary.wgs84.df <- merge(boundary.wgs84.points, boundary.wgs84@data, by = "id")

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

# Read in overlay data
cities <- readOGR("../Input/Cities", "Cities")
rivers <- readOGR("../Input/Rivers", "Rivers")
counties <- readOGR("../Input/Counties", "Counties")
counties.wgs <- spTransform(counties, WGS84)

# ---- plot ------
#sc.wgs <- SpatialPoints(sc[,7:6], proj4string = WGS84)
#sc.wgs <- SpatialPointsDataFrame(sc.wgs, sc)

sc.precise.wgs <- SpatialPoints(sc.precise[,8:7], proj4string = WGS84)
sc.precise.wgs <- SpatialPointsDataFrame(sc.precise.wgs, sc.precise)

# Write the file
date.today <- format(Sys.Date(), "%Y%m%d")
write.path.geojson <- paste("../Output/FlexCroppingSpringCanola20180201_", date.today, ".geojson", sep = "")
#writeOGR(sc.precise.wgs, ".", "../Output/spring-canola", driver="ESRI Shapefile")
writeOGR(sc.precise.wgs, write.path.geojson, layer="FlexCroppingSpringCanola20180201", driver="GeoJSON")
#rbPal <- colorRampPalette(c("orange", "green"))
#sc.sp$color <- rbPal(10)[as.numeric(cut(sc.sp$yield.mean, breaks=10))]
#plot(boundary.wgs84)
#plot(sc.sp, pch = 20, col = sc.sp$color, add=T)

# ---- //plot ----

# ---- ggplot ------------

ggplot() +
  geom_polygon(data = boundary.wgs84.df, aes(x=long, y=lat, group = group), 
               color="black", fill="grey100") +
  labs(x="", y="", title="Estimated Spring Canola Yield for HY2018") +
  theme_minimal() +
  theme(plot.title = element_text(lineheight = 0.8, face="bold", vjust=1, hjust=0.5)) +
  theme(legend.text=element_text(size=5))+
  #geom_point(data=sc, aes(x=longitude, y=latitude, color = yield.mean.ha), 
  #           size = 4, shape=15) +
  geom_point(data=sc.precise, aes(x=Longitude, y=Latitude, color = yield.mean.lb.ac), 
             size = 4, shape=15) +
  scale_shape_manual(values=c(19,19)) +
  scale_color_gradientn("Est. yield\n(lb/ac)", colors=c("orange3", "green4")) 

# ---- //ggplot ------------
