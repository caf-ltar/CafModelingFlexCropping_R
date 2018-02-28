setwd("C:\\Dev\\Projects\\CafModelingFlexCropping\\CafModelingFlexCropping_R\\Working")

library(ggplot2)
library(ggmap)
library(rgdal)
library(plyr)
library(mapproj)

boundary <- readOGR("../Input/region_boundary", "Boundary")
proj4string(boundary)
WGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
boundary.wgs84 <- spTransform(boundary, WGS84)

# Convert to dataframe
boundary.wgs84@data$id <- rownames(boundary.wgs84@data)
boundary.wgs84.points <- fortify(boundary.wgs84)
boundary.wgs84.df <- merge(boundary.wgs84.points, boundary.wgs84@data, by = "id")

# Convert to dataframe
boundary@data$id <- rownames(boundary@data)
boundary.points <- fortify(boundary)
boundary.df <- merge(boundary.points, boundary@data, by = "id")

sc.raw <- read.csv("../Input/season_sc_20170227.dat", sep = "\t")
sc <- ddply(sc.raw, .(location, rotation), summarize, yield.mean=mean(yield), yield.sd=sd(yield))
sc["yield.cv"] <- sc["yield.sd"] / sc["yield.mean"] * 100
sc["latitude"] <- as.numeric(substr(sc[,"location"], 1, 5))
sc["longitude"] <- as.numeric(substr(sc[,"location"], 7, 12)) * -1
sc["yield.mean.ha"] <- sc["yield.mean"] * 10000

# ---- plot ------
#sc.sp <- SpatialPoints(sc[,7:6], proj4string = WGS84)
sc.sp <- SpatialPoints(sc[,7:6], proj4string = CRS("+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
sc.sp <- SpatialPointsDataFrame(sc.sp, sc)

rbPal <- colorRampPalette(c("orange", "green"))
sc.sp$color <- rbPal(10)[as.numeric(cut(sc.sp$yield.mean, breaks=10))]
plot(boundary.wgs84)
plot(sc.sp, pch = 20, col = sc.sp$color, add=T)

legend(x="topright", legend = levels(sc.sp$yield.mean), col=sc.sp$color, pch=1)

# ---- //plot ----

# ---- ggplot ------------
#ggplot(sc, aes(longitude, latitude)) +
#  geom_point(aes(colour = yield.mean.ha), size = 2)
#
#ggplot(sc, aes(longitude, latitude)) +
#  geom_point(aes(colour = yield.cv), size = 2)
#
#boundary <- ggplot(data = boundary.wgs84.points, aes(x=long, y=lat, group = group)) +
#  geom_path()
#
#print(boundary)

ggplot() +
  geom_polygon(data = boundary.wgs84.df, aes(x=long, y=lat, group = group), 
               color="black", fill="grey100") +
  labs(x="", y="", title="Estimated Spring Canola Yield for HY2018") +
  coord_map(projection = "albers") +
  theme_minimal() +
  theme(plot.title = element_text(lineheight = 0.8, face="bold", vjust=1, hjust=0.5)) +
  theme(legend.text=element_text(size=5))+
  #geom_point(data=sc, aes(x=longitude, y=latitude, color = yield.mean.ha), 
  #           size = 4, shape=15) +
  geom_point(data=sc, aes(x=longitude, y=latitude, color = yield.mean.ha), 
             size = 2) +
  scale_shape_manual(values=c(19,19)) +
  scale_color_gradientn("Est. yield\n(kg/ha)", colors=c("orange3", "green4")) 

# ---- //ggplot ------------


ggplot() +
  geom_polygon(data = boundary.points, aes(x=long, y=lat, group = group), 
               color="black", fill="grey100") + 
  geom_point(data=sc.sp, aes(x=longitude, y=latitude, color = yield.mean.ha), 
             size = 2) +
  coord_map(projection = "lambert", parameters = c(-37, -44))
