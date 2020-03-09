#------------------------------------------------------------------
#Visulaizing samples in R
#------------------------------------------------------------------
library(maptools)
library(maps)
library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(mapview) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(shiny)   # for web applications

ceratina.samples <- read.csv("C:/Users/evank/OneDrive/Desktop/Ceratina Samples.csv")
ceratina.samples <- subset(ceratina.samples, ceratina.samples$Latitude != "?")
ceratina.samples <- subset(ceratina.samples, ceratina.samples$Latitude != "Double Check45.135")
ceratina.samples <- subset(ceratina.samples, ceratina.samples$Latitude != "")
ceratina.samples <- subset(ceratina.samples, ceratina.samples$Latitude != "-")
ceratina.samples <- subset(ceratina.samples, ceratina.samples$Latitude != "- ")
ceratina.samples$Latitude <- as.numeric(as.character(ceratina.samples$Latitude))
ceratina.samples$Longitude <- as.numeric(as.character(ceratina.samples$Longitude))

## Specify a geographic extent for the map
## by defining the top-left and bottom-right geographic coordinates
mapExtent <- rbind(c(-156, 80), c(-68, 19))

## Specify the required projection using a proj4 string
## Use http://www.spatialreference.org/ to find the required string
## Polyconic for North America
newProj <- CRS("+proj=poly +lat_0=0 +lon_0=-100 +x_0=0 
            +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

## Project the map extent (first need to specify that it is longlat) 
mapExtentPr <- spTransform(SpatialPoints(mapExtent, 
                                         proj4string=CRS("+proj=longlat")),
                           newProj)
# can0<-getData('GADM', country="CAN", level=0) # Canada
can1<-getData('GADM', country="CAN", level=1) # provinces
# can2<-getData('GADM', country="CAN", level=2) # counties

#plot(can1)    
#spplot(can1, "NAME_1") # colors the provinces and provides
# a color-coded legend for them
#can1$NAME_1            # returns names of provinces/territories
# us0 <- getData('GADM', country="USA", level=0)
us1 <- getData('GADM', country="USA", level=1)
# us2 <- getData('GADM', country="USA", level=2)
#plot(us1)              # state boundaries split at 
# the dateline
#us1$NAME_1             # returns names of the states + DC
#spplot(us1, "ID_1")
#spplot(us1, "NAME_1")  # color codes states and
# provides their names
#
## Project other layers
can1Pr <- spTransform(can1, newProj)
us1Pr <- spTransform(us1, newProj) 

## Plot each projected layer, beginning with the projected extent
plot(mapExtentPr, pch=NA)
plot(can1Pr, border="white", col="lightgrey", add=TRUE)
plot(us1Pr, border="white", col="lightgrey", add=TRUE)



library(raster)
library(rworldmap)
states    <- c('California', 'Nevada', 'Utah', 'Colorado', 'Wyoming', 'Montana', 'Idaho', 'Oregon', 'Washington')
provinces <- c("British Columbia", "Alberta")

us <- getData("GADM",country="USA",level=1)
canada <- getData("GADM",country="CAN",level=1)

us.states <- us[us$NAME_1 %in% states,]
ca.provinces <- canada[canada$NAME_1 %in% provinces,]

us.bbox <- bbox(us.states)
ca.bbox <- bbox(ca.provinces)
xlim <- c(min(us.bbox[1,1],ca.bbox[1,1]),max(us.bbox[1,2],ca.bbox[1,2]))
ylim <- c(min(us.bbox[2,1],ca.bbox[2,1]),max(us.bbox[2,2],ca.bbox[2,2]))
plot(us.states, xlim=xlim, ylim=ylim)
plot(ca.provinces, xlim=xlim, ylim=ylim, add=T)
plot(us, xlim = c(-90, -60), ylim = c(25, 55))
plot(canada, xlim = c(-90, -60), ylim = c(25, 55), add = T)

 newmap <- getMap(resolution = "low")
 plot(newmap, xlim = c(-90, -60), ylim = c(25, 55), asp = 1)
points(ceratina.samples$Longitude, ceratina.samples$Latitude, col = "red", cex = .6)




ggplot() +
  geom_polygon(data = us, aes(x = long, y = lat, group = group) ,
               fill = "white" ,color = "black") + 
  geom_polygon(data = canada, aes(x = long, y = lat, group = group,),
               fill = "white" ,color = "black") +
  #geom_path(aes(group=group), size=1) +
  coord_cartesian(xlim = c(-90, -60), ylim = c(25, 55)) +
  geom_point(data = ceratina.samples, aes(x = Longitude, y = Latitude,
                                         size=Number.of.Samples))+
  theme_bw()








# Basemap
carto <- "https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}{r}.png"

# Create a "normal" tmap except we'll add  
# the basemap and the `popup.vars` argument.
# The symbol size of the bubbles will be
# based on the data so use our calculated
# field `diffrad` which will apply sizes
# 1 through 5. Sizes can be further adjusted
# using the `scale` argument. 
cnty <- st_as_sf(map("county", plot = FALSE, fill = TRUE))

mymap <- tm_basemap(carto) +  
  tm_shape(cnty) +
  tm_borders(col = "azure2") +
  tm_bubbles("diffrad", 
             col = "Insured Adults Ages 18-34, 2012-2016", 
             border.col = "white", 
             scale = 1.5,
             style = "fixed",
             palette = c("coral2", "aquamarine3", "gray"),
             popup.vars = c("County: " = "NAME", "Change: " = "popup"))

tmap_leaflet(mymap)

?tm_shape
