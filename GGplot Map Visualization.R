#------------------------------------------------------------------
#Visulaizing samples in R
#------------------------------------------------------------------
library(raster)
library(rworldmap)
library(ggplot2) 
library(sf)
library(maptools) #readshapePoly function
library(rgeos)  #gDifference

#For general locations load dataframe from Ceratina.ballparking location.R

# Subsetting to Lat 
ceratina.samples <- read.csv("C:/Users/evank/OneDrive/Desktop/Maps/Ceratina Data/Ceratina Samples.csv")
ceratina.samples <- subset(ceratina.samples, ceratina.samples$Latitude != "?")
ceratina.samples <- subset(ceratina.samples, ceratina.samples$Latitude != "Double Check45.135")
ceratina.samples <- subset(ceratina.samples, ceratina.samples$Latitude != "")
ceratina.samples <- subset(ceratina.samples, ceratina.samples$Latitude != "-")
ceratina.samples <- subset(ceratina.samples, ceratina.samples$Latitude != "- ")
ceratina.samples$Latitude <- as.numeric(as.character(ceratina.samples$Latitude))
ceratina.samples$Longitude <- as.numeric(as.character(ceratina.samples$Longitude))

# Contemporary Specimens
Lat.Long <- read.csv("C:/Users/evank/OneDrive/Desktop/Maps/Ceratina Data/Lat.Long.csv")
Lat.Long <- subset(Lat.Long, Lat.Long$Number.of.Samples != 0)



# Import Maps
#us <- getData("GADM",country="USA",level=1) # level= 0 Canada, 1 Provinces, 2 counties
#canada <- getData("GADM",country="CAN",level=1) # level= 0 USA, 1 States, 2 counties
#canada.province <- getData("GADM",country="CAN",level=2)

# From the computer
 canada <- readRDS("C:/Users/evank/OneDrive/Desktop/Maps/Map Layers/gadm36_CAN_1_sp.rds")
 us <- readRDS("C:/Users/evank/OneDrive/Desktop/Maps/Map Layers/gadm36_USA_1_sp.rds")
 canada.province <- readRDS("C:/Users/evank/OneDrive/Desktop/Maps/Map Layers/gadm36_CAN_2_sp.rds")
 
 
 
### website to download lakes data (shapefile)
### https://gist.github.com/aurielfournier/b897f1b4b507ef823b67f59acfce733d
lakes <- readShapePoly("C:/Users/evank/OneDrive/Desktop/Maps/Map Layers/ne_10m_lakes/ne_10m_lakes") # file path to stored lake shapefile
lakes <- lakes[lakes$scalerank==0,] # subset largest lakes - scalerank ranks lake size 0 = largest

# Trying to make the layer by clipping -  byid=rue does not apper to cut the 
# clip the shape, and byid=FALSE trims the shape but removes the state lines 
# from the map
#usa.land <- gDifference(us, lakes, byid=TRUE) # Clips US by lakes to form a new shapefile
#canada.land <- gDifference(canada, lakes, byid=TRUE)
#canada.province.land <- gDifference(canada.province, lakes, byid=TRUE)


# Museum Map Prior to 1980
ggplot() +
  geom_polygon(data = us , aes(x = long, y = lat, group = group,),
               fill = "antiquewhite" ,color = "black") + # Builds the US Layer
  geom_polygon(data = canada , aes(x = long, y = lat, group = group,),
               fill = "antiquewhite" ,color = "black") + # Builds the Canada Layer
  geom_polygon(data = lakes, aes(x = long, y = lat, group = group,),
              fill = "aliceblue" ,color = "black") + # Builds the Canada Layer
  coord_cartesian(xlim = c(-100, -60), ylim = c(25, 55)) + # Sets the viewing window
  geom_point(data = all.samples.prior1980, aes(x = Long, y = Lat,
                                          size=as.numeric(Number.of.Samples)), col = "purple") +
  theme(panel.grid.major = element_line(color = gray(.5), 
        linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"))



# 1980 - 2009 Specimens - Eastern US
ggplot() +
  geom_polygon(data = us, aes(x = long, y = lat, group = group) ,
               fill = "antiquewhite" ,color = "black") + 
  geom_polygon(data = canada, aes(x = long, y = lat, group = group,),
               fill = "antiquewhite" ,color = "black") +
  geom_polygon(data = lakes, aes(x = long, y = lat, group = group,),
               fill = "aliceblue" ,color = "black") + 
  coord_cartesian(xlim = c(-100, -60), ylim = c(25, 55)) +
  geom_point(data = all.samples.1980.2009, aes(x = Long, y = Lat,
                                          size=as.numeric(Number.of.Samples)), col = "orange") +
  theme(panel.grid.major = element_line(color = gray(.5), 
                                        linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"))


# Contemporary Specimens - Eastern US
ggplot() +
  geom_polygon(data = us, aes(x = long, y = lat, group = group) ,
               fill = "antiquewhite" ,color = "black") + 
  geom_polygon(data = canada, aes(x = long, y = lat, group = group,),
               fill = "antiquewhite" ,color = "black") +
  geom_polygon(data = lakes, aes(x = long, y = lat, group = group,),
               fill = "aliceblue" ,color = "black") + # Builds the Canada Layer
  #geom_path(aes(group=group), size=1) +
  coord_cartesian(xlim = c(-100, -60), ylim = c(25, 55)) +
  geom_point(data = all.samples.2009, aes(x = Long, y = Lat,
                                          size=as.numeric(Number.of.Samples)), col = "green") +
  theme(panel.grid.major = element_line(color = gray(.5), 
                                        linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"))


# Contemporary Specimens - Toronto
ggplot() +
  geom_polygon(data = canada.province, aes(x = long, y = lat, group = group,),
               fill = "white" ,color = "black") +
  #geom_path(aes(group=group), size=1) +
  coord_cartesian(xlim = c(-79.9, -79.25), ylim = c(43.55, 43.95)) +
  geom_point(data = all.samples.2009, aes(x = Long, y = Lat,
                                  size=as.numeric(Number.of.Samples)), col = "blue") +
  theme_bw()


