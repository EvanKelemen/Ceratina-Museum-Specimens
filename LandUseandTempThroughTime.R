#-------------------------------------------------------------------------------
# Calculating Change in temperature and land-use through time
#-------------------------------------------------------------------------------
# Add required libraries
require(raster)
require(sp)
require(rgdal)
library(lme4)

#https://gis.stackexchange.com/questions/29118/how-to-find-the-average-raster-value-of-an-area-defined-by-a-shapefile-using-r
# $ indicates that the pattern is at the end of the file name
#rlist=list.files(getwd(), pattern="img$", full.names=TRUE) 

#>>>>>>> Temperature Data
# Compile the files
rlist=list.files("C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Temperature Variable CRU/Previous Summer", 
                 pattern=".tif$", full.names=TRUE) 

r <- stack(rlist)   

# Create a polygon 
# A list of all lat and long
xy <- cbind(morph.data.temp$Long, morph.data.temp$Lat)

y_coord <- c(min(xy[,2]),  min(xy[,2]),  max(xy[,2]), max(xy[,2]))
x_coord <- c(min(xy[,1]),  max(xy[,1]),  max(xy[,1]), min(xy[,1]))
xym <- cbind(x_coord, y_coord)
xym

p = Polygon(xym)
ps = Polygons(list(p),1)
sdata = SpatialPolygons(list(ps))
#plot(sdata)

# Extract raster values to list object
r.vals <- extract(r, sdata)

r.vals <- r.vals[[1]]
#Removes NAs
r.vals <- na.omit(r.vals)

# Use list apply to calculate mean for each polygon
r.mean <- apply(r.vals, MARGIN = 2,  FUN=mean)

# Make a Data Frame of the Data
temp.through.time <- data.frame(summer.temp = r.mean, year= 1901:2018)
# Analyze the data
temp.through.time.lm <- lm(summer.temp ~ year, data = temp.through.time)
summary(temp.through.time.lm)




#>>>>> Precipiatation Data
# Make a list of all the precipiation data
rlist=list.files("C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Temperature Variable CRU/Previous Precip", 
                 pattern=".tif$", full.names=TRUE) 
# Stack the files 
r <- stack(rlist)   


# Extract raster values to list object using sdata from above
r.vals <- extract(r, sdata)

r.vals <- r.vals[[1]]
# Removes NAs
r.vals <- na.omit(r.vals)

# Use list apply to calculate mean for each polygon
r.mean <- apply(r.vals, MARGIN = 2,  FUN=mean)

#Make Data Frame
precip.through.time <- data.frame(summer.precip = r.mean, year= 1901:2018)
# Analyze the data
precip.through.time.lm <- lm(summer.precip ~ year, data = precip.through.time)
summary(precip.through.time.lm)






#>>>>>>> Land USe 
# Originally wanted to use a for loop to load in ever raster. So large, had to 
# Load in one at a time

us.year <- c(1974, 1982, 1992, 2002, 2012)
r.us <- stack()
for (i in 1:5){
  filename <- list.files(
    "C:/Users/evank/Documents/ArcGIS/Projects/Temperature/",
    pattern=paste0("lu", us.year[i]), full.names=TRUE)
  temp <- raster(filename)
  # Crop the dimensions of the raster to equal that of sampling area
  temp <- crop(temp, sdata)
  r.us <- stack(temp, r.us)
}

ca.year <- c(1990, 2000, 2010)
r.ca <- list()
for (i in 1:3){
  filename <- list.files(paste0(
    "C:/Users/evank/Documents/ArcGIS/Projects/Temperature/"),
    pattern=paste0("ca", ca.year[i], ".tif$"), full.names=TRUE) 
  temp <- raster(filename)
  # Crop the dimensions of the raster to equal that of sampling area
  temp <- crop(temp, sdata)
  r.ca <- list(temp, r.ca)
}


# Calculates the frequency of different unique cell types
land.use.frequncy<- freq(r.ca)
frequncy.list.ca <- lapply(r.ca, freq)
land.use.frequncy.ca <- merge(frequncy.list.ca[[1]], frequncy.list.ca[[2]], by='value', suffixes=c(2010, 2000), all=TRUE)
land.use.frequncy.ca <- merge(land.use.frequncy.ca, frequncy.list.ca[[3]], by='value', suffixes=c("", 1990), all=TRUE)

# For Canada  Urban 21, 25, Cropland 51
# For US      urban 21, 22, 23, 24, 25, 26, 27, 31, 32, 33, Cropland 43

# Extract Land-use Variable from summary tables
landuse_value <- function(year, landuse_variable, us=TRUE){
  if (us){
  land.use.frequncy[land.use.frequncy$value %in% landuse_variable, paste0("lu", year, "_050815")]
  } else {
    land.use.frequncy.ca[land.use.frequncy.ca$value %in% landuse_variable, paste0("count", year)]
  }
}


# Fill in the data.frame from freq data
land.use.through.time <- data.frame(land.use.ag.raw = c(landuse_value(51, 1974), # 1974 
                                                        landuse_value(51, 1982), # 1982 
                                                        landuse_value(51, 1992), # 1992 
                                                        landuse_value(51, 2002), # 2002 
                                                        landuse_value(51, 2012), # 2012 
                                                        landuse_value(43, 1990, us=FALSE), # 1990
                                                        landuse_value(43, 2000, us=FALSE), # 2000
                                                        landuse_value(43, 2010, us=FALSE)), # 2010
                                    land.use.urb.raw = c(landuse_value(c(21, 22, 23, 24, 25, 26, 27, 31, 32, 33), 1974), # 1974
                                                         landuse_value(c(21, 22, 23, 24, 25, 26, 27, 31, 32, 33), 1982), # 1982
                                                         landuse_value(c(21, 22, 23, 24, 25, 26, 27, 31, 32, 33), 1992), # 1992
                                                         landuse_value(c(21, 22, 23, 24, 25, 26, 27, 31, 32, 33), 2002), # 2002
                                                         landuse_value(c(21, 22, 23, 24, 25, 26, 27, 31, 32, 33), 2012), # 2012
                                                         landuse_value(c(21, 25,), 1990, us=FALSE), #1990
                                                         landuse_value(c(21, 25,), 2000, us=FALSE), #2000
                                                         landuse_value(c(21, 25,), 2010, us=FALSE)),#2010
                                    land.use.total = c(landuse_value(NA, 1974), # 1974
                                                       landuse_value(NA, 1982), # 1982
                                                       landuse_value(NA, 1992), # 1992
                                                       landuse_value(NA, 2002), # 2002
                                                       landuse_value(NA, 2012), # 2012
                                                       landuse_value(c(NA), 1990, us=FALSE), #1990
                                                       landuse_value(c(NA), 2000, us=FALSE), #2000
                                                       landuse_value(c(NA), 2010, us=FALSE)), #2010
                                    year = c(1974, 1982, 1992, 2002, 2012,
                                             1990, 2000, 2010),
                                    country = c(rep("US", 5),
                                                rep("CA", 3)))
                                    
# Calulate the precentage of agriculature and urbanization
land.use.through.time$percent.ag <- land.use.through.time$land.use.ag.raw/
  land.use.through.time$land.use.total *100

land.use.through.time$percent.urb <- land.use.through.time$land.use.urb.raw/
  land.use.through.time$land.use.total * 100


# Analyze the data
ag.through.time.lm <- lme(percent.ag ~ year, random = ~1|country, data = land.use.through.time)
summary(ag.through.time.lm)

urb.through.time.lm <- lme(percent.urb ~ year, random = ~1|country, data = land.use.through.time)
summary(urb.through.time.lm)

plot(percent.urb~year, data= land.use.through.time)

