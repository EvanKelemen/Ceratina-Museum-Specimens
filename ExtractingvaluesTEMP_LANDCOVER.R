#-------------------------------------------------------------------------------
# Calculating Change in temperature through time
#-------------------------------------------------------------------------------
#https://gis.stackexchange.com/questions/29118/how-to-find-the-average-raster-value-of-an-area-defined-by-a-shapefile-using-r
# $ indicates that the pattern is at the end of the file name
#rlist=list.files(getwd(), pattern="img$", full.names=TRUE) 

rlist=list.files("C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Temperature Variable CRU/Previous Summer", 
                 pattern=".tif$", full.names=TRUE) 

r <- stack(rlist)   

# Add required libraries
require(raster)
require(sp)
require(rgdal)

# Set working directory, raster, in and out shapefiles

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
plot(sdata)


# Extract raster values to list object
r.vals <- extract(r, sdata)


r.vals <- r.vals[[1]]
r.vals <- na.omit(r.vals)

# Use list apply to calculate mean for each polygon
r.mean <- apply(r.vals, MARGIN = 2,  FUN=mean)

length(r.mean)

# Analyze the data
temp.through.time <- data.frame(ave.temp = r.mean, year= 1901:2018)
temp.through.time.lm <- lm(ave.temp ~ year, data = temp.through.time)
summary(temp.through.time.lm)



# Land USe 
us.year <- c(1992) #, 1982, 1992, 2002, 2012)
result <- vector("character", 1)
for (i in 1:1){
  filename <- list.files(
    "C:/Users/evank/Documents/ArcGIS/Projects/Temperature/",
    pattern=paste0("lu", us.year[i]), full.names=TRUE)
  result[i] <- filename[1]
}




ca.year <- c( 2000) 
result.ca <- vector("character", 1)
for (i in 1:1){
  filename <- list.files(paste0(
    "C:/Users/evank/Documents/ArcGIS/Projects/Temperature/"),
    pattern=paste0("ca", ca.year[i], ".tif$"), full.names=TRUE) 
  result.ca[i] <- filename
}

# These stacks are to large to work with
r.us <- stack(result)

r.ca <- stack(result.ca)

# Check if the raster and the sampling area overlap
plot(r.ca)
plot(sdata, add = T)

# Crop the dimensions of the raster to equal that of sampling area
r.us <- crop(r.us, sdata)



#-----Scratch-----
# Tried changing the projection in R but "no enough memory" so changed projection in ArcGIS
#rasterOptions(memfrac=.3)
#r.ca <- projectRaster(r.ca,r, filename="r.ca", overwrite=T)
# Make a raster to crop the raster down to work with
#sdata.raster <- raster(sdata)
#crs(sdata.raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

# Potential way to extract information if the rasters were smaller
#r.vals <- zonal(r.ca, sdata, 'sum')
#-----End Scratch ----

# Calculate the total number of cells
number.of.cells <-ncell(r.us)

# Calculates the frequency of different unique cell types
land.use.frequncy<- freq(r.us)

# For Canada  Urban 21, 25, Cropland 51
# For US      urban 21, 22, 23, 24, 25, 26, 27, 31, 32, 33, Cropland 43

# Analyze the data

# manually fill in the data.frame from ncell and freq data
land.use.through.time <- data.frame(land.use.ag.raw = c(163336772, # 1974 99517599 - 44, 8905575 - 45 630904 - 41
                                                        163336772, # 1982 99517599 - 44, 8905575 - 45 630904 - 41
                                                        (64197588), # 1990
                                                        (64223227), # 2000
                                                        (63632703)), # 2010
                                    land.use.urb.raw = c((5607707 + 4354866 + 2111712 + 1687609 + 3647106 + 10816265 + 6038739 + 5467794 + 16987900 + 169237), # 1974
                                                         (5607707 + 4354866 + 2111712 + 1687609 + 3647106 + 10816265 + 6038739 + 5467794 + 16987900 + 169237), # 1982
                                                         (9563390 + 7065820), #1990
                                                         (12045687 + 7139952), #2000
                                                         (13429206 + 7602544)),#2010
                                    land.use.total = c(1381443000, # 1974
                                                       1381443000, # 1982
                                                       4167202410, #1990
                                                       4663759107, #2000
                                                       4167202410), #2010
                                    year = c(1974, 1982, 1992, 2002, 2012,
                                             1990, 2000, 2010))
                                    
temp.through.time.lm <- lm(ave.temp ~ year, data = temp.through.time)
summary(temp.through.time.lm)

