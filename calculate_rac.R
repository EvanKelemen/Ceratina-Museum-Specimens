#-------------------------------------------------------------------------------
# Calculate RAC
#-------------------------------------------------------------------------------


calculate_rac <- function(dataframe_used, model_used){
  
######> Based on Crase, Liedloff, & Wintle, 2012 Supplemental Code <############
#RAC model (autocovariate derived from residuals of model with environmental predictors)
xy <- cbind(dataframe_used$Long, dataframe_used$Lat) 
#Derive the autocovariate term from focal operation
#Set up a blank rasterfile
rast <- raster(ncol=78, nrow = 44, ymn = 26.4944, ymx = 48.3792, 
               xmn = -98.7056, xmx = -59.7547) 
res(rast) <- 1  
#Extract residuals from the model called "env_glm" and map them
xy_residuals <- cbind(xy, resid(model_used))
rast[cellFromXY(rast, xy_residuals)] <- xy_residuals[,3] 
#Calculate residuals autocovariate
#Focal operations: ngb is neighbourhood size, set to 3 by 3 cells; fun is function, 
#here the mean value within the defined neighbourhood
focal_rac_rast <- focal(rast, w = matrix(1,3,3), fun = mean, na.rm = TRUE) 
#Extract the values of the focal operation from “focal_rac_rast” rasterfile using the 
#co‐ordinates stored in “xy”
focal_rac_vect <- extract(focal_rac_rast, xy) 

#Add as a column to the data
dataframe_used$name_of_rac <- focal_rac_vect 
}