# Create a raster by averaging multiple rasters

RasterCalculator(expression, output_raster)




import arcpy
from arcpy import env
from arcpy.sa import *
RasterCalculator((Raster(r"Tmax06_Layer" ) + Raster(r"Tmax07_Layer" ) + Raster(r"Tmax08_Layer" ) )/3, "Summer_Average_Adapt_West")


#Import system modules
import arcpy
from arcpy import env
from arcpy.sa import *

# Set environment settings
env.workspace = "C:/Users/evank/Documents/ArcGIS/Projects/Temperature (that opens)"

# Set local variables
inRaster6 = Raster("Tmax06_Layer")
inRaster7 = Raster("Tmax07_Layer")
inRaster8 = Raster("Tmax08_Layer")

# Check out the ArcGIS Spatial Analyst extension license
arcpy.CheckOutExtension("Spatial")

# Execute Times
outTimes = (inRaster6 + inRaster7 + inRaster8)/3

# Save the output
outTimes.save("C:/Users/evank/Documents/ArcGIS/Projects/Temperature (that opens)/Summer_Average_Adapt_West")