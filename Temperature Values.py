# Creating the temperature values for morphological data
# Annual Mean Temperature
month = ["01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"]
year = "2017"

for month_number in month:
    arcpy.md.MakeNetCDFRasterLayer(r"C:\Users\evank\Documents\ArcGIS\Projects\Temperature (that opens)\air.mon.mean.v501.nc",
                                   "air", "lon", "lat", "air_ave_" + year + "_" + month_number, '',
                                   "time '" + month_number + "/01/" + year + " 12:00:00 AM'", "BY_VALUE", "CENTER")

inRaster = ["air_ave_2017_" + month_number for month_number in month]

#Import system modules
import arcpy
from arcpy import env
from arcpy.sa import *

# Set environment settings
env.workspace = "C:/Users/evank/Documents/ArcGIS/Projects/Temperature (that opens)"
# Check out the ArcGIS Spatial Analyst extension license
arcpy.CheckOutExtension("Spatial")

# Execute Times
Year_Average = (Raster(inRaster[0]) + Raster(inRaster[1]) + Raster(inRaster[2]) + Raster(inRaster[3]) + Raster(inRaster[4]) +
                Raster(inRaster[5]) + Raster(inRaster[6]) + Raster(inRaster[7]) + Raster(inRaster[8]) +
                Raster(inRaster[9]) + Raster(inRaster[10]) + Raster(inRaster[11]))/12

# Save the output
Year_Average.save("C:/Users/evank/Documents/ArcGIS/Projects/Temperature (that opens)/Year_Average")


# Maximum Temperature of the Warmest Month
# Minimum Temperature of the Coldest Month
# Isothermality
# Temperature Seasonality
