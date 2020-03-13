# Creating the temperature values for morphological data
# Annual Mean Temperature

# LOADS RASTERS For each month and year
month = ["01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"]
year = ["2016", "2017"]

for year_number in year:
    for month_number in month:
        arcpy.md.MakeNetCDFRasterLayer(
            r"C:\Users\evank\Documents\ArcGIS\Projects\Temperature (that opens)\air.mon.mean.v501.nc",
            "air", "lon", "lat", "air_ave_" + year_number + "_" + month_number, '',
            "time '" + month_number + "/01/" + year_number + " 12:00:00 AM'", "BY_VALUE", "CENTER")


# AVERAGES THE RASTERS then deletes extra rasters
#Import system modules
import arcpy
from arcpy import env
from arcpy.sa import *

# Set environment settings
env.workspace = "C:/Users/evank/Documents/ArcGIS/Projects/Temperature (that opens)"
# Check out the ArcGIS Spatial Analyst extension license
arcpy.CheckOutExtension("Spatial")

for year_number in year:
    inRaster = ["air_ave_" + year_number + "_" + month_number for month_number in month]
    # Execute Times
    Year_Average = (Raster(inRaster[0]) + Raster(inRaster[1]) + Raster(inRaster[2]) + Raster(inRaster[3]) +
                    Raster(inRaster[4]) +
                    Raster(inRaster[5]) + Raster(inRaster[6]) + Raster(inRaster[7]) + Raster(inRaster[8]) +
                    Raster(inRaster[9]) + Raster(inRaster[10]) + Raster(inRaster[11])) / 12
    # Save the output
    # Save the output
    Year_Average.save("C:/Users/evank/Documents/ArcGIS/Projects/Temperature (that opens)/Temperature Variable/Yearly Average/Year_Average" + str(year_number) + ".tif")


#Remove extra rasters
for year_number in year:
    for month_number in month:
        arcpy.Delete_management("air_ave_" + year_number + "_" + month_number)


import arcpy
import os
from arcpy import env
from arcpy.sa import *
arcpy.env.workspace = r"C:/Users/evank/Documents/ArcGIS/Projects/Temperature (that opens)/Temperature Variable/Yearly Average/"
rasters = arcpy.ListRasters()
outFolder = r"C:/Users/evank/Documents/ArcGIS/Projects/Temperature (that opens)/Temperature Variable/Yearly Average"
# Loop through the list of rasters
for inRaster in rasters:
    # Set the outputname for each output to be the same as the input
    outRaster = outFolder + "/" + inRaster
    in_point_features = r"C:/Users/evank/Documents/ArcGIS/Projects/Temperature (that opens)/Temperature (that opens).gdb/Centroids"
    out_point_features = (r"C:/Users/evank/Documents/ArcGIS/Projects/Temperature (that opens)/Centroids_temps" + inRaster + ".shp")
    arcpy.MakeRasterLayer_management(outRaster, "temp")
    arcpy.management.ProjectRaster("temp",
                                   r"C:\Users\evank\Documents\ArcGIS\Projects\Temperature (that opens)\Temperature (that opens).gdb\temp_ProjectRast",
                                   "GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]]",
                                   "NEAREST", "0.5 0.5", None, None,
                                   "GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]]",
                                   "NO_VERTICAL")
    ExtractValuesToPoints(in_point_features, "temp_ProjectRast", out_point_features, "NONE", "Value_ONLY")
    #arcpy.Delete_management("temp")