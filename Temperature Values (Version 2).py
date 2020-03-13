# Creating the temperature values for morphological data
# Annual Mean Temperature
#Import system modules
import arcpy
from arcpy import env
from arcpy.sa import *

# Set environment settings
env.workspace = "C:/Users/evank/Documents/ArcGIS/Projects/Temperature (that opens)"
# Check out the ArcGIS Spatial Analyst extension license
arcpy.CheckOutExtension("Spatial")

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
    inRaster = ["air_ave_" + year_number + "_" + month_number for month_number in month]
    # Execute Times
    Year_Average = (Raster(inRaster[0]) + Raster(inRaster[1]) + Raster(inRaster[2]) + Raster(inRaster[3]) +
                    Raster(inRaster[4]) +
                    Raster(inRaster[5]) + Raster(inRaster[6]) + Raster(inRaster[7]) + Raster(inRaster[8]) +
                    Raster(inRaster[9]) + Raster(inRaster[10]) + Raster(inRaster[11])) / 12
    # Save the output
    arcpy.management.ProjectRaster(Year_Average,
                                   r"C:/Users/evank/Documents/ArcGIS/Projects/Temperature (that opens)/Temperature Variable/Yearly Average/Year_Average" + str(year_number) + ".tif",
                                   "GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]]",
                                   "NEAREST", "0.5 0.5", None, None,
                                   "GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]]",
                                   "NO_VERTICAL")
    # Load in points for that year
    import pandas as pd
    location_df = pd.read_csv("C:/Users/evank/Documents/ArcGIS/Projects/Temperature (that opens)/samples.summary.csv")
    temp = location_df[location_df['Year'] == int(year_number)]
    temp.to_csv("C:/Users/evank/Documents/ArcGIS/Projects/Temperature (that opens)/temp.csv", index=False)
    in_point_features = (r"C:\Users\evank\Documents\ArcGIS\Projects\Temperature (that opens)\Temperature (that opens).gdb\Specimen_Location_" + str(year_number))
    arcpy.management.XYTableToPoint(
        r"C:\Users\evank\Documents\ArcGIS\Projects\Temperature (that opens)\temp.csv",
        in_point_features, "Long", "Lat", None, "GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]];-400 -400 1000000000;-100000 10000;-100000 10000;8.98315284119521E-09;0.001;0.001;IsHighPrecision")
    out_point_features = (
                r"C:/Users/evank/Documents/ArcGIS/Projects/Temperature (that opens)/Temperature Variable/Yearly Average/Specimen_Location_" + str(
            year_number))
    ExtractValuesToPoints(in_point_features, "Year_Average" + str(year_number) + ".tif", out_point_features, "NONE", "Value_ONLY")
    print(year_number)
    #Remove extra rasters
    for month_number in month:
        arcpy.Delete_management("air_ave_" + year_number + "_" + month_number)

