# Creating the temperature values for morphological data
# Annual Mean Temperature
#Import system modules
import arcpy
from arcpy import env
from arcpy.sa import *
from arcpy import env
from arcpy.sa import *
# Set environment settings
env.workspace = "C:/Users/evank/Documents/ArcGIS/Projects/Temperature"
# Check out the ArcGIS Spatial Analyst extension license
arcpy.CheckOutExtension("Spatial")

# Load in location data using pandas
import pandas as pd
location_df = pd.read_csv("C:/Users/evank/Documents/ArcGIS/Projects/Temperature/morphdata4721.csv",
                          encoding="ISO-8859-1")


# LOADS RASTERS For each month and year
month = ["06", "07", "08"]
year = set(location_df["Specimen.Year"])


for year_number in year:
    for month_number in month:
        arcpy.md.MakeNetCDFRasterLayer(
            r"C:\Users\evank\Documents\ArcGIS\Projects\Temperature\cru_ts4.04.1901.2019.tmp.dat.nc\cru_ts4.04.1901.2019.tmp.dat.nc",
            "tmp", "lon", "lat", "temp" + str(year_number) + "_" + str(month_number), '',
            "time '" + str(month_number) + "/01/" + str(year_number) + " 12:00:00 AM'", "BY_VALUE", "CENTER")
    # AVERAGES THE RASTERS then deletes extra rasters
    inRaster = ["temp" + str(year_number) + "_" + month_number for month_number in month]
    # Execute Times
    Previous_Summer = (Raster(inRaster[0]) + Raster(inRaster[1]) + Raster(inRaster[2])) / 3
    # Save the output
    arcpy.management.ProjectRaster(Previous_Summer,
                                   r"C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Temperature Variable CRU/Previous Summer/Previous_Summer" + str(year_number) + ".tif",
                                   "GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]]",
                                   "NEAREST", "0.5 0.5", None, None,
                                   "GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]]",
                                   "NO_VERTICAL")
    # Load in points for that year
    temp = location_df[location_df['Specimen.Year'] == int(year_number)]
    temp.to_csv("C:/Users/evank/Documents/ArcGIS/Projects/Temperature/temp.csv", index=False)
    in_point_features = (r"C:\Users\evank\Documents\ArcGIS\Projects\Temperature\Temperature.gdb\Specimen_Location_" + str(year_number))
    arcpy.management.XYTableToPoint(
        r"C:\Users\evank\Documents\ArcGIS\Projects\Temperature\temp.csv",
        in_point_features, "Long", "Lat", None, "GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]];-400 -400 1000000000;-100000 10000;-100000 10000;8.98315284119521E-09;0.001;0.001;IsHighPrecision")
    out_point_features = (
                r"C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Temperature Variable CRU/Previous Summer/Specimen_Location_" +
                str(year_number))
    ExtractValuesToPoints(in_point_features, "Previous_Summer" + str(year_number) + ".tif", out_point_features, "NONE", "Value_ONLY")
# Export the saved file as a .csv file
    import numpy
    fc = (r"C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Temperature Variable CRU/Previous Summer/Specimen_Location_" +
          str(year_number) + ".shp")
    nparr = arcpy.da.FeatureClassToNumPyArray(fc, ['FID', 'Sample', 'RASTERVALU'])
    pdarr = pd.DataFrame(nparr)
    pdarr.to_csv(
        r"C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Temperature Variable CRU/Previous Summer/Specimen_Location_" +
        str(year_number) + ".csv")

    print(year_number)
    #Remove extra rasters
    for month_number in month:
        arcpy.Delete_management("temp" + str(year_number) + "_" + month_number)
    arcpy.Delete_management("Previous_Summer" + str(year_number) + ".tif")
    arcpy.Delete_management(r"C:\Users\evank\Documents\ArcGIS\Projects\Temperature\Temperature.gdb\Specimen_Location_" + str(year_number))
    arcpy.Delete_management("Specimen_Location_" + str(year_number))
