# Calculating the climate variables for morphological data
# Requires ArcGIS, and Spatial Analysis License
# Import system modules
import arcpy
from arcpy import env
from arcpy.sa import *
import pandas as pd
import numpy 

# Set environment settings
env.workspace = "C:/Users/evank/Documents/ArcGIS/Projects/Temperature"


def climate_variables(month, year, clim_var, locations, workspace, output_folder):
    """Calculates the climate variable at each specimen's location for a given time period"""
    for year_number in year:
        for month_number in month:
            arcpy.md.MakeNetCDFRasterLayer(
                workspace + "\cru_ts4.04.1901.2019.tmp.dat.nc\cru_ts4.04.1901.2019." + str(clim_var) + ".dat.nc",
                str(clim_var), "lon", "lat", str(clim_var) + str(year_number) + "_" + str(month_number), '',
                "time '" + str(month_number) + "/01/" + str(year_number) + " 12:00:00 AM'", "BY_VALUE", "CENTER")
        # AVERAGES THE RASTERS then deletes extra rasters
        inRaster = [str(clim_var) + str(year_number) + "_" + month_number for month_number in month]
        # Execute Times
        ave_var = (Raster(inRaster[0]) + Raster(inRaster[1]) + Raster(inRaster[2])) / 3
        # Save the output
        arcpy.management.ProjectRaster(ave_var,
                                       output_folder + str(clim_var) + "/" + str(clim_var) + str(year_number) + ".tif",
                                       "GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.25"\
                                       "7223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]]",
                                       "NEAREST", "0.5 0.5", None, None,
                                       "GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257"\
                                       "223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]]",
                                       "NO_VERTICAL")
        # Load in points for that year
        temp = locations[locations['Specimen.Year'] == int(year_number)]
        temp.to_csv(workspace + "/temp.csv", index=False)
        in_point_features = (workspace + "/Temperature.gdb/Specimen_Location_" + str(year_number))
        arcpy.management.XYTableToPoint(workspace + "/temp.csv", in_point_features, "Long", "Lat", None,
                                        "GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.25"\
                                        "7223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]];-400 -40"\
                                        "0 1000000000;-100000 10000;-100000 10000;8.98315284119521E-09;0.001;0.001;"\
                                        "IsHighPrecision")
        out_point_features = (output_folder + str(clim_var) + "/Specimen_Location_" + str(year_number))
        ExtractValuesToPoints(in_point_features, str(clim_var) + str(year_number) + ".tif", out_point_features, "NONE",
                              "Value_ONLY")
        # Export the saved file as a .csv file
        fc = (output_folder + str(clim_var) + "/Specimen_Location_" + str(year_number) + ".shp")
        nparr = arcpy.da.FeatureClassToNumPyArray(fc, ['FID', 'Sample', 'RASTERVALU'])
        pdarr = pd.DataFrame(nparr)
        pdarr.to_csv(output_folder + str(clim_var) + "/Specimen_Location_" + str(year_number) + ".csv")

        print(year_number)
        #Remove extra rasters
        for month_number in month:
            arcpy.Delete_management("temp" + str(year_number) + "_" + month_number)
        arcpy.Delete_management(str(clim_var) + str(year_number) + ".tif")
        arcpy.Delete_management(workspace + "/Temperature.gdb/Specimen_Location_" + str(year_number))
        arcpy.Delete_management("Specimen_Location_" + str(year_number))


# For the manuscript locations and year based on museum specimens and summer months
# Load in location data using pandas
location_df = pd.read_csv(env.workspace + "/morphdata_arcgis.csv", encoding="ISO-8859-1")

# Set month and year
summer_month = ["06", "07", "08"]
specimen_year = set(location_df["Specimen.Year"])
# Set where outputs will be saved
output = env.workspace + "Temperature Variable CRU/"

# Summer Temperatures
climate_variables(summer_month, specimen_year, "tmp", location_df, env.workspace, output)

# Summer precipitation
climate_variables(summer_month, specimen_year, "pre", location_df, env.workspace, output)
