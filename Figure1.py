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

prior_conditions = (r"C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Temperature Variable CRU/Previous Summer/Previous_Summer" +
            str(1901) + ".tif")

post_conditions = (r"C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Temperature Variable CRU/Previous Summer/Previous_Summer" +
            str(2018) + ".tif")

Different_conditions = (Raster(post_conditions) - Raster(prior_conditions))

prior_raster = r"C:/Users/evank/Documents/ArcGIS/Projects/Temperature/ds948_landuse" + str(1974) + "/nwalt_landuse_" + str(1974) + "/lu" + str(1974) + "_050815"
#Add raster to the map using
#arcpy.MakeRasterLayer_management(prior_raster, "prior_raster")

post_raster = r"C:/Users/evank/Documents/ArcGIS/Projects/Temperature/ds948_landuse" + str(2012) + "/nwalt_landuse_" + str(2012) + "/lu" + str(2012) + "_050815"

Different_conditions_ag_us = (Raster(post_raster) - Raster(prior_raster))

prior_raster_ca = Raster(
    r"C:/Users/evank/Documents/ArcGIS/Projects/Temperature/CanadaLandcover" + str(1990) + "/eastern_ca" + str(
        1990) + ".tif")
post_raster_ca = Raster(
    r"C:/Users/evank/Documents/ArcGIS/Projects/Temperature/CanadaLandcover" + str(2010) + "/eastern_ca" + str(
        2010) + ".tif")

arcpy.MakeRasterLayer_management(post_raster_ca, "post_raster_ca")
Different_conditions_ag_ca = (Raster(post_raster) - Raster(prior_raster))