#  Extract value to point
ExtractValuesToPoints(in_point_features, in_raster, out_point_features, {interpolate_values}, {add_attributes})

import arcpy
from arcpy import env
from arcpy.sa import *
from arcpy.sa import *
env.workspace =  "C:/Users/evank/Documents/ArcGIS/Projects/Temperature (that opens)"
in_point_features = "Centroids"
in_raster = "Average_Summer_AdaptWest.tif"
out_point_features = "Centroids_temps"
ExtractValuesToPoints(in_point_features, in_raster, out_point_features)
