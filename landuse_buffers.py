# Tabulates the area of the different raster attributes within the buffer
#Import system modules
import arcpy
from arcpy.sa import *


def landuse_buffers(years, iter_points, iter_raster, buffer_size, workspace, ca=False):
    """Calculates the the land use within a given size buffer around location at a given year"""
    for yearbin in years:
        points = iter_points.replace("year", str(yearbin))
        raster = iter_raster.replace("year", str(yearbin))
        if ca:
            Raster(raster)
        out_point_features = str(workspace + "\Temperature.gdb\land_use_points_" + str(yearbin))
        arcpy.management.XYTableToPoint(points, out_point_features, "Long", "Lat", None,
                                        "GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.25"\
                                        "7223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]];-400 -40"\
                                        "0 1000000000;-100000 10000;-100000 10000;8.98315284119521E-09;0.001;0.001;"\
                                        "IsHighPrecision")
        buffer = arcpy.analysis.Buffer(out_point_features, workspace + "/Temperature.gdb/land_use"\
                                       + str(yearbin) + "_Buffer", buffer_size, "FULL", "ROUND", "NONE", None, "PLANAR")
        with arcpy.da.SearchCursor(buffer, str("Sample")) as cursor:
            for row in cursor:
                print(row[0])
                val = str(row[0])
                field = arcpy.AddFieldDelimiters(buffer, "Sample")
                selection = "{field} = '{val}'".format(field=field, val=val)
                arcpy.MakeFeatureLayer_management(buffer, "NEW_SELECTION", selection)
                rast = arcpy.sa.TabulateArea("NEW_SELECTION", "Sample", raster, "Value",
                                             workspace + "/Land Use Specimens/Points_DecadeAfter/Output/" + str(
                                                 yearbin) + "/output_" + str(yearbin) + str(row[0]) + ".dbf")
                arcpy.TableToTable_conversion(rast,
                                              workspace + "/Land Use Specimens/Points_DecadeAfter/Output/" + str(
                                                  yearbin) + "/", "output_" + str(yearbin) + str(row[0]) + ".csv")
        arcpy.Delete_management(out_point_features)
        arcpy.Delete_management(buffer)


# ID the workspace
current_workspace = "C:/Users/evank/Documents/ArcGIS/Projects/Temperature"

# For the US Data (note year in points and raster will be replaced with specific decade)
us_years = ["1974", "1982", "1992", "2002", "2012"]
us_points = current_workspace + "/Land Use Specimens/Points_DecadeAfter/morph.data.previous" + "year" + ".csv"
us_raster = current_workspace + "/ds948_landuse" + "year" + "/nwalt_landuse_" + "year" + "/lu" + "year" + "_050815"

# Calculate US land use
landuse_buffers(us_years, us_points, us_raster, "500_meters", current_workspace)


# For the CA Data (note year in points and raster will be replaced with specific decade)
ca_years = ["1990", "2000", "2010"]
ca_points = current_workspace + "/Land Use Specimens/Canada/morph.data.previous" + str(yearbin) + ".csv"
ca_raster = current_workspace + "/CanadaLandcover" + "year" + "/Land_Use_" + "year" + ".tif"

# Calculate CA land use
landuse_buffers(ca_years, ca_points, ca_raster, "500_meters", current_workspace, ca=True)
