# Tabulates the area of the different raster attributes within the buffer
#Import system modules
import arcpy
from arcpy.sa import *

years = ["1974", "1982", "1992", "2002", "2012"]
for yearbin in years:
    points = r"C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Land Use Specimens/Points_DecadeAfter/morph.data." + str(yearbin) + ".csv"
    out_point_features = str(r"C:\Users\evank\Documents\ArcGIS\Projects\Temperature\Temperature.gdb\land_use_points_" + str(yearbin))
    arcpy.management.XYTableToPoint(
        points,
        out_point_features,
        "Long", "Lat", None,
        "GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]];-400 -400 1000000000;-100000 10000;-100000 10000;8.98315284119521E-09;0.001;0.001;IsHighPrecision")
    raster = r"C:/Users/evank/Documents/ArcGIS/Projects/Temperature/ds948_landuse" + str(yearbin) + "/nwalt_landuse_" + str(yearbin) + "/lu" + str(yearbin) + "_050815"
    buffer = arcpy.analysis.Buffer(out_point_features,
                          r"C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Temperature.gdb/land_use" + str(yearbin) + "_Buffer",
                          "500 Meters", "FULL", "ROUND", "NONE", None, "PLANAR")
    with arcpy.da.SearchCursor(buffer, str("Sample")) as cursor:
        for row in cursor:
            print(row[0])
            val = str(row[0])
            field = arcpy.AddFieldDelimiters(buffer, "Sample")
            selection = "{field} = '{val}'".format(field=field, val=val)
            arcpy.MakeFeatureLayer_management(buffer, "NEW_SELECTION", selection)
            rast = arcpy.sa.TabulateArea("NEW_SELECTION", "Sample", raster, "Value", r"C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Land Use Specimens/Points_DecadeAfter/Output/" + str(yearbin) + "/output_" + str(yearbin) + str(row[0]) + ".dbf")
            arcpy.TableToTable_conversion(rast,
                                      r"C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Land Use Specimens/Points_DecadeAfter/Output/" + str(yearbin) + "/",
                                          "output_" + str(yearbin) + str(row[0]) + ".csv")
    #arcpy.Delete_management(points)
    arcpy.Delete_management(out_point_features)
    arcpy.Delete_management(buffer)

years = ["1990", "2000", "2010"]
for yearbin in years:
    points = r"C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Land Use Specimens/Canada/morph.data." + str(yearbin) + ".csv"
    out_point_features = str(r"C:\Users\evank\Documents\ArcGIS\Projects\Temperature\Temperature.gdb\land_use_points_" + str(yearbin))
    arcpy.management.XYTableToPoint(
        points,
        out_point_features, "Long", "Lat", None, "GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]];-400 -400 1000000000;-100000 10000;-100000 10000;8.98315284119521E-09;0.001;0.001;IsHighPrecision")
    raster = Raster(r"C:/Users/evank/Documents/ArcGIS/Projects/Temperature/CanadaLandcover" + str(yearbin) + "/Land_Use_" + str(yearbin) + ".tif")
    buffer = arcpy.analysis.Buffer(out_point_features,
                                   r"C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Temperature.gdb/land_use" + str(
                                       yearbin) + "_Buffer",
                                   "500 Meters", "FULL", "ROUND", "NONE", None, "PLANAR")
    with arcpy.da.SearchCursor(buffer, str("Sample")) as cursor:
        for row in cursor:
            print(row[0])
            val = str(row[0])
            field = arcpy.AddFieldDelimiters(buffer, "Sample")
            selection = "{field} = '{val}'".format(field=field, val=val)
            arcpy.MakeFeatureLayer_management(buffer, "NEW_SELECTION", selection)
            rast = arcpy.sa.TabulateArea("NEW_SELECTION", "Sample", raster, "Value",
                                         r"C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Land Use Specimens/Points_DecadeAfter/Output/" + str(yearbin) + "/output_" + str(yearbin) + str(row[0]) + ".dbf")
            arcpy.TableToTable_conversion(rast,
                                      r"C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Land Use Specimens/Points_DecadeAfter/Output/" + str(yearbin) + "/",
                                          "output_" + str(yearbin) + str(row[0]) + ".csv")
    #arcpy.Delete_management(points)
    arcpy.Delete_management(out_point_features)
    arcpy.Delete_management(buffer)
# Can't have dBase with a length of time longer than 10 character (most sample names are longer) (i.e. can't merge database)
#for yearbin in years:
#    arcpy.env.workspace = r"C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Land Use Specimens/Points_DecadeAfter/Output/" + str(yearbin)
#    tableList = arcpy.ListTables()
#    arcpy.TableToTable_conversion("combine" + str(yearbin) + ".dbf",
#                                  r"C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Land Use Specimens/Points_DecadeAfter/Output/500meter",
#                                  "combine" + str(yearbin) + ".csv")
#    arcpy.Merge_management(tableList, "combine" + str(yearbin) + ".dbf")
#    arcpy.TableToTable_conversion("combine" + str(yearbin) + ".dbf",
#                                  r"C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Land Use Specimens/Points_DecadeAfter/Output/500meter",
#                                  "combine" + str(yearbin) + ".csv")

#for yearbin in years:
#    arcpy.env.workspace = r"C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Land Use Specimens/Points_DecadeAfter/Output/" + str(yearbin)
#    tableList = arcpy.ListTables()
#    arcpy.Merge_management(tableList, "combine" + str(yearbin) + ".dbf")
#    arcpy.TableToTable_conversion("combine" + str(yearbin) + ".dbf",
#                                  r"C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Land Use Specimens/Points_DecadeAfter/Output/500meter",
#                                  "combine" + str(yearbin) + ".csv")