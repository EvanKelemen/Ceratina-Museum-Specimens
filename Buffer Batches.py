# Tabulates the area of the different raster attributes within the buffer

buffer = "samples2016_updated_11_8_19_"
raster = "Ontario Land Cover Compilation Version 2"
with arcpy.da.SearchCursor(buffer, "OID@") as cursor:
    for row in cursor:
        arcpy.MakeFeatureLayer_management(buffer, "tempFL", "\"OBJECTID\"=" + str(row[0]))
        rast = arcpy.sa.TabulateArea("tempFL", "OBJECTID", raster, "Value", r"C:/Users/evank/Documents/ArcGIS/Projects/Land Use - Ceratina/outTable/outputtables" + str(row[0]) + ".dbf")

# Condenses all output tables into one
import arcpy
arcpy.env.workspace = "C:/Users/evank/Documents/ArcGIS/Projects/Land Use - Ceratina"
tableList = arcpy.ListTables()
arcpy.Merge_management(tableList, "easy.dbf")
