import arcpy
mxd = arcpy.mapping.MapDocument(r"C:\Project\Project.mxd")
df = arcpy.mapping.ListDataFrames(mxd, "Transportation")[0]
arcpy.mapping.ExportToTIFF(mxd, r"C:\Project\Output\ProjectDataFrame.tif", df,
                           df_export_width=1600,
                           df_export_height=1200,
                           geoTIFF_tags=True)
del mxd