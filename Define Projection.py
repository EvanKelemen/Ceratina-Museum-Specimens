# Defines the projection for a raster (In this case the adaptwest dataset


arcpy.DefineProjection_management(in_dataset, coord_sys)

PROJCS['NAD_1983_Lambert_Conformal_Conic',GEOGCS['GCS_North_American_1983',DATUM['D_North_American_1983',SPHEROID['GRS_1980',6378137.0,298.257222101]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Lambert_Conformal_Conic'],PARAMETER['False_Easting',0.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',-95.0],PARAMETER['Standard_Parallel_1',49.0],PARAMETER['Standard_Parallel_2',77.0],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0]]


# Corrected Define Projection
# Not this only workers for .asc and .tiff, not .nc
# For .nc, export the layer before running
arcpy.management.DefineProjection("Tmax06_Layer.tif", "PROJCS['NAD_1983_Lambert_Conformal_Conic',GEOGCS['GCS_North_American_1983',DATUM['D_North_American_1983',SPHEROID['GRS_1980',6378137.0,298.257222101]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Lambert_Conformal_Conic'],PARAMETER['False_Easting',0.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',-95.0],PARAMETER['Standard_Parallel_1',49.0],PARAMETER['Standard_Parallel_2',77.0],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0]]")