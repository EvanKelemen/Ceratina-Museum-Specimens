# Import cdf raster files
#MakeNetCDFRasterLayer(in_netCDF_file, variable, x_dimension, y_dimension, out_raster_layer, {band_dimension},
#                      {dimension_values}, {value_selection_method}, {cell_registration})

# This imports a single raster (does not have time as dimension)
import arcpy
arcpy.MakeNetCDFRasterLayer_md("C:/Users/evank/Documents/ArcGIS/Projects/Temperature (that opens)/NA_NORM_8110_Monthly_netCDF/Tmax07.nc","Tmax07",
                         "easting", "northing", "Tmax07_Layer")

# This imports the single dimension of the raster (still can access the other dimensions)
arcpy.md.MakeNetCDFRasterLayer(r"C:\Users\evank\Documents\ArcGIS\Projects\Temperature (that opens)\air.mon.mean.v501.nc", "air", "lon", "lat", "air_Layer5",
                               '', "time '08/01/2017 12:00:00 AM'", "BY_VALUE", "CENTER")

