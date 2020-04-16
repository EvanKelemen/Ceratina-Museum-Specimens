# Make sure pandas is loaded
import pandas as pd

# Read in the survey CSV
location_df = pd.read_csv("C:/Users/evank/Documents/ArcGIS/Projects/Temperature (that opens)/samples.summary.csv")
location_df

location_df[location_df['Year'] == 2019]
arcpy.management.XYTableToPoint(r"C:\Users\evank\Documents\ArcGIS\Projects\Temperature (that opens)\samples.summary.csv", r"C:\Users\evank\Documents\ArcGIS\Projects\Temperature (that opens)\Temperature (that opens).gdb\samples_XYTableToPoint", "Long", "Lat", None, "GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]];-400 -400 1000000000;-100000 10000;-100000 10000;8.98315284119521E-09;0.001;0.001;IsHighPrecision")