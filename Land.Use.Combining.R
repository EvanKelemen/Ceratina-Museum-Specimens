#-------------------------------------------------------------------------------
# Combine Land Use Data and Morphological Data
#-------------------------------------------------------------------------------
# Makes data.frame morph.data
source("C:/Users/evank/OneDrive/Desktop/Maps/Ceratina-Maps/morphologicaldata.R")
# Creates the function to combine temperature data to morphological data
source("C:/Users/evank/OneDrive/Desktop/Maps/Ceratina-Maps/Combining.temperature.data.R")
source("C:/Users/evank/Documents/R/General Scripts/Rsquared.R")


morph.data.lat.long <- subset(morph.data, !is.na(morph.data$Lat))

#average.temp <- combining.temperature.data("Yearly Average", morph.data.lat.long)
#average.precip <- combining.temperature.data("Yearly Precip Average", morph.data.lat.long)

# Using UDEL data
#morph.data.lat.long$average.temp <- average.temp
# Remove any point not on a raster (Holly Shelter NC 1951 temp = 16.80833, precip = 7.976666)
#morph.data.lat.long <- subset(morph.data.lat.long, morph.data.lat.long$average.temp > -100 )
#morph.data.lat.long <- subset(morph.data.lat.long, morph.data.lat.long$Specimen.Year < 2018)

# Using the Climate Research Unit (university of east anglia)
# have to turn cell identity for Specimen Location 2018 and 2019 because it adds a leading zero
average.temp <- combining.temperature.data.cru("Yearly Average", morph.data.lat.long)
average.precip <- combining.temperature.data.cru("Yearly Precip Average", morph.data.lat.long)
average.dtr <- combining.temperature.data.cru("DTR", morph.data.lat.long)
frost.frequency <- combining.temperature.data.cru("FRS", morph.data.lat.long)
wet.frequency <- combining.temperature.data.cru("WET", morph.data.lat.long)
previous.summer <- combining.temperature.data.cru("Previous Summer", morph.data.lat.long)
previous.precip <- combining.temperature.data.cru("Previous Precip", morph.data.lat.long)

morph.data.lat.long$average.temp <- average.temp
morph.data.lat.long$average.precip <- average.precip
morph.data.lat.long$average.dtr <- average.dtr
morph.data.lat.long$frost.frequency <- frost.frequency
morph.data.lat.long$wet.frequency <- wet.frequency
morph.data.lat.long$previous.summer <- previous.summer
morph.data.lat.long$previous.precip <- previous.precip

morph.data.lat.long <- subset(morph.data.lat.long, morph.data.lat.long$Specimen.Year > 1901)

# Preserves a version of Lat.Long for temeprature analysis
morph.data.temp <- morph.data.lat.long

# To determine the agricultural landscape during development
morph.data.lat.long$Specimen.Year <- morph.data.lat.long$Specimen.Year - 1

# Identify states
us.states <- c("WI", "IN", "NH", "MO", "NC", "VA", "GA", "AL", "TN", "IL", "PA",
               "OH", "KY", "ME", "NY", "MI", "MA", "NJ","NE", "MD", "MN", "WV",
               "LA", "DE", "",   "SC", "KS", "AR", "VT", "FL", "OK", "IA", "MS")


# Subset morphological data into the bins according to land use data.
# US Data
morph.data.1974.1981 <- subset(morph.data.lat.long, morph.data.lat.long$Specimen.Year >= 1974 &
                                 morph.data.lat.long$Specimen.Year <= 1981 &
                                 morph.data.lat.long$State.Province %in% us.states)
morph.data.1974.1981$Decade <- 1974

morph.data.1982.1991 <- subset(morph.data.lat.long, morph.data.lat.long$Specimen.Year >= 1982 &
                                 morph.data.lat.long$Specimen.Year <= 1991 &
                                 morph.data.lat.long$State.Province %in% us.states)
morph.data.1982.1991$Decade <- 1982

morph.data.1992.2001 <- subset(morph.data.lat.long, morph.data.lat.long$Specimen.Year >= 1992 &
                                 morph.data.lat.long$Specimen.Year <= 2001 &
                                 morph.data.lat.long$State.Province %in% us.states)
morph.data.1992.2001$Decade <- 1992

morph.data.2002.2011 <- subset(morph.data.lat.long, morph.data.lat.long$Specimen.Year >= 2002 &
                                 morph.data.lat.long$Specimen.Year <= 2011 &
                                 morph.data.lat.long$State.Province %in% us.states)
morph.data.2002.2011$Decade <- 2002

morph.data.2012.beyond <- subset(morph.data.lat.long, morph.data.lat.long$Specimen.Year >= 2012 &
                                 morph.data.lat.long$State.Province %in% us.states)
morph.data.2012.beyond$Decade <- 2012



#excess.specimens <- unlist(apply(unique(morph.data.1974.1981["Location.State"]), MARGIN = 1, function(loc.s.y){
#  temp <- morph.data.1974.1981[ morph.data.1974.1981$Location.State == loc.s.y, ]
#  if(nrow(temp) > 2) {
#    temp[2:nrow(temp), "Fate"] <- "Remove"
#    temp[temp$Fate == "Remove", "Sample"]
#  } else{}
#}))
#morph.data.1974 <- subset(morph.data.1974.1981,! morph.data.1974.1981$Sample %in% excess.specimens)

#excess.specimens <- unlist(apply(unique(morph.data.1982.1991["Location.State"]), MARGIN = 1, function(loc.s.y){
#  temp <- morph.data.1982.1991[ morph.data.1982.1991$Location.State == loc.s.y, ]
#  if(nrow(temp) > 2) {
#    temp[2:nrow(temp), "Fate"] <- "Remove"
#    temp[temp$Fate == "Remove", "Sample"]
#  } else{}
#}))
#morph.data.1982 <- subset(morph.data.1982.1991,! morph.data.1982.1991$Sample %in% excess.specimens)

#excess.specimens <- unlist(apply(unique(morph.data.1992.2001["Location.State"]), MARGIN = 1, function(loc.s.y){
# temp <- morph.data.1992.2001[ morph.data.1992.2001$Location.State == loc.s.y, ]
#  if(nrow(temp) > 2) {
#    temp[2:nrow(temp), "Fate"] <- "Remove"
#    temp[temp$Fate == "Remove", "Sample"]
#  } else{}
#}))
#morph.data.1992 <- subset(morph.data.1992.2001,! morph.data.1992.2001$Sample %in% excess.specimens)

#excess.specimens <- unlist(apply(unique(morph.data.2002.2011["Location.State"]), MARGIN = 1, function(loc.s.y){
#  temp <- morph.data.2002.2011[ morph.data.2002.2011$Location.State == loc.s.y, ]
#  if(nrow(temp) > 2) {
#    temp[2:nrow(temp), "Fate"] <- "Remove"
#    temp[temp$Fate == "Remove", "Sample"]
#  } else{}
#}))
#morph.data.2002 <- subset(morph.data.2002.2011,! morph.data.2002.2011$Sample %in% excess.specimens)

#excess.specimens <- unlist(apply(unique(morph.data.2012.beyond["Location.State"]), MARGIN = 1, function(loc.s.y){
#  temp <- morph.data.2012.beyond[ morph.data.2012.beyond$Location.State == loc.s.y, ]
#  if(nrow(temp) > 2) {
#    temp[2:nrow(temp), "Fate"] <- "Remove"
#    temp[temp$Fate == "Remove", "Sample"]
#  } else{}
#}))
#morph.data.2012 <- subset(morph.data.2012.beyond,! morph.data.2012.beyond$Sample %in% excess.specimens)


#Write the data to use in ARCGIS
#write.csv(morph.data.1974, "C:/Users/evank/OneDrive/Desktop/morph.data.previous1974.csv")
#write.csv(morph.data.1982, "C:/Users/evank/OneDrive/Desktop/morph.data.previous1982.csv")
#write.csv(morph.data.1992, "C:/Users/evank/OneDrive/Desktop/morph.data.previous1992.csv")
#write.csv(morph.data.2002, "C:/Users/evank/OneDrive/Desktop/morph.data.previous2002.csv")
#write.csv(morph.data.2012, "C:/Users/evank/OneDrive/Desktop/morph.data.previous2012.csv")

# Combine the data into on data.frame
morph.data.land.use <- rbind(morph.data.1974.1981, morph.data.1982.1991, 
morph.data.1992.2001, morph.data.2002.2011, morph.data.2012.beyond)

# Remove extra files
rm(morph.data.1974.1981, morph.data.1982.1991, 
   morph.data.1992.2001, morph.data.2002.2011, morph.data.2012.beyond)


# CANADA Data
morph.data.1990.1999.ca <- subset(morph.data.lat.long, morph.data.lat.long$Specimen.Year >= 1990 &
                                 morph.data.lat.long$Specimen.Year <= 1999 &
                                 !morph.data.lat.long$State.Province %in% us.states)
morph.data.1990.1999.ca$Decade <- 1990

morph.data.2000.2009.ca <- subset(morph.data.lat.long, morph.data.lat.long$Specimen.Year >= 2000 &
                                 morph.data.lat.long$Specimen.Year <= 2009 &
                                 !morph.data.lat.long$State.Province %in% us.states)
morph.data.2000.2009.ca$Decade <- 2000

morph.data.2010.2019.ca <- subset(morph.data.lat.long, morph.data.lat.long$Specimen.Year >= 2010 &
                                 morph.data.lat.long$Specimen.Year <= 2019 &
                                 !morph.data.lat.long$State.Province %in% us.states)
morph.data.2010.2019.ca$Decade <- 2010



#excess.specimens <- unlist(apply(unique(morph.data.2000.2009.ca["Location.State"]), MARGIN = 1, function(loc.s.y){
#  temp <- morph.data.2000.2009.ca[ morph.data.2000.2009.ca$Location.State == loc.s.y, ]
#  if(nrow(temp) > 2) {
#    temp[2:nrow(temp), "Fate"] <- "Remove"
#    temp[temp$Fate == "Remove", "Sample"]
#  } else{}
#}))
#morph.data.2000 <- subset(morph.data.2000.2009.ca,! morph.data.2000.2009.ca$Sample %in% excess.specimens)

#excess.specimens <- unlist(apply(unique(morph.data.1990.1999.ca["Location.State"]), MARGIN = 1, function(loc.s.y){
#  temp <- morph.data.1990.1999.ca[ morph.data.1990.1999.ca$Location.State == loc.s.y, ]
#  if(nrow(temp) > 2) {
#    temp[2:nrow(temp), "Fate"] <- "Remove"
#    temp[temp$Fate == "Remove", "Sample"]
#  } else{}
#}))
#morph.data.1990 <- subset(morph.data.1990.1999.ca,! morph.data.1990.1999.ca$Sample %in% excess.specimens)

#excess.specimens <- unlist(apply(unique(morph.data.2010.2019.ca["Location.State"]), MARGIN = 1, function(loc.s.y){
#  temp <- morph.data.2010.2019.ca[ morph.data.2010.2019.ca$Location.State == loc.s.y, ]
#  if(nrow(temp) > 2) {
#    temp[2:nrow(temp), "Fate"] <- "Remove"
#    temp[temp$Fate == "Remove", "Sample"]
#  } else{}
#}))
#morph.data.2010 <- subset(morph.data.2010.2019.ca,! morph.data.2010.2019.ca$Sample %in% excess.specimens)

# Write the Data for ARCGIS
#write.csv(morph.data.1990, "C:/Users/evank/OneDrive/Desktop/morph.data.previous1990.csv")
#write.csv(morph.data.2000, "C:/Users/evank/OneDrive/Desktop/morph.data.previous2000.csv")
#write.csv(morph.data.2010, "C:/Users/evank/OneDrive/Desktop/morph.data.previous2010.csv")

# Combine the data into one dataframe
morph.data.land.use.ca <- rbind(morph.data.1990.1999.ca, morph.data.2000.2009.ca,
                                morph.data.2010.2019.ca)
# Remove extra dataframes
rm(morph.data.1990.1999.ca, morph.data.2000.2009.ca, morph.data.2010.2019.ca)






# Function to combine data if land use of a single point (not a buffer)
#----
#combining.landuse.data <- function(variable.folder){
#  temp.files <- list.files(path = paste0("C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Land Use Specimens/",
#                                         variable.folder), pattern = "*.csv")
#  temp <- do.call(rbind, lapply(temp.files, function(file.name){
#    read.csv(paste0("C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Land Use Specimens/",
#                    variable.folder,"/", file.name))
#  }))
#  land.use.temp <- data.frame(value =
#  c(11, 12, 21, 22, 23, 24, 25, 26, 27, 31, 32, 33, 41, 42, 43, 44, 45, 50, 60),
#  use.type = 
#  c("Water", "Wetlands", "Developed, Major Transportation", "Developed, Commercial/Services", 
#    "Developed, Industrial/Military", "Developed, Recreation", "Developed, Residential, High Density",
#    "Developed, Residential, Low-Medium Density", "Developed, Other", "Semi-Developed, Urban Interface High",
#    "Semi-Developed, Urban Interface Low-Medium", "Semi-Developed Anthropogenic Other", 
#    "Production, Mining/Extraction", "Production, Timber/Forest cutting", "Production, Crops",
#    "Production, Pasture/Hay", "Production, Grazing Potential", "Low Use", "Very Low Use, Conservation"),
#  use.type.simplified = 
#    c("Wetlands", "Wetlands", "Developed", "Developed", 
#      "Developed", "Developed", "Developed",
#      "Developed", "Developed", "Semi-Developed",
#      "Semi-Developed", "Semi-Developed", 
#      "Production, Mining/Extraction", "Production, Timber/Forest cutting", "Production",
#      "Production", "Production", "Low Use", "Very Low Use, Conservation"))
  
  
#  temp$land.use <- unlist(sapply(temp[, "Sample"], function(sample.name){
#    land.use.temp[land.use.temp$value == temp[ temp$Sample == as.character(sample.name), "RASTERVALU"], "use.type.simplified"]
#  }))
#  
#  temp
#}
#----



# Combining US Data

# Load in Data
us.land.use.data <- read.csv("C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Land Use Specimens/Points_DecadeAfter/Output/500meterprevious/US/combined_permenant.csv")
us.land.use.data <- us.land.use.data[3:length(us.land.use.data)]
us.land.use.data[is.na(us.land.use.data)] <- 0

# Create a temporary file to convert land use values into type
land.use.temp <- data.frame(value =
                            c(11, 12, 21, 22, 23, 24, 25, 26, 27, 31, 32, 33, 41, 43, 44, 45, 50, 60), #42,
                            use.type = 
                            c("Water", "Wetlands", "Developed, Major Transportation", "Developed, Commercial/Services", 
                              "Developed, Industrial/Military", "Developed, Recreation", "Developed, Residential, High Density",
                              "Developed, Residential, Low-Medium Density", "Developed, Other", "Semi-Developed, Urban Interface High",
                              "Semi-Developed, Urban Interface Low-Medium", "Semi-Developed Anthropogenic Other", 
                              "Production, Mining/Extraction",  "Production, Crops", #"Production, Timber/Forest cutting",
                              "Production, Pasture/Hay", "Production, Grazing Potential", "Low Use", "Very Low Use, Conservation"),
                            use.type.simplified = 
                            c("Wetlands", "Wetlands", "Developed", "Developed",                                 "Developed", "Developed", "Developed",
                              "Developed", "Developed", "Semi-Developed",
                              "Semi-Developed", "Semi-Developed", 
                              "Production, Mining/Extraction",  "Production", #"Production, Timber/Forest cutting",
                              "Production", "Production", "Low Use", "Very Low Use, Conservation"))
  
# Create a list of names bases on land use types 
column.names <- as.character(unlist(lapply(land.use.temp, FUN = function(value.num){
  land.use.temp[ land.use.temp$value == value.num, "use.type"]
})))
# Rename the columns
colnames(us.land.use.data) <- c("Decade", "Sample", column.names)


# To makes sure all of the buffers have the same size
us.land.use.data$sum <- us.land.use.data[, 3] +  us.land.use.data[,4] + us.land.use.data[,5] +  
  us.land.use.data[,6] + us.land.use.data[,7] +  us.land.use.data[,8] + us.land.use.data[,9] +  
  us.land.use.data[,10] + us.land.use.data[,11] +  us.land.use.data[,12] + us.land.use.data[,13] +  
  us.land.use.data[,14] + us.land.use.data[,15] +  us.land.use.data[,16] + us.land.use.data[,17] +
  us.land.use.data[,18] + us.land.use.data[,19] + us.land.use.data[,20] 


us.land.use.data$Agriculture <- ((#us.land.use.data[,15]  +
                                    us.land.use.data[,16])/ #+ 
                                    #us.land.use.data[,17]) /# + us.land.use.data[,18])/
                                   us.land.use.data$sum) * 100

us.land.use.data$Agriculture_raw <- (#us.land.use.data[,15] +  
                                       us.land.use.data[,16])# + 
                                   # us.land.use.data[,17]) # + us.land.use.data[,18])

us.land.use.data$Development <- ((us.land.use.data[, 5] +  us.land.use.data[,6] + us.land.use.data[,7] +  
                        us.land.use.data[,8] + us.land.use.data[,9] +  us.land.use.data[,10] + us.land.use.data[,11] 
                        + us.land.use.data[,12] + us.land.use.data[,13] + us.land.use.data[,14]
                      ) /us.land.use.data$sum) * 100
us.land.use.data$Development_raw <- (us.land.use.data[, 5] +  us.land.use.data[,6] + us.land.use.data[,7] +  
                                    us.land.use.data[,8] + us.land.use.data[,9] +  us.land.use.data[,10] + us.land.use.data[,11]
                                    + us.land.use.data[,12] + us.land.use.data[,13] + us.land.use.data[,14]) 

# When we remove NA measurement, lost connecting sample to add lat and long
us.land.use.data$Sample <- as.character(us.land.use.data$Sample)
us.land.use.data[ us.land.use.data$Sample == "SEM01211279", "Sample"] <- "SEM01211324"
us.land.use.data[ us.land.use.data$Sample == "FSCA00091292", "Sample"] <- "FSCA00091291"

# Add lat and long to the data to link the two dataframe
us.land.use.data$Lat <- unlist(sapply(us.land.use.data[, "Sample"], function(sample.name){
  #Had to add due to some samples containing NA measurements removed from morph.data
  if (length(which(morph.data.land.use$Sample == sample.name)) == 0){
    NA
  } else{ morph.data.land.use[morph.data.land.use$Sample == sample.name, "Lat"]}
}))
us.land.use.data$Long <- unlist(sapply(us.land.use.data[, "Sample"], function(sample.name){
  if (length(which(morph.data.land.use$Sample == sample.name)) == 0){
    NA
  } else{ morph.data.land.use[morph.data.land.use$Sample == sample.name, "Long"]}
#  if (length(which(morph.data.land.use$Sample == sample.name)) == 0) {
#    as.character(sample.name)
#  } else{}
}))
us.land.use.data$Lat.Long.Decade <- paste0(us.land.use.data$Lat, us.land.use.data$Long, us.land.use.data$Decade)

morph.data.land.use$Lat.Long.Decade <- paste0(morph.data.land.use$Lat, morph.data.land.use$Long, morph.data.land.use$Decade)


morph.data.land.use$Development <- unlist(sapply(morph.data.land.use[, "Sample"], function(sample.name){
  us.land.use.data[us.land.use.data$Lat.Long.Decade == morph.data.land.use[morph.data.land.use$Sample == sample.name, "Lat.Long.Decade"], "Development"][1] 
}))
morph.data.land.use$Development_raw <- unlist(sapply(morph.data.land.use[, "Sample"], function(sample.name){
  us.land.use.data[us.land.use.data$Lat.Long.Decade == morph.data.land.use[morph.data.land.use$Sample == sample.name, "Lat.Long.Decade"], "Development_raw"][1] 
}))

morph.data.land.use$Agriculture <- unlist(sapply(morph.data.land.use[, "Sample"], function(sample.name){
  us.land.use.data[us.land.use.data$Lat.Long.Decade == morph.data.land.use[morph.data.land.use$Sample == sample.name, "Lat.Long.Decade"], "Agriculture"][1] 
}))
morph.data.land.use$Agriculture_raw <- unlist(sapply(morph.data.land.use[, "Sample"], function(sample.name){
  us.land.use.data[us.land.use.data$Lat.Long.Decade == morph.data.land.use[morph.data.land.use$Sample == sample.name, "Lat.Long.Decade"], "Agriculture_raw"][1] 
}))

rm(us.land.use.data, land.use.temp)


# Combining Canada Data

ca.land.use.data <- read.csv("C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Land Use Specimens/Points_DecadeAfter/Output/500meterprevious/Canada/combined_permenant.csv")
ca.land.use.data <- ca.land.use.data[3:length(ca.land.use.data)]
ca.land.use.data[is.na(ca.land.use.data)] <- 0


land.use.ca.temp <- data.frame(value =
                                    c(#11, 
                                      21, 25, 31, 41, 42, 45, 46, 51, #61,62, 
                                      71, 73, #74, 
                                      91), 
                                  use.type = 
                                    c(#"Unclassified", 
                                    "Settlement", "Roads", "Water", "Forest", "Forest Wetland", "Trees", "Treed Wetland",
                                    "Cropland", #"Grassland Managed", "Unmanaged", 
                                    "Wetland", "Wetland Shrub", #"Wetland Herb", 
                                    "Other land" ))


column.names <- as.character(unlist(lapply(land.use.ca.temp, FUN = function(value.num){
  land.use.ca.temp[ land.use.ca.temp$value == value.num, "use.type"]
})))

colnames(ca.land.use.data) <- c("Decade", "Sample", column.names)


# To makes sure all of the buffers have the same size
ca.land.use.data$sum <- ca.land.use.data[, 3] +  ca.land.use.data[,4] + ca.land.use.data[,5] +  
  ca.land.use.data[,6] + ca.land.use.data[,7] +  ca.land.use.data[,8] + ca.land.use.data[,9] +  
  ca.land.use.data[,10] + ca.land.use.data[,11] +  ca.land.use.data[,12] + ca.land.use.data[,13]


ca.land.use.data$Agriculture <- ((ca.land.use.data[,10]) /ca.land.use.data$sum) * 100
ca.land.use.data$Agriculture_raw <- (ca.land.use.data[,10])

ca.land.use.data$Development <- ((ca.land.use.data[, 3] +  ca.land.use.data[,4]) /ca.land.use.data$sum) * 100
ca.land.use.data$Development_raw <- (ca.land.use.data[, 3] +  ca.land.use.data[,4])

ca.land.use.data$Lat <- unlist(sapply(ca.land.use.data[, "Sample"], function(sample.name){
  morph.data.land.use.ca[morph.data.land.use.ca$Sample == sample.name, "Lat"] 
}))
ca.land.use.data$Long <- unlist(sapply(ca.land.use.data[, "Sample"], function(sample.name){
  morph.data.land.use.ca[morph.data.land.use.ca$Sample == sample.name, "Long"] 
}))
ca.land.use.data$Lat.Long.Decade <- paste0(ca.land.use.data$Lat, ca.land.use.data$Long, ca.land.use.data$Decade)

morph.data.land.use.ca$Lat.Long.Decade <- paste0(morph.data.land.use.ca$Lat, morph.data.land.use.ca$Long, morph.data.land.use.ca$Decade)


morph.data.land.use.ca$Development <- unlist(sapply(morph.data.land.use.ca[, "Sample"], function(sample.name){
  ca.land.use.data[ca.land.use.data$Lat.Long.Decade == morph.data.land.use.ca[morph.data.land.use.ca$Sample == sample.name, "Lat.Long.Decade"], "Development"][1] 
}))
morph.data.land.use.ca$Development_raw <- unlist(sapply(morph.data.land.use.ca[, "Sample"], function(sample.name){
  ca.land.use.data[ca.land.use.data$Lat.Long.Decade == morph.data.land.use.ca[morph.data.land.use.ca$Sample == sample.name, "Lat.Long.Decade"], "Development_raw"][1] 
}))

morph.data.land.use.ca$Agriculture <- unlist(sapply(morph.data.land.use.ca[, "Sample"], function(sample.name){
  ca.land.use.data[ca.land.use.data$Lat.Long.Decade == morph.data.land.use.ca[morph.data.land.use.ca$Sample == sample.name, "Lat.Long.Decade"], "Agriculture"][1] 
}))
morph.data.land.use.ca$Agriculture_raw <- unlist(sapply(morph.data.land.use.ca[, "Sample"], function(sample.name){
  ca.land.use.data[ca.land.use.data$Lat.Long.Decade == morph.data.land.use.ca[morph.data.land.use.ca$Sample == sample.name, "Lat.Long.Decade"], "Agriculture_raw"][1] 
}))

rm(ca.land.use.data,land.use.ca.temp)


# Combine the Canadian and the US Data
morph.data.land.use <- rbind(morph.data.land.use, morph.data.land.use.ca)

rm(morph.data.land.use.ca, elevation, locations.with.specimens)

