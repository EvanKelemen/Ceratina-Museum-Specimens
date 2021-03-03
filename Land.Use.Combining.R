#-------------------------------------------------------------------------------
# Combine Land Use Data and Morphological Data
#-------------------------------------------------------------------------------
# Makes data.frame morph.data
source("./morphologicaldata.R")
# Creates the function to combine temperature data to morphological data
source("./Combining.temperature.data.R")

#Double check that only sample with geographic locations are included
morph.data.lat.long <- subset(morph.data, !is.na(morph.data$Lat))

# Using the Climate Research Unit (university of east anglia)
morph.data.lat.long$average.temp <- combining.temperature.data("Yearly Average", morph.data.lat.long)
morph.data.lat.long$average.precip <- combining.temperature.data("Yearly Precip Average", morph.data.lat.long)
morph.data.lat.long$average.dtr <- combining.temperature.data("DTR", morph.data.lat.long)
morph.data.lat.long$frost.frequency <- combining.temperature.data("FRS", morph.data.lat.long)
morph.data.lat.long$wet.frequency <- combining.temperature.data("WET", morph.data.lat.long)
morph.data.lat.long$previous.summer <- combining.temperature.data("Previous Summer", morph.data.lat.long)
morph.data.lat.long$previous.precip <- combining.temperature.data("Previous Precip", morph.data.lat.long)

morph.data.lat.long <- subset(morph.data.lat.long, morph.data.lat.long$Specimen.Year > 1901)

# Preserves a version of Lat.Long for temperature analysis
morph.data.temp <- morph.data.lat.long

# To determine the agricultural landscape during development
morph.data.lat.long$Specimen.Year <- morph.data.lat.long$Specimen.Year - 1

# Identify states
us.states <- c("WI", "IN", "NH", "MO", "NC", "VA", "GA", "AL", "TN", "IL", "PA",
               "OH", "KY", "ME", "NY", "MI", "MA", "NJ","NE", "MD", "MN", "WV",
               "LA", "DE", "",   "SC", "KS", "AR", "VT", "FL", "OK", "IA", "MS")


# Subset morphological data into the bins according to land use data.
# US Data
sub_by_time_us <- function(data_frame, first_year, last_year){
  temp <- subset(morph.data.lat.long, morph.data.lat.long$Specimen.Year >= first_year &
                   morph.data.lat.long$Specimen.Year <= last_year &
                   morph.data.lat.long$State.Province %in% us.states)
  temp["Decade"] <- first_year
  return(temp)
}

morph.data.1974.1981 <- sub_by_time_us(morph.data.lat.long, 1974, 1981) 
morph.data.1982.1991 <- sub_by_time_us(morph.data.lat.long, 1982, 1991)
morph.data.1992.2001 <- sub_by_time_us(morph.data.lat.long, 1992, 2001)
morph.data.2002.2011 <- sub_by_time_us(morph.data.lat.long, 2002, 2011)
morph.data.2012.beyond <- sub_by_time_us(morph.data.lat.long, 2012, 2019)


#Write the data to use in ARCGIS (need to remove duplicate locations)
#excess_specimens <- function(decade_data){
#  unlist(apply(unique(decade_data["Location.State"]), MARGIN = 1, function(loc.s.y){
#    temp <- decade_data[ decade_data$Location.State == loc.s.y, ]
#    if(nrow(temp) > 2) {
#      temp[2:nrow(temp), "Fate"] <- "Remove"
#      temp[temp$Fate == "Remove", "Sample"]
#    } else{}
#  }))
#}

#for (time_slice in c(morph.data.1974.1981, morph.data.1982.1991, morph.data.1992.2001, 
#                    morph.data.2002.2011, morph.data.2012.beyond)){
#  temp <- subset(time_slice,! time_slice$Sample %in% excess_specimens(time_slice))
#  write.csv(temp, paste0("C:/Users/evank/OneDrive/Desktop/morphdata", time_slice$Decade[1], ".csv"))
#}


# Combine the data into on data.frame
morph.data.land.use <- rbind(morph.data.1974.1981, morph.data.1982.1991, 
morph.data.1992.2001, morph.data.2002.2011, morph.data.2012.beyond)

# Remove extra files
rm(morph.data.1974.1981, morph.data.1982.1991, 
   morph.data.1992.2001, morph.data.2002.2011, morph.data.2012.beyond)


# CANADA Data
sub_by_time_ca <- function(data_frame, first_year, last_year){
  temp <- subset(morph.data.lat.long, morph.data.lat.long$Specimen.Year >= first_year &
                   morph.data.lat.long$Specimen.Year <= last_year &
                   ! morph.data.lat.long$State.Province %in% us.states)
  temp["Decade"] <- first_year
  return(temp)
}

morph.data.1990.1999.ca <- sub_by_time_ca(morph.data.lat.long, 1990, 1999)
morph.data.2000.2009.ca <- sub_by_time_ca(morph.data.lat.long, 2000, 2009)
morph.data.2010.2019.ca <- sub_by_time_ca(morph.data.lat.long, 2010, 2019)


#for (time_slice in c(morph.data.1990.1999.ca, morph.data.2000.2009.ca, morph.data.2010.2019.ca)){
#  temp <- subset(time_slice,! time_slice$Sample %in% excess_specimens(time_slice))
#  write.csv(temp, paste0("C:/Users/evank/OneDrive/Desktop/morphdata", time_slice$Decade[1], ".csv"))
#}


# Combine the data into one dataframe
morph.data.land.use.ca <- rbind(morph.data.1990.1999.ca, morph.data.2000.2009.ca,
                                morph.data.2010.2019.ca)
# Remove extra dataframes
rm(morph.data.1990.1999.ca, morph.data.2000.2009.ca, morph.data.2010.2019.ca)


#>>> Combining US Data
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

# Calculate Land Use
us.land.use.data$Agriculture <- ((us.land.use.data[,16])/ us.land.use.data$sum) * 100
us.land.use.data$Agriculture_raw <- (us.land.use.data[,16])

us.land.use.data$Development <- ((us.land.use.data[, 5] +  us.land.use.data[,6] + us.land.use.data[,7] +  
                        us.land.use.data[,8] + us.land.use.data[,9] +  us.land.use.data[,10] + us.land.use.data[,11] 
                        + us.land.use.data[,12] + us.land.use.data[,13] + us.land.use.data[,14]
                      ) /us.land.use.data$sum) * 100
us.land.use.data$Development_raw <- (us.land.use.data[, 5] +  us.land.use.data[,6] + us.land.use.data[,7] +  
                                    us.land.use.data[,8] + us.land.use.data[,9] +  us.land.use.data[,10] + us.land.use.data[,11]
                                    + us.land.use.data[,12] + us.land.use.data[,13] + us.land.use.data[,14]) 

# When we remove NA measurement from morph.data, lost connecting sample to add lat and long
us.land.use.data$Sample <- as.character(us.land.use.data$Sample)
us.land.use.data[ us.land.use.data$Sample == "SEM01211279", "Sample"] <- "SEM01211324"
us.land.use.data[ us.land.use.data$Sample == "FSCA00091292", "Sample"] <- "FSCA00091291"

# Add lat and long to the data to link the two dataframe
for (lat_or_long in c("Lat", "Long")){
  us.land.use.data$Lat <- unlist(sapply(us.land.use.data[, "Sample"], function(sample.name){
    #Had to add due to some samples containing NA measurements removed from morph.data
    if (length(which(morph.data.land.use$Sample == sample.name)) == 0){
      NA
    } else{ morph.data.land.use[morph.data.land.use$Sample == sample.name, lat_or_long]}
  }))
}

# Unique Identifier for each buffer
us.land.use.data$Lat.Long.Decade <- paste0(us.land.use.data$Lat, us.land.use.data$Long, us.land.use.data$Decade)
morph.data.land.use$Lat.Long.Decade <- paste0(morph.data.land.use$Lat, morph.data.land.use$Long, morph.data.land.use$Decade)
# Link land data to morph data
for (land_use in c("Development", "Development_raw", "Agriculture", "Agriculture_raw")){
  morph.data.land.use[land_use] <- unlist(sapply(morph.data.land.use[, "Sample"], function(sample.name){
    us.land.use.data[us.land.use.data$Lat.Long.Decade == morph.data.land.use[morph.data.land.use$Sample == sample.name, "Lat.Long.Decade"], land_use][1] 
  }))
}

rm(us.land.use.data, land.use.temp)



#>> Combining Canada Data
ca.land.use.data <- read.csv("C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Land Use Specimens/Points_DecadeAfter/Output/500meterprevious/Canada/combined_permenant.csv")
ca.land.use.data <- ca.land.use.data[3:length(ca.land.use.data)]
ca.land.use.data[is.na(ca.land.use.data)] <- 0


land.use.ca.temp <- data.frame(value =c(#11, 
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


for (lat_or_long in c("Lat", "Long")){
  ca.land.use.data$Lat <- unlist(sapply(ca.land.use.data[, "Sample"], function(sample.name){
    morph.data.land.use.ca[morph.data.land.use.ca$Sample == sample.name, lat_or_long] 
  }))
}

# Unique Identifier for each buffer
ca.land.use.data$Lat.Long.Decade <- paste0(ca.land.use.data$Lat, ca.land.use.data$Long, ca.land.use.data$Decade)
morph.data.land.use.ca$Lat.Long.Decade <- paste0(morph.data.land.use.ca$Lat, morph.data.land.use.ca$Long, morph.data.land.use.ca$Decade)
# Link land data to morph data
for (land_use in c("Development", "Development_raw", "Agriculture", "Agriculture_raw")){
  morph.data.land.use.ca[ land_use] <- unlist(sapply(morph.data.land.use.ca[, "Sample"], function(sample.name){
    ca.land.use.data[ca.land.use.data$Lat.Long.Decade == morph.data.land.use.ca[morph.data.land.use.ca$Sample == sample.name, "Lat.Long.Decade"], land_use][1] 
  }))
}

rm(ca.land.use.data,land.use.ca.temp)


# Combine the Canadian and the US Data
morph.data.land.use <- rbind(morph.data.land.use, morph.data.land.use.ca)

rm(morph.data.land.use.ca, elevation, locations.with.specimens)

