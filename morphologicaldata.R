#-------------------------------------------------------------------------------
# Analyze Morphological Data
#-------------------------------------------------------------------------------
#Load in the data
morph.data <- read.csv("C:/Users/evank/OneDrive/Desktop/Maps/Ceratina Data/Morphological Measurements.csv")
state_abbrivations <- read.csv("C:/Users/evank/OneDrive/Desktop/Maps/Ceratina Data/StatetoAbreviations.csv")

#Remove Extraneous rows
morph.data <- subset(morph.data, ! is.na(morph.data$Specimen.Year))
morph.data <- subset(morph.data, ! is.na(morph.data$Magnification)) 


#Remove "." "-" " " in samples and replace with "_"
name_replace <- function(remove_char, replace_with){
  as.character(unlist(lapply(morph.data["Sample"], FUN = function(sample){
  sub( remove_char, replace_with, morph.data[ morph.data$Sample == sample, "Sample"], perl = TRUE)
})))
}
morph.data$Sample <- name_replace("[.]", "_")
morph.data$Sample <- name_replace("-", "_")
morph.data$Sample <- name_replace(" ", "_")

#Check for duplicate names
#duplicate.names <- unlist(apply(morph.data[ "Sample"], MARGIN = 1, function(sample.name){
#  if( length(morph.data[ morph.data$Sample == sample.name, "Sample"]) > 1){
#    as.character(sample.name)  }
#}))

#>>>> Location Data <<<<<<
# Make all state names abbreviations
morph.data$State.Province <- as.character(morph.data$State.Province)
state_abbrivations$Abbreviation <- as.character(state_abbrivations$Abbreviation)
apply(state_abbrivations["State_Province"], MARGIN = 1, function(loc) {
  morph.data$State.Province <<- ifelse(morph.data$State.Province == loc, 
                                       state_abbrivations[state_abbrivations$State_Province == loc, "Abbreviation"],
                            morph.data$State.Province)
})
rm(state_abbrivations)

morph.data$Location.State <- paste(morph.data$Location, morph.data$State.Province, sep = "_")

# Create a data.frame of locations - indicating Lat and Long
locations.with.specimens <- data.frame(Location.State = unique(morph.data$Location.State))
#Add: Location
  for (info in c("Location", "State.Province", "Lat", "Long")){
  locations.with.specimens[info] <- apply(locations.with.specimens["Location.State"], MARGIN = 1, function(loc){
    morph.data[morph.data[ , "Location.State"] == loc, info][1]
  })
  }

    #Check for duplicate locations
  #duplicate.locations <- unlist(apply(locations.with.specimens[ "Lat"], MARGIN = 1, function(sample.name){
  #  if( length(locations.with.specimens[ locations.with.specimens$Lat == sample.name, "Lat"]) > 1) {
  #    as.character(sample.name)  }
  #}))
  #duplicate.locations.dataframe <- subset(locations.with.specimens, locations.with.specimens$Lat %in% duplicate.locations)
  
# Take locations that already appear in the dataframe and attach lat and longs to future events  
apply(morph.data["Location.State"], MARGIN = 1, function(loc) {
    morph.data$Lat <<- ifelse(morph.data$Location.State == loc, 
                              locations.with.specimens[locations.with.specimens$Location.State == loc, "Lat"],
                                        morph.data$Lat)
    morph.data$Long <<- ifelse(morph.data$Location.State == loc, 
                               locations.with.specimens[locations.with.specimens$Location.State == loc, "Long"],
                                         morph.data$Long)
  })


# >>>>>>>> Convert Measurements <<<<<<<<<<<<<
# Measurement Calibrations
measurement.calibrations <- data.frame(mag = c(1:5), micrometer = c(100, 100, 100, 97, 97),
                                      mm_EV_AR = c(9.9, 5.0, 3.3, 2.4, 1.9), #Evan and Arshdeep (Microscope 1)
                                      mm_YOU = c(9.8, 5.0, 3.5, 2.4, 1.6), # Yousaf (Microscope 2)
                                      mm_EQ = c(10, 4.9, 2.9, 2.4, 1.6)) # 	Evan-quarantined (Microscope 3)
for (mic in c("EV_AR", "YOU", "EQ")){
  measurement.calibrations[paste0(mic,"_conversion")] <- measurement.calibrations[, paste0("mm_", mic)]/
    measurement.calibrations$micrometer
}


# Converting measurements to mm based on who/where the sample was measured
apply(morph.data["Sample"], MARGIN = 1, function(sample) {
  if(morph.data[morph.data$Sample == sample, "Measured.By"] == "Yousaf"){
    conversion <- "YOU_conversion"
  } else if (morph.data[morph.data$Sample == sample, "Measured.By"] == "Evan-quarentined"){
    conversion <- "EQ_conversion"
  } else {
    conversion <- "EV_AR_conversion"
  }
  for (measure in c("Head.Width", "Intertegular.Width","Right.Wing", "Left.Wing")){
    morph.data[morph.data$Sample == sample, paste0(measure, ".mm")] <<- morph.data[morph.data$Sample == sample, grep(measure, colnames(morph.data))[1]] * 
      measurement.calibrations[measurement.calibrations$mag == morph.data[morph.data$Sample == sample, "Magnification"], conversion]
  }
})

# Remove the calibration dataframe as it are no longer needed
rm(measurement.calibrations)



#Identify the uniqe locations and years
morph.data$Location.State.Year <- paste0(morph.data$Location, morph.data$State.Province,
                                         morph.data$Specimen.Year)

# Create the Additonal Measurements
morph.data$head.to.wing <- morph.data$Right.Wing.mm/morph.data$Head.Width.mm
morph.data$head.to.intertegular <- morph.data$Intertegular.Width.mm/morph.data$Head.Width.mm
morph.data$wing.to.intertegular <- morph.data$Right.Wing.mm/morph.data$Intertegular.Width.mm
morph.data$asymmetry <- abs(morph.data$Right.Wing.mm - 
                              morph.data$Left.Wing.mm)

# Subset to equal only C. calcarata
morph.data <- subset(morph.data, morph.data$Species == "calcarata")

# Save a datafrme for ARCGIS of each specimen's location they year it developed (year prior to collection)
#morph.data_arcgis <- morph.data; morph.data_arcgis$Specimen.Year <- morph.data_arcgis$Specimen.Year - 1
#write.csv(morph.data_arcgis, "C:/Users/evank/OneDrive/Desktop/morphdata_arcgis.csv")


# Subset to include only those with complete measurements
morph.data <- subset(morph.data, !is.na(morph.data$Head.Width.mm))
#morph.data <- subset(morph.data, !is.na(morph.data$Intertegular.Width.mm))
#morph.data <- subset(morph.data, !is.na(morph.data$Right.Wing.mm))


# Adding Elevation
elevation <- read.csv("C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Elevation_Data.csv")

morph.data$Elevation <- unlist(sapply(morph.data[, "Sample"], function(sample.name){
  elevation[elevation$Sample == sample.name, "RASTERVALU"] 
}))

morph.data$EcoRegion <- unlist(sapply(morph.data[, "Sample"], function(sample.name){
  elevation[elevation$Sample == sample.name, "LEVEL2"] 
}))
