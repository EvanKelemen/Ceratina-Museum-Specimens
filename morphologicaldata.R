#-------------------------------------------------------------------------------
# Analyze Morphological Data
#-------------------------------------------------------------------------------
#Load in the data
morph.data <- read.csv("C:/Users/evank/OneDrive/Desktop/Morphological Measurements.csv")
state_abbrivations <- read.csv("C:/Users/evank/OneDrive/Desktop/Maps/Ceratina Data/StatetoAbreviations.csv")

#Remove Extraneous rows
morph.data <- subset(morph.data, ! is.na(morph.data$Specimen.Year))
morph.data <- subset(morph.data, ! is.na(morph.data$Magnification)) 
#morph.data <- subset(morph.data, morph.data$Specimen.Year > 2016)

#Remove "." "-" "" in samples and replace with "_"
morph.data$Sample <- as.character(unlist(lapply(morph.data["Sample"], FUN = function(sample){
  sub( "[.]", "_", morph.data[ morph.data$Sample == sample, "Sample"], perl = TRUE)
})))
morph.data$Sample <- as.character(unlist(lapply(morph.data["Sample"], FUN = function(sample){
  sub("-", "_", morph.data[ morph.data$Sample == sample, "Sample"], perl = TRUE)
})))
morph.data$Sample <- as.character(unlist(lapply(morph.data["Sample"], FUN = function(sample){
  sub(" ", "_", morph.data[ morph.data$Sample == sample, "Sample"], perl = TRUE)
})))
morph.data$Sample <- as.character(unlist(lapply(morph.data["Sample"], FUN = function(sample){
  sub(" ", "_", morph.data[ morph.data$Sample == sample, "Sample"], perl = TRUE)
})))

#>>>> Location Data <<<<<<
# Make all state names abbrevations
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
  locations.with.specimens$Location <- apply(locations.with.specimens["Location.State"], MARGIN = 1, function(loc){
    morph.data[morph.data[ , "Location.State"] == loc, "Location"][1]
  })
  #State Province ID
  locations.with.specimens$State.Province <- apply(locations.with.specimens["Location.State"], MARGIN = 1, function(loc){
    morph.data[morph.data[ , "Location.State"] == loc, "State.Province"][1]
  })
  #Lat
  locations.with.specimens$Lat <- apply(locations.with.specimens["Location.State"], MARGIN = 1, function(loc){
    morph.data[morph.data[ , "Location.State"] == loc, "Lat"][1]
  })
  #Long
  locations.with.specimens$Long <- apply(locations.with.specimens["Location.State"], MARGIN = 1, function(loc){
    morph.data[morph.data[ , "Location.State"] == loc, "Long"][1]
  })
  
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
#Evan and Arshdeep (Microscope 1)
measurement.calibrations.Evan.Arshdeep <- data.frame(mag = c(1:5), micrometer = c(100, 100, 100, 97, 97),
                                                     mm = c(9.9, 5.0, 3.3, 2.4, 1.9))
measurement.calibrations.Evan.Arshdeep$conversion <- measurement.calibrations.Evan.Arshdeep$mm/
  measurement.calibrations.Evan.Arshdeep$micrometer
# Yousaf (Microscope 2)
measurement.calibrations.Yousaf <- data.frame(mag = c(1:5), micrometer = c(100, 100, 100, 97, 81),
                                              mm = c(9.8, 5.0, 3.5, 2.4, 1.6))
measurement.calibrations.Yousaf$conversion <- measurement.calibrations.Yousaf$mm/
  measurement.calibrations.Yousaf$micrometer
# 	Evan-quarentined (Microscope 3)
measurement.calibrations.Evan.q <- data.frame(mag = c(1:4), micrometer = c(100, 99, 90, 97),
                                              mm = c(10, 4.9, 2.9, 2.4))
measurement.calibrations.Evan.q$conversion <- measurement.calibrations.Evan.q$mm/
  measurement.calibrations.Evan.q$micrometer

# Converiting measurements to mm based on who/where the sample was measured
apply(morph.data["Sample"], MARGIN = 1, function(sample) {
  #print(sample)
  if(morph.data[morph.data$Sample == sample, "Measured.By"] == "Yousaf"){
    morph.data[morph.data$Sample == sample, "Head.Width.mm"] <<- morph.data[morph.data$Sample == sample, "Head.Width"] * 
      measurement.calibrations.Yousaf[measurement.calibrations.Yousaf$mag == morph.data[morph.data$Sample == sample, "Magnification"], "conversion"]
    morph.data[morph.data$Sample == sample, "Intertegular.Width.mm"] <<- morph.data[morph.data$Sample == sample, "Intertegular.Width"] * 
      measurement.calibrations.Yousaf[measurement.calibrations.Yousaf$mag == morph.data[morph.data$Sample == sample, "Magnification"], "conversion"]
    morph.data[morph.data$Sample == sample, "Right.Wing.mm"] <<-  morph.data[morph.data$Sample == sample, "Right.Wing.Costal.Vein.Length"] * 
      measurement.calibrations.Yousaf[measurement.calibrations.Yousaf$mag == morph.data[morph.data$Sample == sample, "Magnification"], "conversion"]
    morph.data[morph.data$Sample == sample, "Left.Wing.mm"] <<- morph.data[morph.data$Sample == sample, "Left.Wing.Costal.Vein.Length"] * 
      measurement.calibrations.Yousaf[measurement.calibrations.Yousaf$mag == morph.data[morph.data$Sample == sample, "Magnification"], "conversion"]
  } else if (morph.data[morph.data$Sample == sample, "Measured.By"] == "Evan-quarentined"){
    morph.data[morph.data$Sample == sample, "Head.Width.mm"] <<- morph.data[morph.data$Sample == sample, "Head.Width"] * 
      measurement.calibrations.Evan.q[measurement.calibrations.Evan.q$mag == morph.data[morph.data$Sample == sample, "Magnification"], "conversion"]
    morph.data[morph.data$Sample == sample, "Intertegular.Width.mm"] <<- morph.data[morph.data$Sample == sample, "Intertegular.Width"] * 
      measurement.calibrations.Evan.q[measurement.calibrations.Evan.q$mag == morph.data[morph.data$Sample == sample, "Magnification"], "conversion"]
    morph.data[morph.data$Sample == sample, "Right.Wing.mm"] <<-  morph.data[morph.data$Sample == sample, "Right.Wing.Costal.Vein.Length"] * 
      measurement.calibrations.Evan.q[measurement.calibrations.Evan.q$mag == morph.data[morph.data$Sample == sample, "Magnification"], "conversion"]
    morph.data[morph.data$Sample == sample, "Left.Wing.mm"] <<- morph.data[morph.data$Sample == sample, "Left.Wing.Costal.Vein.Length"] * 
      measurement.calibrations.Evan.q[measurement.calibrations.Evan.q$mag == morph.data[morph.data$Sample == sample, "Magnification"], "conversion"]
  } else {
    morph.data[morph.data$Sample == sample, "Head.Width.mm"] <<- morph.data[morph.data$Sample == sample, "Head.Width"] * 
      measurement.calibrations.Evan.Arshdeep[measurement.calibrations.Evan.Arshdeep$mag == morph.data[morph.data$Sample == sample, "Magnification"], "conversion"]
    morph.data[morph.data$Sample == sample, "Intertegular.Width.mm"] <<- morph.data[morph.data$Sample == sample, "Intertegular.Width"] * 
      measurement.calibrations.Evan.Arshdeep[measurement.calibrations.Evan.Arshdeep$mag == morph.data[morph.data$Sample == sample, "Magnification"], "conversion"]
    morph.data[morph.data$Sample == sample, "Right.Wing.mm"] <<- morph.data[morph.data$Sample == sample, "Right.Wing.Costal.Vein.Length"] * 
      measurement.calibrations.Evan.Arshdeep[measurement.calibrations.Evan.Arshdeep$mag == morph.data[morph.data$Sample == sample, "Magnification"], "conversion"]
    morph.data[morph.data$Sample == sample, "Left.Wing.mm"] <<- morph.data[morph.data$Sample == sample, "Left.Wing.Costal.Vein.Length"] * 
      measurement.calibrations.Evan.Arshdeep[measurement.calibrations.Evan.Arshdeep$mag == morph.data[morph.data$Sample == sample, "Magnification"], "conversion"]
  }
})

# Remove the calibration dataframes as they are no longer needed
rm(measurement.calibrations.Evan.Arshdeep, measurement.calibrations.Evan.q, measurement.calibrations.Yousaf)


#Check for duplicate names
#duplicate.names <- unlist(apply(morph.data[ "Sample"], MARGIN = 1, function(sample.name){
#  if( length(morph.data[ morph.data$Sample == sample.name, "Sample"]) > 1){
#    as.character(sample.name)  }
#}))

#Check for how many samples per location and remove extra
morph.data$Location.State.Year <- paste0(morph.data$Location, morph.data$State.Province,
                                         morph.data$Specimen.Year)
#sample.summary <- data.frame( Location.State.Year = unique(morph.data$Location.State.Year),
#                             number.of.samples = apply(unique(morph.data["Location.State.Year"]), MARGIN = 1, function(loc.s.y){
#                               nrow( morph.data[ morph.data$Location.State.Year == loc.s.y, ])
#                             }),
#                             number.of.males = apply(unique(morph.data["Location.State.Year"]), MARGIN = 1, function(loc.s.y){
#                               nrow( morph.data[ morph.data$Location.State.Year == loc.s.y &
#                                                          morph.data$Sex == "Male", ])
#                             }), 
#                             number.of.females = apply(unique(morph.data["Location.State.Year"]), MARGIN = 1, function(loc.s.y){
#                               nrow( morph.data[ morph.data$Location.State.Year == loc.s.y &
#                                                          morph.data$Sex == "Female", ])
#                             }), 
#                             Lat = apply(unique(morph.data["Location.State.Year"]), MARGIN = 1, function(loc.s.y){
#                               morph.data[ morph.data$Location.State.Year == loc.s.y, "Lat"][1]
#                             }),
#                             Long = apply(unique(morph.data["Location.State.Year"]), MARGIN = 1, function(loc.s.y){
#                               morph.data[ morph.data$Location.State.Year == loc.s.y, "Long"][1]
#                             }),
#                             Year = apply(unique(morph.data["Location.State.Year"]), MARGIN = 1, function(loc.s.y){
#                               morph.data[ morph.data$Location.State.Year == loc.s.y, "Specimen.Year"][1]
#                             }))
#write.csv(sample.summary, "C:/Users/evank/OneDrive/Desktop/sample.summary.prior2018.csv")


#hist(sample.summary$number.of.samples, main = "", xlab = "Number of Samples", ylab = "Number of Locations + Year")
#hist(sample.summary$number.of.females, main = "", xlab = "Number of Females", ylab = "Number of Locations + Year")
#hist(sample.summary$number.of.males, main = "", xlab = "Number of Females", ylab = "Number of Locations + Year")

#excess.males <- unlist(apply(unique(morph.data["Location.State.Year"]), MARGIN = 1, function(loc.s.y){
#  temp.male <- morph.data[ morph.data$Location.State.Year == loc.s.y &
#                             morph.data$Sex == "Male", ]
#  if(nrow(temp.male) > 25) {
#    temp.male[25:nrow(temp.male), "Fate"] <- "Remove"
#    temp.male[temp.male$Fate == "Remove", "Sample"]
 # } else{}
#}))

#excess.females <- unlist(apply(unique(morph.data["Location.State.Year"]), MARGIN = 1, function(loc.s.y){
#  temp <- morph.data[ morph.data$Location.State.Year == loc.s.y &
#                             morph.data$Sex == "Female", ]
#  if(nrow(temp) > 25) {
#    temp[25:nrow(temp), "Fate"] <- "Remove"
#    temp[temp$Fate == "Remove", "Sample"]
#  } else{}
#}))

#morph.data <- subset(morph.data,! morph.data$Sample %in% excess.males)
#morph.data <- subset(morph.data,! morph.data$Sample %in% excess.females)

# Create the Additonal Measurements
morph.data$head.to.wing <- morph.data$Right.Wing.mm/morph.data$Head.Width.mm
morph.data$head.to.intertegular <- morph.data$Intertegular.Width.mm/morph.data$Head.Width.mm
morph.data$wing.to.intertegular <- morph.data$Right.Wing.mm/morph.data$Intertegular.Width.mm
morph.data$asymmetry <- abs(morph.data$Right.Wing.mm - 
                              morph.data$Left.Wing.mm)

# Subset to equal only C. calcarata
morph.data <- subset(morph.data, morph.data$Species == "calcarata")

# Subsetting by data that has lat and long values
#morph.data_4_7_20 <- subset(morph.data, !is.na(morph.data$Lat))
#write.csv(morph.data_4_7_20, "C:/Users/evank/OneDrive/Desktop/morphdata4720.csv")
#morph.data_4_7_21 <- morph.data_4_7_20
#morph.data_4_7_21$Specimen.Year <- morph.data_4_7_20$Specimen.Year - 1
#morph.data_4_7_21 <- subset(morph.data_4_7_21, morph.data_4_7_21$Specimen.Year > 1900)
#write.csv(morph.data_4_7_21, "C:/Users/evank/OneDrive/Desktop/morphdata4721.csv")


# Subset to include only those with complete measurements
morph.data <- subset(morph.data, !is.na(morph.data$Head.Width.mm))
#morph.data <- subset(morph.data, !is.na(morph.data$Intertegular.Width.mm))
#morph.data <- subset(morph.data, !is.na(morph.data$Right.Wing.mm))

# Adding a binning scheme
morph.data$pre.post <- ifelse(morph.data$Specimen.Year < 1988, "pre", "post")


# Adding Elevation
elevation <- read.csv("C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Elevation_Data.csv")

morph.data$Elevation <- unlist(sapply(morph.data[, "Sample"], function(sample.name){
  elevation[elevation$Sample == sample.name, "RASTERVALU"] 
}))

morph.data$EcoRegion <- unlist(sapply(morph.data[, "Sample"], function(sample.name){
  elevation[elevation$Sample == sample.name, "LEVEL2"] 
}))
