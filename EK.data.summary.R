#-------------------------------------------------------------------------------
# Summarizing Morphlogical Data
#-------------------------------------------------------------------------------

##import the data
EK.data <- read.csv("C:/Users/evank/OneDrive/Desktop/EK Samples Details.csv")
# Lat Long Data
Lat.Long <- read.csv("C:/Users/evank/OneDrive/Desktop/Maps/Ceratina Data/Lat.Long.csv")

# Remove samples of just larvae or unknown species
EK.data <- subset(EK.data, EK.data$Sex %in% c("male", "female"))

# Creating a summary data.frame
EK.data.summary <- data.frame( location = unique(EK.data[ , "Location"]))
buckets <-  unique(EK.data[ , "Location"])
sapply(buckets, simplify = TRUE,  function( ind.bucket){
  temp <- subset(EK.data, EK.data[ , "Location"] == ind.bucket) 
  EK.data.summary[ EK.data.summary[ , "location"] == ind.bucket, "total Ceratina"] <<- nrow(temp)
  temp.cal <- subset(temp, temp[ , "Species"] == "calcarata")
  EK.data.summary[ EK.data.summary[ , "location"] == ind.bucket, "total calcarata" ] <<- nrow(temp.cal)
  temp.cal.female <- subset(temp.cal, temp.cal[ , "Sex"] == "female")
  EK.data.summary[ EK.data.summary[ , "location"] == ind.bucket, "total calcarata.female" ] <<- 
    nrow(temp.cal.female)
  EK.data.summary[ EK.data.summary[ , "location"] == ind.bucket, "total calcarata.female.from.different.nests" ] <<- 
    length(unique(temp.cal.female$Nest))
})

#>> Add Lat and Long
  # Create Columns Lat and Long
  EK.data.summary$Latitude <- 0
  EK.data.summary$Longitude <- 0
  EK.data.summary$State.Province <- NA
# Combine Lat.Long data with EK.data.summary  
apply(EK.data.summary["location"], MARGIN = 1, function(location) {
  EK.data.summary$Latitude <<- ifelse(EK.data.summary$location == location, 
                                      Lat.Long[Lat.Long$Location == location, "Lat"],
                                      EK.data.summary$Latitude)
  EK.data.summary$Longitude <<- ifelse(EK.data.summary$location == location, 
                                         Lat.Long[Lat.Long$Location == location, "Long"],
                                         EK.data.summary$Longitude)
  EK.data.summary$State.Province <<- ifelse(EK.data.summary$location == location, 
                                          as.character(Lat.Long[Lat.Long$Location == location, "State.Province"]),
                                          as.character(EK.data.summary$State.Province))
})

#Save the Data
write.csv(EK.data.summary, "C:/Users/evank/OneDrive/Desktop/EKsamples.summary.data.csv")
