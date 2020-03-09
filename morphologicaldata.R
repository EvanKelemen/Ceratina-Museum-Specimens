#-------------------------------------------------------------------------------
# Analyze Morphological Data
#-------------------------------------------------------------------------------
#Load in the data
morph.data <- read.csv("C:/Users/evank/OneDrive/Desktop/Morphological Measurements.csv")
state_abbrivations <- read.csv("C:/Users/evank/OneDrive/Desktop/Maps/Ceratina Data/StatetoAbreviations.csv")

# Create the Additonal Measurements
morph.data$head.to.wing <- morph.data$Head.Width/morph.data$Right.Wing.Costal.Vein.Length
morph.data$head.to.intertegular <- morph.data$Head.Width/morph.data$Intertegular.Width
morph.data$asymmetry <- abs(morph.data$Right.Wing.Costal.Vein.Length - 
                              morph.data$Left.Wing.Costal.Vein.Length)

#Remove Extraneous rows
morph.data <- subset(morph.data, ! is.na(morph.data$Specimen.Year))
#morph.data <- subset(morph.data, morph.data$Specimen.Year > 2016)

# Make all state names abbrevations
morph.data$State.Province <- as.character(morph.data$State.Province)
state_abbrivations$Abbreviation <- as.character(state_abbrivations$Abbreviation)
apply(state_abbrivations["State_Province"], MARGIN = 1, function(loc) {
  morph.data$State.Province <<- ifelse(morph.data$State.Province == loc, 
                                       state_abbrivations[state_abbrivations$State_Province == loc, "Abbreviation"],
                            morph.data$State.Province)
})
morph.data$Location.State <- paste(morph.data$Location, morph.data$State.Province, sep = "_")

# Create a data.frame of locations - indicating Lat and Long
locations.with.specimens <- data.frame(Location.State = unique(morph.data$Location.State))
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
  
# Take locations that aready appear in the dataframe and attach lat and longs to future events  
apply(morph.data["Location.State"], MARGIN = 1, function(loc) {
    morph.data$Lat <<- ifelse(morph.data$Location.State == loc, 
                              locations.with.specimens[locations.with.specimens$Location.State == loc, "Lat"],
                                        morph.data$Lat)
    morph.data$Long <<- ifelse(morph.data$Location.State == loc, 
                               locations.with.specimens[locations.with.specimens$Location.State == loc, "Long"],
                                         morph.data$Long)
  })


#Remove unadjected for individuals
morph.data <- subset(morph.data, morph.data$Magnification > 3)
#Plotting the data to visualize it
plot(Head.Width ~ Specimen.Year, data = morph.data, cex = 2, cex.lab = 2, cex.axis = 2)
abline(159.254448, -0.041149, col = "blue")
summary(lm(Head.Width ~ Specimen.Year, data = morph.data))


plot(Intertegular.Width ~ Specimen.Year, data = morph.data,  cex = 2, cex.lab = 2, cex.axis = 2)
abline(99.878820, -0.020805, col = "blue")
summary(lm(Intertegular.Width ~ Specimen.Year, data = morph.data))


plot(Right.Wing.Costal.Vein.Length ~ Specimen.Year, data = morph.data, cex = 2, cex.lab = 2, cex.axis = 2)
summary(lm(Right.Wing.Costal.Vein.Length ~ Specimen.Year, data = morph.data))
abline(166.920104, -0.04, col = "blue")

plot(Left.Wing.Costal.Vein.Length ~ Specimen.Year, data = morph.data)
plot(Wing.Wear ~ Specimen.Year, data = morph.data)
plot(head.to.wing ~ Specimen.Year, data = morph.data)
plot(head.to.intertegular ~ Specimen.Year, data = morph.data)
plot(asymmetry ~ Specimen.Year, data = morph.data, cex = 2, cex.lab = 2, cex.axis = 2)
summary(lm(asymmetry ~ Specimen.Year, data = morph.data))



hist(morph.data[,"Wing.Wear"])
plot(Left.Wing.Costal.Vein.Length ~ Right.Wing.Costal.Vein.Length, data = morph.data)



morph.data[morph.data$Head.Width < 50,]
morph.data[morph.data$Intertegular.Width < 40,]
morph.data[morph.data$Intertegular.Width > 72,]

morph.data[morph.data$head.to.wing > 1,]
morph.data[morph.data$head.to.intertegular < 1.2,]

morph.data[morph.data$asymmetry > 8,]

#>> Export the Data File to Import into GIS for temperature Data
write.csv(morph.data, "C:/Users/evank/OneDrive/Desktop/morph.data.2.21.20.csv")


#--------------------------------------------
# Scratch for just current specimens
#--------------------------------------------

morph.data <- subset(morph.data, ! morph.data$Site == "")
morph.data <- subset(morph.data, ! morph.data$Site == "Knener/Grixiti")
morph.data <- subset(morph.data, morph.data$Sex != "Male")

centroid.temp <- read.csv("C:/Users/evank/OneDrive/Desktop/centroids.csv")
centroid.temp <- read.csv("C:/Users/evank/OneDrive/Desktop/centroids_2017_average.csv")


morph.data$temp <-0
morph.data$Site <- as.character(morph.data$Site)

apply(morph.data["Site"], MARGIN = 1, function(loc) {
  morph.data$Lat <<- ifelse(morph.data$Site == loc, 
                            centroid.temp[centroid.temp$State_Prov == loc, "Lat"],
                            morph.data$Lat)
  morph.data$Long <<- ifelse(morph.data$Site == loc, 
                             centroid.temp[centroid.temp$State_Prov == loc, "Long"],
                             morph.data$Long)
  morph.data$temp <<- ifelse(morph.data$Site == loc, 
                             centroid.temp[centroid.temp$State_Prov == loc, "RASTERVALU"],
                             morph.data$temp)
})


plot(Head.Width ~ temp, data = morph.data,  cex = 2, cex.lab = 2, cex.axis = 2)
abline(99.878820, -0.020805, col = "blue")
summary(lm(Head.Width ~ temp, data = morph.data))



plot(Intertegular.Width ~ temp, data = morph.data,  cex = 2, cex.lab = 2, cex.axis = 2)
abline(99.878820, -0.020805, col = "blue")
summary(lm(Intertegular.Width ~ temp, data = morph.data))


plot(Right.Wing.Costal.Vein.Length ~ temp, data = morph.data, cex = 2, cex.lab = 2, cex.axis = 2)
summary(lm(Right.Wing.Costal.Vein.Length ~ temp, data = morph.data))
abline(166.920104, -0.04, col = "blue")

plot(Left.Wing.Costal.Vein.Length ~ temp, data = morph.data)
plot(as.numeric(Wing.Wear) ~ temp, data = morph.data)
plot(head.to.wing ~ temp, data = morph.data)
plot(head.to.intertegular ~ temp, data = morph.data)
plot(asymmetry ~ temp, data = morph.data, cex = 2, cex.lab = 2, cex.axis = 2)


morph.data[morph.data$head.to.wing >1,]






centroid.temp == -9999


centroid.temp <- read.csv("C:/Users/evank/OneDrive/Desktop/morph_2017_average.csv")
centroid.temp$Head_Width <- ifelse(centroid.temp$Head_Width == -9999, NA, centroid.temp$Head_Width)
centroid.temp$Intertegul <- ifelse(centroid.temp$Intertegul == -9999, NA, centroid.temp$Intertegul)
centroid.temp$Right_Wing <- ifelse(centroid.temp$Right_Wing == -9999, NA, centroid.temp$Right_Wing)
centroid.temp <- subset(centroid.temp, centroid.temp$Species == "calcarata")
centroid.temp.female <- subset(centroid.temp, centroid.temp$Sex != "Male")


plot(Head_Width ~ RASTERVALU, data = centroid.temp.female,  cex = 2, cex.lab = 2, cex.axis = 2,
     pch = 16,  tck = 0.02)
abline(71.3614, 0.147, col = "blue")
summary(lm(Head_Width ~ RASTERVALU, data = centroid.temp.female))
        

plot(Intertegul ~ RASTERVALU, data = centroid.temp.female,  cex = 2, cex.lab = 2, cex.axis = 2,
     pch = 16, tck = 0.02)
abline(53.6354, 0.3732, col = "blue")
summary(lm(Intertegul ~ RASTERVALU, data = centroid.temp.female))

plot(Right_Wing ~ RASTERVALU, data = centroid.temp.female,  cex = 2, cex.lab = 2, cex.axis = 2,
     pch = 16, tck = 0.02)
abline(53.6354, 0.3732, col = "blue")
summary(lm(Right_Wing ~ RASTERVALU, data = centroid.temp.female))

centroid.temp.female$asymmetry <- abs(centroid.temp.female$Right_Wing - 
                              centroid.temp.female$Left_Wing_)

plot(asymmetry ~ RASTERVALU, data = centroid.temp.female,  cex = 2, cex.lab = 2, cex.axis = 2,
     pch = 16, tck = 0.02)
summary(lm(asymmetry ~ RASTERVALU, data = centroid.temp.female))
