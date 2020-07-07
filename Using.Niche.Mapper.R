#-------------------------------------------------------------------------------
# Niche Mapper 
#-------------------------------------------------------------------------------
#https://mrke.github.io/getting_started/
# Manually Downloading the pack did not work
#install.packages("C:/Users/evank/Documents/R/NicheMapR_2.0.0.zip", 
#                   repos = NULL, 
#                 type = "win.binary")
#Downloaded it from GitHub
#install.packages('devtools')
#devtools::install_github('mrke/NicheMapR')
#install.packages("Rtools")
#install.packages("rtools40")
library(NicheMapR)
#get.global.climate(folder="C:/Users/evank/Documents/R")

# Need to run the function micro_ncep
#install.packages('RNCEP')
#install.packages('elevatr')
#devtools::install_github('ilyamaclean/microclima', force = TRUE)
#install.packages('rlang')
# Requires the package futile.logger
library(futile.logger)

loc <- c(-89.49, 43.07) #-89.40, 43.07 (-82.45, 27.95) Tampa Florida
dstart <- "01/01/2000"  # Can't go further back than 1980
dfinish <- "31/12/2000" # Can't go further back than 1980

loc <- c(locations.date.with.specimens[1, "Long"], 
         locations.date.with.specimens[1, "Lat"]) #-89.40, 43.07 (-82.45, 27.95) Tampa Florida
dstart <- locations.date.with.specimens[1, "dstart"]  # Can't go further back than 1980
dfinish <- locations.date.with.specimens[1, "dfinish"] # Can't go further back than 1980

micro <- micro_usa(loc = loc, dstart = dstart, dfinish = dfinish)
metout <- as.data.frame(micro$metout)
soil <- as.data.frame(micro$soil)
soilmoist <- as.data.frame(micro$soilmoist)
air <- as.data.frame(micro$metout)
plot(soil$D5cm ~ micro$dates, type = 'l')
plot(soilmoist$WC5cm ~ micro$dates, type = 'l')
plot(metout$SNOWDEP ~ micro$dates, type = 'l')


# Allowable error ERR
ERR <- 4
micro.1 <- micro_ncep(loc = loc, dstart = dstart, dfinish = dfinish, ERR = ERR) #
                      #reanalysis = FALSE)
micro.1 <- micro_ncep(loc = loc, dstart = dstart, dfinish = dfinish, save = 0, 
                      spatial = "C:/Users/evank/Documents/R/NicheMapR Data")
air.1 <- as.data.frame(micro.1$metout)
soil.1 <- as.data.frame(micro.1$soil)
rain.fall.1 <- as.data.frame(micro.1$RAINFALL)
mean(rain.fall.1[,1])
plot(air.1$TAREF ~ air$TAREF)
summary(lm(air.1$TAREF ~ air$TAREF))
plot(soil.1$D5cm ~ soil$D5cm)
summary(lm(soil.1$D5cm ~ soil$D5cm))




# Requires 
# Makes data.frame morph.data.lat.long
source("C:/Users/evank/OneDrive/Desktop/Maps/Ceratina-Maps/Land.Use.Combining.R")

# Create a data.frame of locations - indicating Lat and Long
morph.data.1958 <- subset(morph.data.lat.long, morph.data.lat.long$Specimen.Year > 1973) #1957
locations.date.with.specimens <- data.frame(Location.State.Date = unique(morph.data.1958$Location.State.Year), 
                                            number.of.zeros = -99999,
                                            Ave.Temp = -99999,
                                            Ave.Precip = -99999
                                            )
#Add: Location
locations.date.with.specimens$Location <- apply(locations.date.with.specimens["Location.State.Date"], MARGIN = 1, function(loc){
  morph.data.1958[morph.data.1958[ , "Location.State.Date"] == loc, "Location"][1]
})
#State Province ID
locations.date.with.specimens$State.Province <- apply(locations.date.with.specimens["Location.State.Date"], MARGIN = 1, function(loc){
  morph.data.1958[morph.data.1958[ , "Location.State.Date"] == loc, "State.Province"][1]
})
#Specimen Year
locations.date.with.specimens$Year <- apply(locations.date.with.specimens["Location.State.Date"], MARGIN = 1, function(loc){
  morph.data.1958[morph.data.1958[ , "Location.State.Date"] == loc, "Specimen.Year"][1]
})
#Lat
locations.date.with.specimens$Lat <- apply(locations.date.with.specimens["Location.State.Date"], MARGIN = 1, function(loc){
  morph.data.1958[morph.data.1958[ , "Location.State.Date"] == loc, "Lat"][1]
})
#Long
locations.date.with.specimens$Long <- apply(locations.date.with.specimens["Location.State.Date"], MARGIN = 1, function(loc){
  morph.data.1958[morph.data.1958[ , "Location.State.Date"] == loc, "Long"][1]
})

# Make the starting year and ending year
apply(locations.date.with.specimens["Location.State.Date"], MARGIN = 1, function(sample_name){
  locations.date.with.specimens[locations.date.with.specimens$Location.State.Date == sample_name, "dstart"] <<-
    paste0("01/01/", locations.date.with.specimens[locations.date.with.specimens$Location.State.Date == sample_name, "Year"])
  locations.date.with.specimens[locations.date.with.specimens$Location.State.Date == sample_name, "dfinish"] <<-
    paste0("31/12/", locations.date.with.specimens[locations.date.with.specimens$Location.State.Date == sample_name, "Year"])
})


# Calculates average points
#apply(locations.date.with.specimens["Location.State.Date"], MARGIN = 1, function(sample_name){
#  loc <- c(locations.date.with.specimens[locations.date.with.specimens$Location.State.Date == sample_name, "Long"],
#           locations.date.with.specimens[locations.date.with.specimens$Location.State.Date == sample_name, "Lat"])
#  dstart <- locations.date.with.specimens[locations.date.with.specimens$Location.State.Date == sample_name, "dstart"]
#  dfinish <- locations.date.with.specimens[locations.date.with.specimens$Location.State.Date == sample_name, "dfinish"]
#  if(locations.date.with.specimens[ locations.date.with.specimens$Location.State.Date == sample_name, "Year"] > 1979){
#    micro <- micro_ncep(loc = loc, dstart = dstart, dfinish = dfinish, ERR = 4)
#  } else{  micro <- micro_ncep(loc = loc, dstart = dstart, dfinish = dfinish, ERR = 4, reanalysis = FALSE)}
#  air <- as.data.frame(micro$metout)
#  rain.fall <- as.data.frame(micro$RAINFALL)
#  ave.precip <- mean(rain.fall[,1])
#  ave.temp <- mean(air$TAREF)
#  number.of.zeros <- length(air[ air$TAREF == 0, "TAREF"])
#  locations.date.with.specimens[ , "Ave.Temp"] <<-
#    ifelse(locations.date.with.specimens$Location.State.Date == sample_name, 
#           ave.temp,
#           locations.date.with.specimens$Ave.Temp)
#  locations.date.with.specimens[ , "Ave.Precip"] <<-
#    ifelse(locations.date.with.specimens$Location.State.Date == sample_name, 
#           ave.precip,
#           locations.date.with.specimens$Ave.Precip)
#  locations.date.with.specimens[ , "number.of.zeros"] <<-
#    ifelse(locations.date.with.specimens$Location.State.Date == sample_name, 
#           number.of.zeros,
#           locations.date.with.specimens$number.of.zeros)
#})


locations.date.with.specimens <- locations.date.with.specimens[240:334,]

apply(locations.date.with.specimens["Location.State.Date"], MARGIN = 1, function(sample_name){
  loc <<- c(locations.date.with.specimens[locations.date.with.specimens$Location.State.Date == sample_name, "Long"],
           locations.date.with.specimens[locations.date.with.specimens$Location.State.Date == sample_name, "Lat"])
  dstart <<- locations.date.with.specimens[locations.date.with.specimens$Location.State.Date == sample_name, "dstart"]
  dfinish <<- locations.date.with.specimens[locations.date.with.specimens$Location.State.Date == sample_name, "dfinish"]
  if(locations.date.with.specimens[ locations.date.with.specimens$Location.State.Date == sample_name, "Year"] > 1979){
    micro <- micro_ncep(loc = loc, dstart = dstart, dfinish = dfinish, ERR = 4)
  } else{  micro <- micro_ncep(loc = loc, dstart = dstart, dfinish = dfinish, ERR = 4, reanalysis = FALSE)}
write.csv(as.data.frame(micro$metout), paste0("C:/Users/evank/OneDrive/Desktop/micro_NCEP tables/", 
                        "air", sample_name, ".csv"))
write.csv(as.data.frame(micro$RAINFALL), paste0("C:/Users/evank/OneDrive/Desktop/micro_NCEP tables/", 
                        "precip", sample_name, ".csv"))
})






# Take locations that aready appear in the dataframe and attach lat and longs to future events  
#apply(morph.data.1958["Location.State.Date"], MARGIN = 1, function(loc) {
#  morph.data.1958$Temp <<- ifelse(morph.data.1958$Location.State.Date == loc, 
#                            locations.date.with.specimens[locations.date.with.specimens$Location.State.Date == loc, "Ave.Temp.NicheMapR"],
#                            morph.data.1958$Ave.Temp.NicheMapR)
#})


