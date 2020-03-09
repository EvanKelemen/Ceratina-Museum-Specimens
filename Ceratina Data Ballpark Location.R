source("C:/Users/evank/OneDrive/Desktop/Maps/Ceratina-Maps/Ceratina Data.R")


# Add states lat long
state.lat.long <- read.csv("C:/Users/evank/OneDrive/Desktop/Maps/Ceratina Data/States.Lat.Long.csv")
ceratina.samples$Latitude <- as.numeric(as.character(ceratina.samples$Latitude))

apply(state.lat.long["State.Province"], MARGIN = 1, function(state) {
  ceratina.samples$Latitude <<- ifelse(ceratina.samples$State.Province == state, 
                                       state.lat.long[state.lat.long$State.Province == state, "Lat"],
                                       ceratina.samples$Latitude)
})

apply(state.lat.long["State.Province"], MARGIN = 1, function(state) {
  ceratina.samples$Longitude <<- ifelse(ceratina.samples$State.Province == state, 
                                        state.lat.long[state.lat.long$State.Province == state, "Long"],
                                        ceratina.samples$Longitude)
})


ceratina.samples <- subset(ceratina.samples, ! is.na(ceratina.samples$Latitude))
