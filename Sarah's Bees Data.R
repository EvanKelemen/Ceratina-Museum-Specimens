# Load in data

# Add states lat long
sarahs.bees <- read.csv("C:/Users/evank/OneDrive/Desktop/Maps/Specimens Recieved/Loan Forms/Competition_Data.csv")
sarahs.bees.lat.long <- read.csv("C:/Users/evank/OneDrive/Desktop/Maps/Specimens Recieved/Loan Forms/Competition_Data_Locations.csv")

# Subset Sarahs Bees to Ceratina
sarahs.bees <- subset(sarahs.bees, sarahs.bees$Genus == "Ceratina")

# Link the Latitude to Sample Data
sarahs.bees$Latitude <- 0
apply(sarahs.bees.lat.long["Site_Ab"], MARGIN = 1, function(state) {
  sarahs.bees$Latitude <<- ifelse(sarahs.bees$Location == state, 
                                       sarahs.bees.lat.long[sarahs.bees.lat.long$Site_Ab == state, "Latitude"],
                                       sarahs.bees$Latitude)
})

# Link the Longitude to Sample Data
sarahs.bees$Longitude <- 0
apply(sarahs.bees.lat.long["Site_Ab"], MARGIN = 1, function(state) {
  sarahs.bees$Longitude <<- ifelse(sarahs.bees$Location == state, 
                                        sarahs.bees.lat.long[sarahs.bees.lat.long$Site_Ab == state, "Longitude"],
                                        sarahs.bees$Longitude)
})


# Summary of Sarah's Bees
source("C:/Users/evank/OneDrive/Desktop/Maps/Ceratina-Maps/bin.by.R")

sarahs.bees.summary <- bin.by.cat("Location", sarahs.bees)

# Link the Latitude to Sample Data
sarahs.bees.summary$Latitude <- 0
apply(sarahs.bees.lat.long["Site_Ab"], MARGIN = 1, function(state) {
  sarahs.bees.summary$Latitude <<- ifelse(sarahs.bees.summary$Location == state, 
                                  sarahs.bees.lat.long[sarahs.bees.lat.long$Site_Ab == state, "Latitude"],
                                  sarahs.bees.summary$Latitude)
})

# Link the Longitude to Sample Data
sarahs.bees.summary$Longitude <- 0
apply(sarahs.bees.lat.long["Site_Ab"], MARGIN = 1, function(state) {
  sarahs.bees.summary$Longitude <<- ifelse(sarahs.bees.summary$Location == state, 
                                   sarahs.bees.lat.long[sarahs.bees.lat.long$Site_Ab == state, "Longitude"],
                                   sarahs.bees.summary$Longitude)
})


sarahs.bees.summary <- subset(sarahs.bees.summary, sarahs.bees.summary$Latitude > 0)
colnames(sarahs.bees.summary) <- c("Location", "Number.of.Samples", "Lat", "Long")
sarahs.bees.summary$Year <- 2019
sarahs.bees.summary$State.Province <- "Ontario"
