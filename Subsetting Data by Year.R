# Load in data
source("C:/Users/evank/OneDrive/Desktop/Maps/Ceratina-Maps/Ceratina Data.R")
  # If ballparking location
  source("C:/Users/evank/OneDrive/Desktop/Maps/Ceratina-Maps/Ceratina Data Ballpark Location.R")
source("C:/Users/evank/OneDrive/Desktop/Maps/Ceratina-Maps/Sarah's Bees Data.R")

# Subset by calcarata
ceratina.samples <- subset(ceratina.samples, ceratina.samples$species == "calcarata" |
                             ceratina.samples$species == "unknown"
                             )
ceratina.samples <- subset(ceratina.samples, !ceratina.samples$Year %in% c("-", "?"))
ceratina.samples$Year <- as.numeric(as.character(ceratina.samples$Year))

# Contemporary Specimens
Lat.Long <- read.csv("C:/Users/evank/OneDrive/Desktop/Maps/Ceratina Data/Lat.Long.csv")
Lat.Long <- subset(Lat.Long, Lat.Long$Number.of.Samples != 0)
Lat.Long$Year <- 2019


# Ballparking Lat Long of My Samples to make all locations Ballparked
apply(state.lat.long["State.Province"], MARGIN = 1, function(state) {
  Lat.Long$Lat <<- ifelse(Lat.Long$States.full == state, 
                               state.lat.long[state.lat.long$State.Province == state, "Lat"],
                               Lat.Long$Lat)
})

apply(state.lat.long["State.Province"], MARGIN = 1, function(state) {
  Lat.Long$Long <<- ifelse(Lat.Long$States.full == state, 
                                state.lat.long[state.lat.long$State.Province == state, "Long"],
                                Lat.Long$Long)
})


# Combining ceratina samples with my samples and Sarah's bees
names(ceratina.samples)[6:7] <- c("Lat", "Long")
names(Lat.Long)[7] <- "State.Province"

all.samples <- rbind(Lat.Long[c(8,3:5,7)], ceratina.samples[c(5:7,11,4)], 
                     sarahs.bees.summary[c(5,3:4,2,6)])

#all.samples <- ceratina.samples
# Removing unknown species
#ceratina.samples <- subset(ceratina.samples, 
 #                          ceratina.samples$State.Province != "?")



# Subsetting data frames by year
all.samples$Year <- as.numeric(as.character(all.samples$Year))
all.samples <- subset(all.samples, all.samples$Number.of.Samples > 0)


all.samples.2016 <- subset(all.samples, all.samples$Year >= 2016)

all.samples.2009 <- subset(all.samples, all.samples$Year >= 2009)

all.samples.prior2009 <- subset(all.samples, all.samples$Year < 2009)
all.samples.1990.2009 <- subset(all.samples.prior2009, all.samples.prior2009$Year >= 1990)

all.samples.prior1990 <- subset(all.samples, all.samples$Year < 1990)
all.samples.1970.1990 <- subset(all.samples.prior1990, all.samples.prior1990$Year >= 1970)

all.samples.prior1970 <- subset(all.samples, all.samples$Year < 1970)

all.samples.1990.2009 <- subset(all.samples.prior2009, all.samples.prior2009$Year >= 1990)


all.samples.1980.2009 <- subset(all.samples.prior2009, all.samples.prior2009$Year >= 1980)

all.samples.prior1980 <- subset(all.samples, all.samples$Year < 1980)


write.csv( all.samples.2016, "C:/Users/evank/OneDrive/Desktop/Maps/Ceratina Data/samples2016.csv")
write.csv( all.samples.2009, "C:/Users/evank/OneDrive/Desktop/Maps/Ceratina Data/samples2009.csv")
write.csv( all.samples.prior1980, "C:/Users/evank/OneDrive/Desktop/Maps/Ceratina Data/samplesprior1980.csv")
write.csv( all.samples.1980.2009, "C:/Users/evank/OneDrive/Desktop/Maps/Ceratina Data/samples1980.2009.csv")

#Adding Contemporary samples to the museum specimens

# Ballparking Lat Long of My Samples to make all locations Ballparked
apply(state.lat.long["State.Province"], MARGIN = 1, function(state) {
  Lat.Long$Latitude <<- ifelse(Lat.Long$States.full == state, 
                               states.lat.long[state.lat.long$State.Province == state, "Lat"],
                               Lat.Long$Latitude)
})

apply(state.lat.long["State.Province"], MARGIN = 1, function(state) {
  Lat.Long$Longitude <<- ifelse(Lat.Long$States.full == state, 
                                state.lat.long[state.lat.long$State.Province == state, "Long"],
                                Lat.Long$Longitude)
})

write.csv(all.samples)


# Subsetting for tables
source("C:/Users/evank/OneDrive/Desktop/Maps/Ceratina-Maps/Ceratina Data.csv")

all.samples.summary <- bin.by.cat("State.Province", all.samples,"Year")
names(all.samples.summary)[1:2] <- c("State.Province", "Year")

write.csv( all.samples, "C:/Users/evank/OneDrive/Desktop/Maps/Ceratina Data/samples.summary.csv")
