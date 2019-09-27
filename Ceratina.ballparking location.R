#-------------------------------------------------------------------------------
# Ballparking locations
#-------------------------------------------------------------------------------




# SUMMARIZING BY STATE
ceratina.samples <- read.csv("C:/Users/evank/OneDrive/Desktop/Ceratina Samples.csv")


ceratina.samples <- subset(ceratina.samples, !ceratina.samples$species %in% c("nanula", "apacheorum", "citriphila", 
                                                                              "cyaniventris", "arizonensis", "acantha/nanula",
                                                                              "callidum", "andreniformis", "neomexicana", 
                                                                              "mocsaryi", "acantha", "cyanea", "guarnacciana",
                                                                              "smaragdula", "Ceratina palavensis Yasumatsu"))


ceratina.samples$State.Providence <- ifelse(ceratina.samples$State.Providence == "NS", "Nova.Scotia", 
                                     ifelse(ceratina.samples$State.Providence == "CT", "Connecticut",
                                     ifelse(ceratina.samples$State.Providence == "DC", "District.of.Columbia",
                                     ifelse(ceratina.samples$State.Providence == "GA", "Georgia",
                                     ifelse(ceratina.samples$State.Providence == "KY", "Kentucky",
                                     ifelse(ceratina.samples$State.Providence == "MA", "Massachusetts",
                                     ifelse(ceratina.samples$State.Providence == "MD", "Maryland",
                                     ifelse(ceratina.samples$State.Providence == "ME", "Maine",
                                     ifelse(ceratina.samples$State.Providence == "MI", "Michigan",
                                     ifelse(ceratina.samples$State.Providence == "NC", "North.Carolina",
                                     ifelse(ceratina.samples$State.Providence == "NH", "New.Hampshire",
                                     ifelse(ceratina.samples$State.Providence == "NJ", "New.Jersey",
                                     ifelse(ceratina.samples$State.Providence == "NY", "New.York",
                                     ifelse(ceratina.samples$State.Providence == "OH", "Ohio",
                                     ifelse(ceratina.samples$State.Providence == "PA", "Pennsylvania",
                                     ifelse(ceratina.samples$State.Providence == "RI", "Rhode.Island",
                                     ifelse(ceratina.samples$State.Providence == "TN", "Tennesse",
                                     ifelse(ceratina.samples$State.Providence == "DE", "Delaware",
                                     ifelse(ceratina.samples$State.Providence == "IN", "Indiana",
                                     ifelse(ceratina.samples$State.Providence == "VA", "Virginia",
                                     ifelse(ceratina.samples$State.Providence == "WI", "Wisconsin",
                                     ifelse(ceratina.samples$State.Providence == "WV", "West.Virginia",
                                     ifelse(ceratina.samples$State.Providence == "CA", "California",
                                     ifelse(ceratina.samples$State.Providence == "SC", "South.Carolina",
                                     ifelse(ceratina.samples$State.Providence == "NE", "Nebraska",
                                     ifelse(ceratina.samples$State.Providence == "ALTA", "Georgia",
                                     ifelse(ceratina.samples$State.Providence == "Tenn", "Tennesse",
                                     ifelse(ceratina.samples$State.Providence == "MD.", "Maryland",
                                     ifelse(ceratina.samples$State.Providence == "MICH", "Michigan",
                                     ifelse(ceratina.samples$State.Providence == "Fla", "Florida",
                                     ifelse(ceratina.samples$State.Providence == "Md", "Maryland",
                                     ifelse(ceratina.samples$State.Providence == "Washington DC", "District.of.Columbia",
                                     ifelse(ceratina.samples$State.Providence == "MO", "Missouri",
                                     ifelse(ceratina.samples$State.Providence == "Minn", "Minnesota",
                                     ifelse(ceratina.samples$State.Providence == "MS", "Mississippi",
                                     ifelse(ceratina.samples$State.Providence == "COLO", "Colorado",
                                     ifelse(ceratina.samples$State.Providence == "NB", "New.Bunswick",
                                     ifelse(ceratina.samples$State.Providence == "AR", "Arkansas",
                                     ifelse(ceratina.samples$State.Providence == "ARK", "Arkansas",
                                     ifelse(ceratina.samples$State.Providence == "TX", "Texas",
                                     ifelse(ceratina.samples$State.Providence == "AZ", "Arizona",
                                     ifelse(ceratina.samples$State.Providence == "Pennsylvania ", "Pennsylvania",
                                     ifelse(ceratina.samples$State.Providence == "MT", "Montana",
                                     ifelse(ceratina.samples$State.Providence == "Quebec ", "Quebec",
                                     ifelse(ceratina.samples$State.Providence == "NF", "New.Foundland",
                                     ifelse(ceratina.samples$State.Providence == "New Hampshire", "New.Hampshire",
                                     ifelse(ceratina.samples$State.Providence == "New Jersey", "New.Jersey",
                                     ifelse(ceratina.samples$State.Providence == "West Virginia", "West.Virginia",
                                     ifelse(ceratina.samples$State.Providence == "South Carolina", "South.Carolina",
                                     as.character(ceratina.samples$State.Providence))))))))))))))))))))))
                                     ))))))))))))))))))))))))))))
ceratina.samples$State.Providence <- ifelse(ceratina.samples$State.Providence == "North Carolina", "North.Carolina",
                                     ifelse(ceratina.samples$State.Providence == "Rhode Island", "Rhode.Island",
                                     ifelse(ceratina.samples$State.Providence == "Nova Scotia", "Nova.Scotia",
                                     ifelse(ceratina.samples$State.Providence == "North Dakota", "North.Dakota",
                                     ifelse(ceratina.samples$State.Providence == "New Mexico", "New.Mexico",
                                     ifelse(ceratina.samples$State.Providence == "New Bunswick", "New.Bunswick",
                                     ifelse(ceratina.samples$State.Providence == "PQ", "Quebec",
                                     ifelse(ceratina.samples$State.Providence == "FI", "Florida",
                                     ifelse(ceratina.samples$State.Providence == "Main", "Maine",
                                     ifelse(ceratina.samples$State.Providence == "Florda", "Florida",
                                     ifelse(ceratina.samples$State.Providence == "District of Columbialumbia", "District.of.Columbia",
                                     ifelse(ceratina.samples$State.Providence == "St Catharines", "Ontario",  
                                     ifelse(ceratina.samples$State.Providence == "District of Columbia", "District.of.Columbia",
                                     ifelse(ceratina.samples$State.Providence == "New London", "Ontario",
                                     ifelse(ceratina.samples$State.Providence == "New York", "New.York",
                                     ifelse(ceratina.samples$State.Providence == "South Dakota", "South.Dakota",
                                     ifelse(ceratina.samples$State.Providence == "Miss", "Mississippi",
                                     ifelse(ceratina.samples$State.Providence == "Ont", "Ontario",
                                     ifelse(ceratina.samples$State.Providence == "ID", "Indiana",
                                     ifelse(ceratina.samples$State.Providence == "IL", "Illinois",
                                     ifelse(ceratina.samples$State.Providence == "MN", "Minnesota",
                                     as.character(ceratina.samples$State.Providence))))))))))))))))))))))

unique(ceratina.samples$State.Providence)



ceratina.samples$species <-   ifelse(ceratina.samples$species == "Calcarata", "calcarata", 
                              ifelse(ceratina.samples$species == "Calcarta", "calcarata",
                              ifelse(ceratina.samples$species == "ceratina calcarata", "calcarata",
                              ifelse(ceratina.samples$species == "ceratina strenua", "strenua",
                              ifelse(ceratina.samples$species == "Dupla", "dupla",
                              ifelse(ceratina.samples$species == "ceratina dupla", "dupla",
                              ifelse(ceratina.samples$species == "Strenua", "strenua",
                              ifelse(ceratina.samples$species == "Ceratina (Zadontomerus) calcarata", "calcarata",
                              ifelse(ceratina.samples$species == "Ceratina (Zadontomerus) strenua Smith, 1879", "strenua",
                              ifelse(ceratina.samples$species == "Ceratina (Zadontomerus) dupla dupla Say, 1837", "dupla",
                              ifelse(ceratina.samples$species == "Ceratina mikmaqi", "mikmaqi",
                              ifelse(ceratina.samples$species == "Ceratina (Zadontomerus) dupla Say, 1837", "dupla",
                              ifelse(ceratina.samples$species == "floridanum", "floridana",
                              ifelse(ceratina.samples$species == "Floridana", "floridana",
                              ifelse(ceratina.samples$species == "Calcarata", "calcarata",
                              ifelse(ceratina.samples$species == "miqmaki", "mikmaqi",
                              ifelse(ceratina.samples$species == "milkmaqi", "mikmaqi",
                              ifelse(ceratina.samples$species == "calcerata", "calcarata",
                              ifelse(ceratina.samples$species == "mikmiqi", "mikmaqi",
                              ifelse(ceratina.samples$species == "Calcarata", "calcarata",
                              ifelse(ceratina.samples$species == "Mikmagqi", "mikmaqi",
                              ifelse(ceratina.samples$species == "Cockerelli", "cockerelli",
                              as.character(ceratina.samples$species) ))))))))))))))))))))))



unique(ceratina.samples$species)    

ceratina.samples <- subset(ceratina.samples, !ceratina.samples$species %in% c("cockerelli"))

ceratina.samples$species <- ifelse(ceratina.samples$species %in% c("calcarata", "dupla", "strenua", "floridana", "mikmaqi"),
                                   as.character(ceratina.samples$species), "unknown" )



ceratina.samples <- subset(ceratina.samples, 
                           ceratina.samples$State.Providence != "?")


state.lat.long <- read.csv("C:/Users/evank/OneDrive/Desktop/States.Lat.Long.csv")
ceratina.samples$Latitude <- as.numeric(as.character(ceratina.samples$Latitude))

apply(state.lat.long["State.Province"], MARGIN = 1, function(state) {
  ceratina.samples$Latitude <<- ifelse(ceratina.samples$State.Providence == state, 
           state.lat.long[state.lat.long$State.Province == state, "Lat"],
         ceratina.samples$Latitude)
})

apply(state.lat.long["State.Province"], MARGIN = 1, function(state) {
  ceratina.samples$Longitude <<- ifelse(ceratina.samples$State.Providence == state, 
                                       state.lat.long[state.lat.long$State.Province == state, "Long"],
                                       ceratina.samples$Longitude)
})


# Subsetting data frames by year
ceratina.samples$Year <- as.numeric(as.character(ceratina.samples$Year))

ceratina.samples.2009 <- subset(ceratina.samples, ceratina.samples$Year >= 2009)

ceratina.samples.prior2009 <- subset(ceratina.samples, ceratina.samples$Year < 2009)
ceratina.samples.1990.2009 <- subset(ceratina.samples.prior2009, ceratina.samples.prior2009$Year >= 1990)

ceratina.samples.prior1990 <- subset(ceratina.samples, ceratina.samples$Year < 1990)
ceratina.samples.1970.1990 <- subset(ceratina.samples.prior1990, ceratina.samples.prior1990$Year >= 1970)

ceratina.samples.prior1970 <- subset(ceratina.samples, ceratina.samples$Year < 1970)





#Adding Contemporary samples to the museum specimens



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


contemporary.2009 <- rbind(Lat.Long[3:5], ceratina.samples.2009[c(6:7,11)])

                           