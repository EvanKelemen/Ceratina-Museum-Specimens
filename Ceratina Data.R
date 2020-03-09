#-------------------------------------------------------------------------------
# Ballparking locations
#-------------------------------------------------------------------------------

#>> SUMMARIZING BY STATE
# Load in data 
ceratina.samples <- read.csv("C:/Users/evank/OneDrive/Desktop/Maps/Ceratina Data/Ceratina Samples.csv")


# Subset to the C. calcarata, dupla, strenua, mikmaqi, cockerelli
# Only relevant for ALL DATA
#ceratina.samples <- subset(ceratina.samples, 
#                           !ceratina.samples$species %in% c("nanula", "apacheorum", "citriphila", 
#                                                            "cyaniventris", "arizonensis", "acantha/nanula",
#                                                            "callidum", "andreniformis", "neomexicana", 
#                                                            "mocsaryi", "acantha", "cyanea", "guarnacciana",
#                                                            "smaragdula", "Ceratina palavensis Yasumatsu"))

# Correct spelling errors
ceratina.samples$State.Province <- ifelse(ceratina.samples$State.Province == "NS", "Nova.Scotia", 
                                     ifelse(ceratina.samples$State.Province == "CT", "Connecticut",
                                     ifelse(ceratina.samples$State.Province == "DC", "District.of.Columbia",
                                     ifelse(ceratina.samples$State.Province == "GA", "Georgia",
                                     ifelse(ceratina.samples$State.Province == "KY", "Kentucky",
                                     ifelse(ceratina.samples$State.Province == "MA", "Massachusetts",
                                     ifelse(ceratina.samples$State.Province == "MD", "Maryland",
                                     ifelse(ceratina.samples$State.Province == "ME", "Maine",
                                     ifelse(ceratina.samples$State.Province == "MI", "Michigan",
                                     ifelse(ceratina.samples$State.Province == "NC", "North.Carolina",
                                     ifelse(ceratina.samples$State.Province == "NH", "New.Hampshire",
                                     ifelse(ceratina.samples$State.Province == "NJ", "New.Jersey",
                                     ifelse(ceratina.samples$State.Province == "NY", "New.York",
                                     ifelse(ceratina.samples$State.Province == "OH", "Ohio",
                                     ifelse(ceratina.samples$State.Province == "PA", "Pennsylvania",
                                     ifelse(ceratina.samples$State.Province == "RI", "Rhode.Island",
                                     ifelse(ceratina.samples$State.Province == "TN", "Tennesse",
                                     ifelse(ceratina.samples$State.Province == "DE", "Delaware",
                                     ifelse(ceratina.samples$State.Province == "IN", "Indiana",
                                     ifelse(ceratina.samples$State.Province == "VA", "Virginia",
                                     ifelse(ceratina.samples$State.Province == "WI", "Wisconsin",
                                     ifelse(ceratina.samples$State.Province == "WV", "West.Virginia",
                                     ifelse(ceratina.samples$State.Province == "CA", "California",
                                     ifelse(ceratina.samples$State.Province == "SC", "South.Carolina",
                                     ifelse(ceratina.samples$State.Province == "NE", "Nebraska",
                                     ifelse(ceratina.samples$State.Province == "ALTA", "Georgia",
                                     ifelse(ceratina.samples$State.Province == "Tenn", "Tennesse",
                                     ifelse(ceratina.samples$State.Province == "MD.", "Maryland",
                                     ifelse(ceratina.samples$State.Province == "MICH", "Michigan",
                                     ifelse(ceratina.samples$State.Province == "Fla", "Florida",
                                     ifelse(ceratina.samples$State.Province == "Md", "Maryland",
                                     ifelse(ceratina.samples$State.Province == "Washington DC", "District.of.Columbia",
                                     ifelse(ceratina.samples$State.Province == "MO", "Missouri",
                                     ifelse(ceratina.samples$State.Province == "Minn", "Minnesota",
                                     ifelse(ceratina.samples$State.Province == "MS", "Mississippi",
                                     ifelse(ceratina.samples$State.Province == "COLO", "Colorado",
                                     ifelse(ceratina.samples$State.Province == "NB", "New.Bunswick",
                                     ifelse(ceratina.samples$State.Province == "AR", "Arkansas",
                                     ifelse(ceratina.samples$State.Province == "ARK", "Arkansas",
                                     ifelse(ceratina.samples$State.Province == "TX", "Texas",
                                     ifelse(ceratina.samples$State.Province == "AZ", "Arizona",
                                     ifelse(ceratina.samples$State.Province == "Pennsylvania ", "Pennsylvania",
                                     ifelse(ceratina.samples$State.Province == "MT", "Montana",
                                     ifelse(ceratina.samples$State.Province == "Quebec ", "Quebec",
                                     ifelse(ceratina.samples$State.Province == "NF", "New.Foundland",
                                     ifelse(ceratina.samples$State.Province == "New Hampshire", "New.Hampshire",
                                     ifelse(ceratina.samples$State.Province == "New Jersey", "New.Jersey",
                                     ifelse(ceratina.samples$State.Province == "West Virginia", "West.Virginia",
                                     ifelse(ceratina.samples$State.Province == "South Carolina", "South.Carolina",
                                     as.character(ceratina.samples$State.Province))))))))))))))))))))))
                                     ))))))))))))))))))))))))))))
ceratina.samples$State.Province <- ifelse(ceratina.samples$State.Province == "North Carolina", "North.Carolina",
                                     ifelse(ceratina.samples$State.Province == "Rhode Island", "Rhode.Island",
                                     ifelse(ceratina.samples$State.Province == "Nova Scotia", "Nova.Scotia",
                                     ifelse(ceratina.samples$State.Province == "North Dakota", "North.Dakota",
                                     ifelse(ceratina.samples$State.Province == "New Mexico", "New.Mexico",
                                     ifelse(ceratina.samples$State.Province == "New Bunswick", "New.Bunswick",
                                     ifelse(ceratina.samples$State.Province == "PQ", "Quebec",
                                     ifelse(ceratina.samples$State.Province == "FI", "Florida",
                                     ifelse(ceratina.samples$State.Province == "Main", "Maine",
                                     ifelse(ceratina.samples$State.Province == "Florda", "Florida",
                                     ifelse(ceratina.samples$State.Province == "District of Columbialumbia", "District.of.Columbia",
                                     ifelse(ceratina.samples$State.Province == "St Catharines", "Ontario",  
                                     ifelse(ceratina.samples$State.Province == "District of Columbia", "District.of.Columbia",
                                     ifelse(ceratina.samples$State.Province == "New London", "Ontario",
                                     ifelse(ceratina.samples$State.Province == "New York", "New.York",
                                     ifelse(ceratina.samples$State.Province == "South Dakota", "South.Dakota",
                                     ifelse(ceratina.samples$State.Province == "Miss", "Mississippi",
                                     ifelse(ceratina.samples$State.Province == "Ont", "Ontario",
                                     ifelse(ceratina.samples$State.Province == "ID", "Indiana",
                                     ifelse(ceratina.samples$State.Province == "IL", "Illinois",
                                     ifelse(ceratina.samples$State.Province == "MN", "Minnesota",
                                     as.character(ceratina.samples$State.Province))))))))))))))))))))))
# See what states and province are left
unique(ceratina.samples$State.Province)


# Correcting species name errors
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


# Check unique species
unique(ceratina.samples$species)    
# Removing cockerelli
ceratina.samples <- subset(ceratina.samples, !ceratina.samples$species %in% c("cockerelli"))

ceratina.samples$species <- ifelse(ceratina.samples$species %in% c("calcarata", "dupla", "strenua", "floridana", "mikmaqi"),
                                   as.character(ceratina.samples$species), "unknown" )



