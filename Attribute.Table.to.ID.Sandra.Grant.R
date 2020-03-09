# Converting GIS summary table 

#Load in attribute Table
attributebuffer <- read.csv("C:/Users/evank/OneDrive/Desktop/SandraGrant/Buffer5.csv")

# Create a Dataframe of the Value IDs
attribute.ID <- data.frame(VALUE = c(-99,-9,1:8, 10:28),
                           ID = c("other","cloud/shadow", "clear open water",
                                  "turbid water", "shoreline", "mudflats",
                                  "marsh", "swamp", "fen", "bog", "heath",
                                  "sparse treed", "treed upland", "decidous treed",
                                  "mixed treed", "coniferous treed", 
                                  "plantations - treed cultivated", "hedge rows",
                                  "disturbance", "cliff and talus", "alvar",
                                  "sand barren and dunes", "open tallgrass prairie",
                                  "tallgrass savannah", "tallgrass woodland",
                                  "sand/gravel/mine tailings/extract",
                                  "bedrock", "community.infrastructure",
                                  "agriculture and undifferentiated rural land use")
)

# Use the ID.present to rename each column
ID.present <- unlist(strsplit(colnames(attributebuffer[3:length(attributebuffer)]) , split='_', fixed=TRUE))[seq(2,((length(attributebuffer)-2)*2),2)]

column.names <- as.character(unlist(lapply(ID.present, FUN = function(value){
  attribute.ID[ attribute.ID$VALUE == value, "ID"]
})))

colnames(attributebuffer) <- c("OIB", "OBJECTID", column.names)


# To makes sure all of the buffers have the same size
attributebuffer$sum <- attributebuffer[, 3] +  attributebuffer[,4] + attributebuffer[,5] +  
  attributebuffer[,6] + attributebuffer[,7] +  attributebuffer[,8] + attributebuffer[,9] +  
  attributebuffer[,10] + attributebuffer[,11] +  attributebuffer[,12] + attributebuffer[,13] +  
  attributebuffer[,14] + attributebuffer[,15] +  attributebuffer[,16] + attributebuffer[,17] +
  attributebuffer[,18] + attributebuffer[,19] 
  
attributebuffer <- attributebuffer[order(attributebuffer$community.infrastructure),]
attributebuffer$ranks <- c(1:length(attributebuffer[,"OIB"]))
attributebuffer$Percent.Urban <- (attributebuffer$community.infrastructure/attributebuffer$sum) * 100

write.csv(attributebuffer, "C:/Users/evank/OneDrive/Desktop/SandraGrant/bufferdistance.csv")


plot(attributebuffer$Percent.Urban ~ attributebuffer$ranks,
     tck = 0.02, cex = 2, pch =16)

#Identify which values are are in the dataframe (future column names)- Only works
# when all buffers are lumped together.
#column.names <- apply(attributebuffer["VALUE"], MARGIN = 1, FUN = function(value){
#  attribute.ID[ attribute.ID$VALUE == value, "ID"]
#})
# Remove columns extraneous columns
#attributebuffer <- attributebuffer[ ,3:length(attributebuffer) ]

# Tranpose the the dataframe
transpose.attriburebuffer <- as.data.frame(t(attributebuffer))
# Add the Column names
#colnames(transpose.attriburebuffer) <- column.names

# Add a column of the different 
#transpose.attriburebuffer$sum <- transpose.attriburebuffer[1] +
#  transpose.attriburebuffer[2] +
# transpose.attriburebuffer[3] +
#  transpose.attriburebuffer[4] +
#  transpose.attriburebuffer[5] +
#  transpose.attriburebuffer[6] +
#  transpose.attriburebuffer[7] +
#  transpose.attriburebuffer[8] +
#  transpose.attriburebuffer[9] +
#  transpose.attriburebuffer[10] +
#  transpose.attriburebuffer[11] +
#  transpose.attriburebuffer[12] 
  



#Load in attribute Table
attributebuffer <- read.csv("C:/Users/evank/OneDrive/Desktop/SandraGrant/Calgarybuffer_5.csv")




attribute.ID <- data.frame(VALUE = c(0:2,5:6, 8, 10:19),
                           ID = c("background","forest", "roads",
                                  "trees", "unmanaged grasslands", "treed wetlands",
                                  "managed grasslands", "wetland", "wetland shrub", "wetland herb", "forest wetland",
                                  "cropland", "other land", "Settlement",
                                  "water", "Forest Wetlands")
)



# Use the ID.present to rename each column
ID.present <- unlist(strsplit(colnames(attributebuffer[3:length(attributebuffer)]) , split='_', fixed=TRUE))[seq(2,((length(attributebuffer)-2)*2),2)]

column.names <- as.character(unlist(lapply(ID.present, FUN = function(value){
  attribute.ID[ attribute.ID$VALUE == value, "ID"]
})))

colnames(attributebuffer) <- c("OIB", "OBJECTID", column.names)


# To makes sure all of the buffers have the same size
attributebuffer$sum <- attributebuffer[, 3] +  attributebuffer[,4] + attributebuffer[,5] +  
  attributebuffer[,6] + attributebuffer[,7] +  attributebuffer[,8] + attributebuffer[,9] +  
  attributebuffer[,10] + attributebuffer[,11] +  attributebuffer[,12] 

attributebuffer <- attributebuffer[order(attributebuffer$community.infrastructure),]
attributebuffer$ranks <- c(1:length(attributebuffer[,"OIB"]))
attributebuffer$Percent.Urban <- (attributebuffer$community.infrastructure/attributebuffer$sum) * 100

write.csv(attributebuffer, "C:/Users/evank/OneDrive/Desktop/SandraGrant/bufferdistance_Calgary.csv")


