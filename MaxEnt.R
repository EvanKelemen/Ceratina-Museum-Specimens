#-------------------------------------------------------------------------------
# MaxEnt for Temperature Model
#-------------------------------------------------------------------------------
#Install package dismo to run MaxEnt
#install.packages("dismo")
#maxent.jar needs to go into the java folder of dismo, it can be found using the
#following code
#system.file("java", package="dismo")
#Requried packages
#install.packages("rJava")

library(dismo)
library(raster)
library(rJava)

#Create occurance data, dataframe morph.data
source("C:/Users/evank/OneDrive/Desktop/Maps/Ceratina-Maps/morphologicaldata.R")
morph.data.maxent <- subset(morph.data,! is.na(morph.data[,"Lat"]))
morph.data.maxent <- subset(morph.data.maxent, morph.data.maxent[,"Specimen.Year"] > 1999)

# our predictor rasters 
pred_files <- list.files("C:/Users/evank/Documents/ArcGIS/Projects/Temperature (that opens)/wc2.1_30s_bio_WorldClim", full.names=TRUE)
predictors <- stack(pred_files)

# Occurance Data

occurance <- data.frame(Lat = morph.data[, "Lat"], Long = morph.data[, "Long"])

# X is the predictor rasters
# p is the occurance data
# factors are which variable in x (if it is a raster, should be treated as catagorical)
#maxent(x, p, factors=NULL)
maxent.model.1 <- maxent(predictors, occurance, factors=NULL)

# Plots the importance of each variable
plot(maxent.model.1)

#Predict the enitre dataset
predict.maxent.model.1 <- predict(maxent.model.1, predictors)
