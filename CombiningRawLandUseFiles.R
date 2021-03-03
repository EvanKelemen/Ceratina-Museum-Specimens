#-------------------------------------------------------------------------------
# Combining files to create Land Use Data
#-------------------------------------------------------------------------------
library(dplyr)


# Function to combine US land use data within a bin
combine.land.use.binned.data <- function(year, distance){
  template.head <- read.csv("C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Land Use Specimens/Points_DecadeAfter/points_combind.csv")
  
  temp.files <- list.files(path =
                             paste0("C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Land Use Specimens/Points_DecadeAfter/Output/", 
                                    year, "/"),
                           pattern = "*.csv")
  
  temp <- dplyr::bind_rows(template.head[1,],lapply(temp.files, function(file.name){
    read.csv(paste0("C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Land Use Specimens/Points_DecadeAfter/Output/", year,
                    "/", file.name), colClasses=c(SAMPLE="character", "numeric"))
    }))
  
  temp$Decade <- year
  temp <- temp[2:length(temp[, "Decade"]) ,]
  temp <- subset(temp, !is.na(temp[2]))
  write.csv(temp, paste0("C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Land Use Specimens/Points_DecadeAfter/Output/",
                         distance, "/US/" , year, ".csv"))
}

#Combined all the binned data
combine.land.use.across.binned <- function(distance){
  temp.files <- list.files(path =
                             paste0("C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Land Use Specimens/Points_DecadeAfter/Output/", 
                                    distance, "/US/"),
                           pattern = "*.csv")
  temp <-   dplyr::bind_rows(lapply(temp.files, function(file.name){
    read.csv(paste0("C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Land Use Specimens/Points_DecadeAfter/Output/", 
                    distance, "/US/", file.name))
    }))
  write.csv(temp, paste0("C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Land Use Specimens/Points_DecadeAfter/Output/",
                         distance, "/US/combined.csv"))
}

# Combine all the data within a bin Canada
combine.land.use.binned.data.ca <- function(year, distance){
  template.head <- read.csv("C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Land Use Specimens/Canada/combine_points.csv")
  
  temp.files <- list.files(path =
                             paste0("C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Land Use Specimens/Points_DecadeAfter/Output/", 
                                    year, "/"),
                           pattern = "*.csv")
  
  temp <- dplyr::bind_rows(template.head[1,],lapply(temp.files, function(file.name){
    read.csv(paste0("C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Land Use Specimens/Points_DecadeAfter/Output/", year,
                    "/", file.name))
  }))
  
  temp$Decade <- year
  temp <- temp[2:length(temp[, "Decade"]) ,]
  temp <- subset(temp, !is.na(temp[2]))
  write.csv(temp, paste0("C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Land Use Specimens/Points_DecadeAfter/Output/",
                         distance, "/Canada/" , year, ".csv"))
}

#combine Data across bins canada
combine.land.use.across.binned.ca <- function(distance){
  temp.files <- list.files(path =
                             paste0("C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Land Use Specimens/Points_DecadeAfter/Output/", 
                                    distance, "/Canada/"),
                           pattern = "*.csv")
  temp <-   dplyr::bind_rows(lapply(temp.files, function(file.name){
    read.csv(paste0("C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Land Use Specimens/Points_DecadeAfter/Output/", 
                    distance, "/Canada/", file.name))
  }))
  write.csv(temp, paste0("C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Land Use Specimens/Points_DecadeAfter/Output/",
                         distance, "/Canada/combined.csv"))
}


year.us <- c(1974, 1982, 1992, 2002, 2012)
year.ca <- c(1990, 2000, 2010)
meters <- "1000meterprevious"


for (year in year.us) {
  print(year)
  combine.land.use.binned.data(year, meters) 
}

combine.land.use.across.binned(meters) 


for (year in year.ca) {
  print(year)
  combine.land.use.binned.data.ca(year, meters) 
}

combine.land.use.across.binned.ca(meters) 

