#-------------------------------------------------------------------------------
# Associating Temperature data with morphological data
#-------------------------------------------------------------------------------

combining.temperature.data <- function(variable.folder, dataframe.to.use){
  temp.files <- list.files(path = paste0("C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Temperature Variable/",
                            variable.folder), pattern = "*.csv")
  temp <- do.call(rbind, lapply(temp.files, function(file.name){
    read.csv(paste0("C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Temperature Variable/",
                    variable.folder,"/", file.name))
  }))
  unlist(sapply(dataframe.to.use[, "Sample"], function(sample.name){
    if(length(temp[ temp$Sample == as.character(sample.name), "RASTERVALU"]) == 0){
      NA} else{
        temp[ temp$Sample == as.character(sample.name), "RASTERVALU"] 
      }
  }))
}

#>>> Testing if the function runs
#combining.temperature.data("Yearly Average", morph.data)

#>>> Testing if all points recorded at temperature 
#temp.files.test <- list.files(path = paste0("C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Temperature Variable/",
#                                       "Yearly Average"), pattern = "*.csv")
#temp.test <- do.call(rbind, lapply(temp.files.test, function(file.name){
#  read.csv(paste0("C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Temperature Variable/",
#                  "Yearly Average","/", file.name))
#}))

#temp.test[ temp.test$RASTERVALU < -100, "Sample"]




combining.temperature.data.cru <- function(variable.folder, dataframe.to.use){
  temp.files <- list.files(path = paste0("C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Temperature Variable CRU/",
                                         variable.folder), pattern = "*.csv")
  temp <- do.call(rbind, lapply(temp.files, function(file.name){
    read.csv(paste0("C:/Users/evank/Documents/ArcGIS/Projects/Temperature/Temperature Variable CRU/",
                    variable.folder,"/", file.name))
  }))
  unlist(sapply(dataframe.to.use[, "Sample"], function(sample.name){
    if(length(temp[ temp$Sample == as.character(sample.name), "RASTERVALU"]) == 0){
      NA} else{
        temp[ temp$Sample == as.character(sample.name), "RASTERVALU"] 
      }
  }))
}

