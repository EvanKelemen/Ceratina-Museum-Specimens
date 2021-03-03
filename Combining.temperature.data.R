#-------------------------------------------------------------------------------
# Associating Temperature data with morphological data
#-------------------------------------------------------------------------------
combining.temperature.data <- function(variable.folder, dataframe.to.use){
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

