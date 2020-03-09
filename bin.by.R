bin.by.numeric <- function(what.to.bin.by, how.to.bin, what.to.bin, range.min = NA, range.max = NA){
  

# what.to.bin.by - the column to bin by
# how.to.bin - the increment to bin
# what.to.bin -the dataframe to bin by 
  
#what.to.bin[ , what.to.bin.by] Format to be able to index within the fuction
  
  
if(is.na(range.max)){
 buckets <- seq(min( what.to.bin[ , what.to.bin.by]), max( what.to.bin[ , what.to.bin.by]), how.to.bin)
 temp <- what.to.bin
 temp$id.num <- 1:nrow(temp)
 sapply(buckets, simplify = TRUE,  function( ind.bucket){
   temp.subsetted <- subset(temp, temp[ , what.to.bin.by] >= ind.bucket) 
   temp.subsetted <- subset(temp.subsetted, temp.subsetted[ , what.to.bin.by] < (ind.bucket + how.to.bin)) 
   what.to.bin[ temp.subsetted$id.num, paste0(what.to.bin.by, how.to.bin) ] <<- ind.bucket
 })

} else{
  buckets <- seq(range.min, range.max, how.to.bin)
  temp <- what.to.bin
  temp$id.num <- 1:nrow(temp)
  sapply(buckets, simplify = TRUE,  function( ind.bucket){
    temp.subsetted <- subset(temp, temp[ , what.to.bin.by] >= ind.bucket) 
    temp.subsetted <- subset(temp.subsetted, temp.subsetted[ , what.to.bin.by] < (ind.bucket + how.to.bin)) 
    what.to.bin[ temp.subsetted$id.num, paste0(what.to.bin.by, how.to.bin) ] <<- ind.bucket
  })
}
  
  print(what.to.bin)
}



bin.by.cat <- function(what.to.bin.by, what.to.bin, second.order.binning = NA, columns.to.match = NA){
  

  # what.to.bin.by - the column to bin by
  # how.to.bin - the increment to bin
  # what.to.bin -the dataframe to bin by 
  
  #what.to.bin[ , what.to.bin.by] Format to be able to index within the fuction
  
  
  if(!is.na(second.order.binning)){
  buckets <-  unique(what.to.bin[ , what.to.bin.by])
  second.order <<-  unlist(sapply(buckets, simplify = TRUE,  function( ind.bucket){
                unique(what.to.bin[what.to.bin[ , what.to.bin.by] == ind.bucket, second.order.binning])
    }))
  first.order <<-  unlist(sapply(buckets, simplify = TRUE,  function( ind.bucket){
             rep(ind.bucket,
                 length(unique(what.to.bin[what.to.bin[ , what.to.bin.by] == ind.bucket, second.order.binning])))
  }))
  new.data.frame <- data.frame( a = first.order, b = second.order, row.num = c(1:length(first.order)))
  sapply(new.data.frame[ , "row.num"], simplify = TRUE,  function( ind.bucket){
    temp <- subset(what.to.bin, what.to.bin[ , what.to.bin.by] == new.data.frame[ ind.bucket, "a"] ) 
    temp <- subset(what.to.bin, what.to.bin[ , second.order.binning] == new.data.frame[ ind.bucket, "b"] ) 
    new.data.frame[  ind.bucket, paste0(what.to.bin.by, "total") ] <<- nrow(temp)
  })
#    buckets <- unique(what.to.bin[ , what.to.bin.by])
#    temp <- what.to.bin
#    sapply(buckets, simplify = TRUE,  function( ind.bucket){
#      temp.subsetted <- subset(temp, temp[ , what.to.bin.by] == ind.bucket) 
#      new.data.frame[ new.data.frame[ , what.to.bin.by] == ind.bucket, paste0(columns.to.average, ".ave") ] <<- mean(temp.subsetted[ , columns.to.average])
#   })
    
  } else{
  new.data.frame <- data.frame( a = unique(what.to.bin[ , what.to.bin.by]))
  colnames(new.data.frame) <- what.to.bin.by
    buckets <-  unique(what.to.bin[ , what.to.bin.by])
 #   matching <- unique(columnes.to.match)
    sapply(buckets, simplify = TRUE,  function( ind.bucket){
      temp <- subset(what.to.bin, what.to.bin[ , what.to.bin.by] == ind.bucket) 
      new.data.frame[ new.data.frame[ , what.to.bin.by] == ind.bucket, paste0(what.to.bin.by, "total") ] <<- nrow(temp)
    })
  }
  
  print(new.data.frame)
}

#test.data <- data.frame(a = c("a", "a", "b", "b"), b = c(1, 10, 5, 3), c = c("test", "fish", "dog", "dog")) 

#test.data <- bin.by.numeric("b", 5, test.data, range.min = -5, range.max = 9)

#test.data.summary <- bin.by.cat("c", test.data, "b")
