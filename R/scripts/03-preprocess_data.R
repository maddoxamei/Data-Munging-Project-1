
column.types <- function(data){
  # Convert columns
  data$unitid <- as.character(data$unitid)
  
  # Convert the two numerically represented categorical variables into factors
  data$ccbasic <- as.factor(data$ccbasic)
  # Re-assign levels to condensed versions of the labels
  levels(data$ccbasic) <- gsub(".*: ", "", ccbasic.labels$label)
  
  return(data)
}


column.reorder <- function(data){
  print(colnames(data))
  colnames(data) <- gsub('([0-9]$)','0\\1',colnames(data))
  colnames(data) <- gsub('([0]$)','',colnames(data))
  data %<>% dplyr::relocate(sort(dplyr::current_vars()))
  return(data)
}

inflation.year.adjustment <- function(data){
  years <- tibble::as_tibble(data[["year"]])
  colnames(years) <- "original"
  years$adjusted <- sapply(years$original, function(x){
    if(x <= 2011) 2014
    else if(x >= as.numeric(format(Sys.Date(), "%Y"))) as.numeric(format(Sys.Date(), "%Y"))
    else if(x >= 2018) x+1 
    else x+3
  })
  return(years)
}

inflation.adjustment <- function(data, years){
  for( col in tuition.idx(colnames(data))){
    print(colnames(data)[col])
    data[,col] <- priceR::adjust_for_inflation(data[,col], years[["original"]], "US", to_date = 2020)
  }

  for( col in earnings.idx(colnames(data))){
    print(colnames(data)[col])
    data[,col] <- priceR::adjust_for_inflation(data[,col], years[["adjusted"]], "US", to_date = 2020)
  }
  
  return(data)
}



