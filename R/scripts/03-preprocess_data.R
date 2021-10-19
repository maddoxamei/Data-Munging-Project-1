
column.types <- function(data){
  # Convert columns
  data$unitid <- as.character(data$unitid)
  
  # Convert the two numerically represented categorical variables into factors
  data$ccbasic <- as.factor(data$ccbasic)
  # Re-assign levels to condensed versions of the labels
  levels(data$ccbasic) <- gsub(".*: ", "", ccbasic.labels$label)
  
  return(data)
}



ccolumn.reorder <- function(data){
  print(colnames(data))
  colnames(data) <- gsub('([0-9]$)','0\\1',colnames(data))
  colnames(data) <- gsub('([0]$)','',colnames(data))
  data %<>% dplyr::relocate(sort(dplyr::current_vars()))
  return(data)
}

