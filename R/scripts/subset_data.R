earnings.idx <- function(csb.names, reduced=TRUE){
  csb.names <- toupper(csb.names)
  if( !reduced ) return( c(which(csb.names=="MN_EARN_WNE_P10"):
                             which(csb.names=="GT_25K_P9")) )
  else{
    earn.mean <- grep('MN_EARN_WNE_P[10,8,6]', csb.names)
    earn.med <- grep('MD_EARN_WNE_P[10,8,6]', csb.names)
    #earn.p <- grep('PCT[0-9]._EARN_WNE_P[10,8,6]', csb.names)
    earn.sd <- grep('SD_EARN_WNE_P[10,8,6]', csb.names)
    return( c(earn.mean, earn.med, earn.sd) )
  }
}

degree.idx <- function(csb.names){
  csb.names <- toupper(csb.names)
  return( grep("PCIP", csb.names) )
}

tuition.idx <- function(csb.names){
  csb.names <- toupper(csb.names)
  return( c(which(csb.names=="COSTT4_A"):which(csb.names=="TUITIONFEE_OUT")) )
}

sample.data <- function(csb.data, size=1000, seed=NULL){
  if( !is.null(seed) ) set.seed(seed)
  return( csb.data[sample(nrow(csb.data), size),] )
}

subset.data <- function(csb.data, size=1000, seed=NULL, ...){
  cols.idx <- c(earnings.idx(colnames(csb.data), ...), 
                degree.idx(colnames(csb.data)), 
                tuition.idx(colnames(csb.data)))
  
  if( !is.null(seed) ) set.seed(seed)
  rows.idx <- sample(nrow(csb.data), size)
  
  return( csb.data[rows.idx, cols.idx] )
}
