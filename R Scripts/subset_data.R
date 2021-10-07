earnings.idx <- function(csb.data, reduced=TRUE){
  if( !reduced ) return( c(which(colnames(csb.2021)=="MN_EARN_WNE_P10"):
                             which(colnames(csb.2021)=="GT_25K_P9")) )
  else{
    earn.mean <- grep('MN_EARN_WNE_P[10,8,6]', colnames(csb.data))
    earn.med <- grep('MD_EARN_WNE_P[10,8,6]', colnames(csb.data))
    #earn.p <- grep('PCT[0-9]._EARN_WNE_P[10,8,6]', colnames(csb.data))
    earn.sd <- grep('SD_EARN_WNE_P[10,8,6]', colnames(csb.data))
    return( c(earn.mean, earn.med, earn.sd) )
  }
}

degree.idx <- function(csb.data){
  return( grep("PCIP", colnames(csb.data)) )
}

tuition.idx <- function(csb.data){
  return( c(which(colnames(csb.data)=="NPT4_PUB"):
              which(colnames(csb.data)=="TUITIONFEE_PROG")) )
}

subset.data <- function(csb.data, size=1000, seed=NULL, ...){
  cols.idx <- c(earnings.idx(csb.data, ...), 
                degree.idx(csb.data), 
                tuition.idx(csb.data))
  
  if( !is.null(seed) ) set.seed(seed)
  rows.idx <- sample(nrow(csb.data), size)
  
  return( csb.data[rows.idx, cols.idx] )
}

csb.2021 <- csb21.sub

col.idx <- c(which(colnames(csb.2021)=="MN_EARN_WNE_P10"):
               which(colnames(csb.2021)=="GT_25K_P9"), grep("PCIP", colnames(csb.2021)), which(colnames(csb.2021)=="NPT4_PUB"):which(colnames(csb.2021)=="TUITIONFEE_PROG"))
csb21.sub <- csb.2021[,col.idx]

tuit.idx <- which(colnames(csb21.sub)=="COSTT4_A"):which(colnames(csb21.sub)=="TUITIONFEE_OUT")

tuit.sub <- csb21.sub[,tuit.idx]