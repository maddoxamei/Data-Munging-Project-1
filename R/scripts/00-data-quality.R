
#Define the function w.r.t. data frame x, prints the proportion of data not missing/NA
missing_test <- function(x){
  # Boolean column
  if (is.logical(x) == T){
    t_total <- sum(x)
    na_total <- sum(is.na(x))
    f_total <- length(x) - t_total - na_total
    tab_x <- matrix(c(t_total, f_total, na_total), nrow = 1, ncol = 3)
    colnames(tab_x) <- c("True", "False", "Null")
    cat("Column Type: Boolean", "\n")
    if(na_total == 0)
      cat("No NA Values Found", "\n")
    print(prop.table(tab_x)*100)   #Table that gives number of True, False and Null values as percentage of total values
  }
  
  # Numeric column
  if (is.numeric(x) == T) {
    x[!is.finite(x)] <- NA     #Changes any non-finite vals to NA
    na_total <- sum(is.na(x))                  
    val_total <- length(x) - na_total
    tab_x <- matrix(c(val_total, na_total), nrow = 1, ncol = 2)
    colnames(tab_x) <- c("Accepted Value", "Null/Infinite Value")
    cat("Column Type: Numeric", "\n")
    if(na_total == 0)
      cat("No NA Values Found", "\n")
    print(prop.table(tab_x)*100)
  }
  
  # Categorical columns
  if (is.character(x) == T | is.factor(x) == T){
    na_total <- sum(is.na(x))
    if(na_total != 0){
      table_x <- table(x, useNA = "always")
      cat("Column Type: Character or Factor", "\n")
      print(prop.table(table_x)*100)
    }else{
      cat("Column Type: Character or Factor", "\n", "No NA Values Found", "\n")
      print(prop.table(table(x))*100)
    }
  }
}

na.test <- function(x){
  for (i in colnames(x)){
    cat("\n","Percentage of Values for", i, "\n")
    missing_test(x[[i]])
  }
}


## Test code using csb.data
# na.test(csb.data[!colnames(csb.data) %in% c("unitid", "instnm", "year")])

  