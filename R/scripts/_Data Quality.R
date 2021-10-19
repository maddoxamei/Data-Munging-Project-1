
#Define the function w.r.t. data frame x, prints the proportion of data not missing/NA
missing_test <- function(x){
  # Boolean columns
  
  # Numeric columns
  x_prime <- apply(x, 2, function(z) z[!is.finite(z)] <- NA)     #Changes any non-finite vals to NA
  n_tot <- 0                                    #Initialize a counter for the number of NA's
  n_tot <- apply(x, 2, function(z) n_tot <- n_tot + sum(is.na(z)))         #Count the NA's contained in each column and output a list, each entry is the proportion of data (as decimal) in columns that is NOT missing
  print((nrow(x) - n_tot)/nrow(x))                    #Print result
  
  # Categorical columns
}

# Define a function that outputs proportion of data with desired size, with input data frame x and desired amount of data y (chosen default is 100)
size_test <- function(x, y = 100){
  dt_size <- apply(x, 2, function(z) min(length(z) / y, y / length(z)))         #Takes the minimum of the column length over the desired data size y and the reciprocal, the closer to 1 the more 'appropriate' the data size
  print(dt_size)              #Print result
}


#Test against the data set iris
missing_test(iris)

#Add a column to iris consisting of the numbers 1 to 149 and NaN, eval with missing_test
iris2 <- iris
iris2$Empty <- append(seq(1,149), NaN)
View(iris2)

missing_test(iris2)

#Test against iris2 y = Default, y = 75, y = 300
size_test(iris2)
size_test(iris2, 75)
size_test(iris2, 300)

#For y = 75 or 300 the test shows that the data is twice as large or half as big, respectively, as desired