library(dplyr)

rscorecard::sc_key(read.table("api_key.txt", sep="")[1,1])

# Grab the institution data dictionary information
csb.fields <- rscorecard::sc_dict('.', limit = Inf, print_off = TRUE, return_df = TRUE)

# Extract the relevant values (school classifications) in the ccbasic column
ccbasic.labels <- csb.fields[schools.idx(csb.fields$label), c("value", "label")]

# Condense releveant information into one vector
vars_to_pull <- c('unitid', 'instnm', 'ccbasic',
                  csb.fields$varname[c(tuition.idx(csb.fields$varname),
                                       earnings.idx(csb.fields$varname))] )

get.data <- function(year, vars.vector){
  print(paste("Grabbing data for",year))
  rscorecard::sc_init() %>%
    rscorecard::sc_filter(ccbasic %in% c(24:32)) %>%
    rscorecard::sc_select_(vars.vector) %>%
    rscorecard::sc_year(year) %>%
    rscorecard::sc_get()
}
