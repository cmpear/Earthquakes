## code to prepare `DATASET` dataset goes here

library(dplyr)
library(tidyr)
library(lubridate)
library(readr)

quakes <- read_delim("C:/Users/cmp53/Documents/R/Earthquakes/Data/quakes.txt", delim = '\t')

# A date column created by uniting the year, month, day and converting it to the Date class
# LATITUDE and LONGITUDE columns converted to numeric class
# In addition, write a function eq_location_clean() that
#  cleans the LOCATION_NAME column by stripping out the country
#  name (including the colon) and converts names to title case
#  (as opposed to all caps). This will be needed later for annotating
#  visualizations. This function should be applied to the raw data to
#  produce a cleaned up version of the LOCATION_NAME column.

ymdNA <- function(...,quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"),
                  truncated = 0){
  vars <- list(...)
  if (length(vars[[1]])>1) return(apply(vars,1,ymdNA,quiet,tz,locale,truncated))
  for (i in 1:length(vars)){
    if (is.na(vars[i])) vars[i] <- 1
  }
  if (rValue == "") rValue <- NA
  else rValue <- substr(rValue,2,nchar(rValue))
  return(rValue)
}

eq_clean_data <- function(data){

  data %>%
    mutate(MONTH = 1 * ifelse(is.na(MONTH),1,MONTH))
    mutate(DAY = ifelse(is.na(YEAR),1,YEAR)) %>%
    mutate(LATITUDE = numeric(LATITUDE)) %>%
    mutate(LONGITUDE = numeric(LONGITUDE)) %>%
    return()
}

eq_location_clean <- function(data){

}


usethis::use_data(quakes, compress = 'xz')
