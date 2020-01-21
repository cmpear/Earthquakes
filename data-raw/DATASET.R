## code to prepare `DATASET` dataset goes here

library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)
library(tibble)

tremors <- read_delim("C:\\Users\\Christopher\\Documents\\R\\Earthquakes\\data\\quakes.txt", delim = '\t',
                     col_types = "ccnnnnnnnnnnnnnnncccnnnnnnnnnnnnnnnnnnnnnnnnnnn")
#                                     5   10   15   20
#                    col_types = "ccnnnnncccccccccccccccccccccccccccccccccccccccc")
# having parsing errors, so will take in as char and convert to numeric later

# A date column created by uniting the year, month, day and converting it to the Date class
# LATITUDE and LONGITUDE columns converted to numeric class
# In addition, write a function eq_location_clean() that
#  cleans the LOCATION_NAME column by stripping out the country
#  name (including the colon) and converts names to title case
#  (as opposed to all caps). This will be needed later for annotating
#  visualizations. This function should be applied to the raw data to
#  produce a cleaned up version of the LOCATION_NAME column.


eq_clean_data <- function(data){

  data %>%
    mutate(ADJUST_YEAR = ifelse(YEAR<1000,
                                ifelse(YEAR<0,
                                       ifelse(YEAR< -1000, YEAR * -2, 1000 + YEAR * -2)
                                       ,1000)
                                ,0) ) %>%
    mutate(YEAR = YEAR + ADJUST_YEAR) %>%
    mutate(MONTH = ifelse(is.na(MONTH),1,MONTH)) %>%
    mutate(DAY = ifelse(is.na(DAY),1,DAY)) %>%
    mutate(YEAR = paste(YEAR, MONTH, DAY)) %>%
    mutate(YEAR = ymd(YEAR)) %>%
    rename(DATE = YEAR) %>%
    mutate(ADJUST_YEAR = lubridate::years(ADJUST_YEAR)) %>%
    mutate(DATE = DATE - ADJUST_YEAR) %>%
    select(-MONTH, -DAY, -ADJUST_YEAR) %>%
    mutate(LATITUDE = as.numeric(LATITUDE)) %>%
    mutate(LONGITUDE = as.numeric(LONGITUDE)) %>%
    return()
  # do this next part without tidyr...try making column first then re-inserting 238 pre-1000ad columns one by one
}

RemoveBefore <- function(str,rmv){
  if (length(str) >1){
    return(sapply(str,RemoveBefore,rmv=rmv))
  }
  if (is.na(str)) return('ocean')
  index <- gregexpr(rmv,str)[[1]]
  if (index[1] == -1) return(str)
  index <- max(index)
  str<-substring(str,index+1)
  str<-str_trim(str)
  return(str)
}

eq_location_clean <- function(data){
  data %>%
    mutate(LOCATION_NAME = RemoveBefore(LOCATION_NAME, ':')) %>%
    return()
}

ymd_bc <- function(date,...){
  if (length(date) >1){
    return(sapply(date,ymd_bc,...))
  }
  date2 <- strsplit(date,' ')[[1]]
  year <- as.numeric(date2[1])
  if (year< -1000){
    date <- ymd(paste(year * -1, date2[2], date2[3]))
    y <- lubridate::years(year * -1)
    return(as.Date(date - y - y))
  }
  else if (year < 0){
    y <- year * -1 + 1000 # lubridate does not accept dates before 1000 AD
    y1000 <- lubridate::years(1000)
    date <- ymd(paste(y, date2[2], date2[3]))
    y <- lubridate::years(y) - y1000
    return(as.Date(date - y1000 - y - y))
  }
  else if (year < 1000){
    y1000 <- lubridate::years(1000)
    y <- year + 1000
    date <- ymd(paste(y, date2[2], date2[3]))
    return(as.Date(date - y1000))
  }
  return(ymd(date))
}

tremors <- tremors %>%
  eq_location_clean() %>%
  eq_clean_data()
  usethis::use_data(tremors, internal = FALSE, compress = 'xz', overwrite = TRUE)
# whenever I run this, I get an undersized dataframe...avoid for now?
