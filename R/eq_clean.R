#' tremors
#' @docType data
#' @author Christopher Pearson
#' @references NOAA
#' @keywords data
"tremors"


#' eq_get_data_raw
#' @description reads data from quakes.txt file and returns it. Not exported
#' @importFrom readr read_delim
#' @return a tibble
eq_get_data_raw <- function(){
  return(readr::read_delim("C:\\Users\\Christopher\\Documents\\R\\Earthquakes\\inst\\extdata\\quakes.txt", delim = '\t',
                           col_types = "ccnnnnnnnnnnnnnnncccnnnnnnnnnnnnnnnnnnnnnnnnnnn"))
}

#' ymd_bc
#' @description given a date in string format, returns a date in date format.  Works with BC.  Can handle lists
#' @param date a date in string format or a list of dates in string format
#' @param ... extra variables that may work wtih the lubridate::date function.
ymd_bc <- function(date,...){
  if (length(date) >1){
    return(sapply(date,ymd_bc,...))
  }
  date2 <- strsplit(date,' ')[[1]]
  year <- as.numeric(date2[1])
  if (year< -1000){
    date <- lubridate::ymd(paste(year * -1, date2[2], date2[3]), ...)
    y <- lubridate::years(year * -1, ...)
    return(as.Date(date - y - y))
  }
  else if (year < 0){
    y <- year * -1 + 1000 # lubridate does not accept dates before 1000 AD
    y1000 <- lubridate::years(1000, ...)
    date <- lubridate::ymd(paste(y, date2[2], date2[3]), ...)
    y <- lubridate::years(y, ...) - y1000
    return(as.Date(date - y1000 - y - y))
  }
  else if (year < 1000){
    y1000 <- lubridate::years(1000, ...)
    y <- year + 1000
    date <- lubridate::ymd(paste(y, date2[2], date2[3]), ...)
    return(as.Date(date - y1000))
  }
  return(lubridate::ymd(date, ...))
}

#' eq_clean_data
#' @description given quake data read from quakes.txt, adds DATE column
#' @param data quake.txt data to be cleaned
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom lubridate ymd
#' @importFrom lubridate years
eq_clean_data <- function(data){

  data %>%
    dplyr::mutate(ADJUST_YEAR = ifelse(YEAR<1000,
                                       ifelse(YEAR<0,
                                              ifelse(YEAR< -1000, YEAR * -2, 1000 + YEAR * -2)
                                              ,1000)
                                       ,0) ) %>%
    dplyr::mutate(DATE = YEAR + ADJUST_YEAR) %>%
    dplyr::mutate(MONTH = ifelse(is.na(MONTH),1,MONTH)) %>%
    dplyr::mutate(DAY = ifelse(is.na(DAY),1,DAY)) %>%
    dplyr::mutate(DATE = paste(DATE, MONTH, DAY)) %>%
    dplyr::mutate(DATE = lubridate::ymd(DATE)) %>%
    dplyr::select(I_D, FLAG_TSUNAMI, DATE, everything()) %>%
    dplyr::mutate(ADJUST_YEAR = lubridate::years(ADJUST_YEAR)) %>%
    dplyr::mutate(DATE = DATE - ADJUST_YEAR) %>%
    dplyr::select(-ADJUST_YEAR) %>%
    dplyr::mutate(LATITUDE = as.numeric(LATITUDE)) %>%
    dplyr::mutate(LONGITUDE = as.numeric(LONGITUDE)) %>%
    return()
}

#' RemoveBefore
#' @description removes all characters from string before pattern rmv
#' @param str string from which to remove characters.  Works with lists of strings
#' @param rmv pattern marking where to cut
#' @importFrom stringr str_trim
#' @return string to return.  if is.na, returns 'ocrean', indicating that the quake occurred at sea (not an exported function for a reason)
RemoveBefore <- function(str,rmv){
  if (length(str) >1){
    return(sapply(str,RemoveBefore,rmv=rmv))
  }
  if (is.na(str)) return('ocean')
  index <- gregexpr(rmv,str)[[1]]
  if (index[1] == -1) return(str)
  index <- max(index)
  str<-substring(str,index+1)
  str<-stringr::str_trim(str)
  return(str)
}

#' eq_location_clean
#' @description given strings of location name, removes everything before the ':'
#' @param data quake data--designed for builtin dataset
#' @importFrom dplyr mutate
#' @return quake data with cleaned location names
eq_location_clean <- function(data){
  data %>%
    mutate(LOCATION_NAME = RemoveBefore(LOCATION_NAME, ':')) %>%
    return()
}

#' eq_get_data
#' @description reads data from quakes.txt, cleans data and location, then returns it
#' @export
#' @return a tibble of cleaned quake data
eq_get_data <- function(){
  eq_get_data_raw() %>%
    eq_clean_data %>%
    eq_location_clean %>%
    return()
}

#' eq_load_data
#' @description saves quake data as tremors.rda in data folder. No return. No parameters. Not exported.
#' @importFrom usethis use_data
eq_load_data <- function(){
  tremors <- eq_get_data()
  usethis::use_data(tremors, internal = FALSE, compress = 'xz', overwrite = TRUE)
}
