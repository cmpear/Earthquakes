#### HELPER FUNCTIONS ####

#' ymd_bc
#' @description given a date in string format, returns a date in date format.  Works with BC.  Can handle lists
#' @param date a date in string format or a list of dates in string format
#' @param ... extra variables that may work wtih the lubridate::date function.
#' @examples
#'   test_data <- c('-2000 01 01', '500 05 30','1990 07 04')
#'   Earthquakes:::ymd_bc(test_data)
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


#' RemoveBefore
#' @description removes all characters from string before pattern rmv
#' @param str string from which to remove characters.  Works with lists of strings
#' @param rmv pattern marking where to cut
#' @importFrom stringr str_trim
#' @return string to return.  if is.na, returns 'ocrean', indicating that the quake occurred at sea (not an exported function for a reason)
#' @note NOT EXPORTED
#' @example dy <- Earthquakes:::RemoveBefore('howdy', 'd')
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

#### DATA RETRIEVAL AND CLEANING FUNCTIONS ####

#' eq_get_data_raw
#' @description reads data from quakes.txt file and returns it.
#' @importFrom readr read_delim
#' @export
#' @return a tibble
#' @example Earthquakes::eq_get_data_raw()
eq_get_data_raw <- function(){
  return(readr::read_delim(system.file('extdata','quakes.txt', package = 'Earthquakes'),
                           delim = '\t', col_types = 'ccnnnnnnnnnnnnnnncccnnnnnnnnnnnnnnnnnnnnnnnnnnn'))
}



#' eq_clean_data
#' @description given quake data read from quakes.txt, adds DATE column
#' @param data quake.txt data to be cleaned
#' @export
#' @import tidyr
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom lubridate ymd
#' @importFrom lubridate years
#' @importFrom rlang .data
#' @examples
#' data <- Earthquakes::eq_get_data_raw() %>%
#'   Earthquakes::eq_clean_data() %>%
#'   Earthquakes::eq_location_clean()
eq_clean_data <- function(data){

  data %>%
    dplyr::mutate("ADJUST_YEAR" = ifelse(.data$YEAR<1000,
                                       ifelse(.data$YEAR<0,
                                              ifelse(.data$YEAR< -1000, .data$YEAR * -2, 1000 + .data$YEAR * -2)
                                              ,1000)
                                       ,0) ) %>%
    dplyr::mutate("DATE" = .data$YEAR + .data$ADJUST_YEAR) %>%
    dplyr::mutate("MONTH" = ifelse(is.na(.data$MONTH),1,.data$MONTH)) %>%
    dplyr::mutate("DAY" = ifelse(is.na(.data$DAY),1,.data$DAY)) %>%
    dplyr::mutate("DATE" = paste(.data$DATE, .data$MONTH, .data$DAY)) %>%
    dplyr::mutate("DATE" = lubridate::ymd(.data$DATE)) %>%
    dplyr::select("I_D", "FLAG_TSUNAMI", "DATE", everything()) %>%
    dplyr::mutate("ADJUST_YEAR" = lubridate::years(.data$ADJUST_YEAR)) %>%
    dplyr::mutate("DATE" = .data$DATE - .data$ADJUST_YEAR) %>%
    dplyr::select(-"ADJUST_YEAR") %>%
    dplyr::mutate("LATITUDE" = as.numeric(.data$LATITUDE)) %>%
    dplyr::mutate("LONGITUDE" = as.numeric(.data$LONGITUDE)) %>%
    return()
}


#' eq_location_clean
#' @description given strings of location name, removes everything before the ':'
#' @param data quake data--designed for builtin dataset
#' @export
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @import tidyr
#' @return quake data with cleaned location names
#' @examples
#' data <- Earthquakes::eq_get_data_raw() %>%
#'   Earthquakes::eq_clean_data() %>%
#'   Earthquakes::eq_location_clean()
eq_location_clean <- function(data){
  data %>%
    mutate("LOCATION_NAME" = RemoveBefore(.data$LOCATION_NAME, ':')) %>%
    return()
}

#### CONSOLIDATED RETRIEVE & CLEAN FUNCTIONS ####

#' eq_get_data
#' @description reads data from quakes.txt, cleans data and location, then returns it
#' @export
#' @import tidyr
#' @return a tibble of cleaned quake data
#' @example data <- Earthquakes::eq_get_data()
eq_get_data <- function(){
  eq_get_data_raw() %>%
    eq_clean_data %>%
    eq_location_clean %>%
    return()
}

#' eq_load_data
#' @description saves quake data as tremors.rda in data folder. No return. No parameters. Not exported.
#' @importFrom usethis use_data
#' @note DO NOT RUN!!! consider only if tremors.rda has a problem.  DO NOT RUN!!!  For setup purposes only!!!
#' @example \dontrun{eq_load_data()}
eq_load_data <- function(){
  tremors <- eq_get_data()
  usethis::use_data(tremors, internal = FALSE, compress = 'xz', overwrite = TRUE)
}
