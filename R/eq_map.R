#' eq_map
#' @description creates a map marking the locations of earthquakes of the builtin tremors dataset
#' @param data the dataset or a subset of it with the quakes you want plotted
#' @param annot_col the columns from the dataset to use for annotations.  Alternatively, type popup_text to get Date, Magnitude and Deaths
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet addCircleMarkers
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @import tidyr
#' @export
#' @return a map (using leaflet) with the appropriate quake locations
#' @examples
#' m<-tremors %>%
#'    dplyr::select(COUNTRY, LATITUDE, LONGITUDE, EQ_PRIMARY, DATE, YEAR, DEATHS) %>%
#'    dplyr::filter(COUNTRY == "JAPAN", YEAR >= 2010) %>%
#'    Earthquakes::eq_map(annot_col = "popup_text")
#'    m

eq_map <- function(data, annot_col = NA){
  if (annot_col == 'popup_text')
  {
    data<-data %>%
      dplyr::mutate("POPUP" = paste(paste("<b>DATE:</b>",.data$DATE),
                           paste("<b>MAGNITUDE:</b>",.data$EQ_PRIMARY),
                           paste("<b>TOTAL DEATHS:</b>",.data$DEATHS),
                           sep = '<br/>')  )
  }
  else if (annot_col == 'DATE'){
    data <-data%>%
      dplyr::mutate("POPUP" = paste(Bold(annot_col),.data$DATE, sep = ': '))
  }
  m <- leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(data$EQ_PRIMARY,lat = data$LATITUDE, lng = data$LONGITUDE, popup = data$POPUP, color = 'blue', weight = 1) %>%
    return()
}

#' Bold
#' @description adds html <b> ... </b> to string
#' @param str string to add html bolding to
#' @return the bolded string
#' @example Earthquakes:::Bold('howdy')
Bold <- function(str){
  return(paste0('<b>',str,'</b>'))
}
