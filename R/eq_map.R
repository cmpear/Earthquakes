#' eq_map
#' @description creates a map marking the locations of earthquakes of the builtin tremors dataset
#' @param data the dataset or a subset of it with the quakes you want plotted
#' @param annot_col the columns from the dataset to use for annotations.  Alternatively, type popup_text to get Date, Magnitude and Deaths
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet addCircleMarkers
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @import tidyr
#' @export
#' @return a map (using leaflet) with the appropriate quake locations

eq_map <- function(data, annot_col = NA){
  if (annot_col == 'popup_text')
  {
    data<-data %>%
      dplyr::mutate(POPUP = paste(paste("<b>DATE:</b>",DATE),
                           paste("<b>MAGNITUDE:</b>",EQ_PRIMARY),
                           paste("<b>TOTAL DEATHS:</b>",DEATHS),
                           sep = '<br/>')  )
  }
  else if (annot_col == 'DATE'){
    data <-data%>%
      dplyr::mutate(POPUP = paste(Bold(annot_col),DATE, sep = ': '))
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
Bold <- function(str){
  return(paste0('<b>',str,'</b>'))
}
