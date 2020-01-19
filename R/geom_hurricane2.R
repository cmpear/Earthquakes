## geom_hurricane starts at line
## Include Statements ####
#*******************************************************************
library(grid)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggmap)

## making new geom ####
#*******************************************************************
#' GeomHurricane
#' @description ggproto() for geom_hurricane
#' @param ne aes, radi for wind-speeds in ne
#' @param nw aes, radi for wind-speeds in nw
#' @param sw aes, radi for wind-speeds in sw
#' @param se aes, radi for wind-speeds in se
#' @param scale_radii aes, shrinks strom radi, ranges from 0 to 1
#' @return a geom for ggplot for a storm object
GeomHurricane<-ggproto("GeomHurricane",Geom,
                       required_aes = c("ne","nw","sw","se"),
                       default_aes = aes(x=0,y=0, scale_radii=1, lwd=1,
                                         colour = c("red","orange","yellow"),fill=c("red","orange","yellow")),
                       draw_key = draw_my_key,
                       draw_panel = GeomHurricane_draw_panel_function)

#' geom_hurricane
#' @description function for adding geom_hurricane() to a ggplot2 graphic
#' @param fill factor for fill color
#' @param color factor for border color
#' @param r_ne Radius for NE wind intesity
#' @param r_se Radius for SE wind intesity
#' @param r_sw Radius for SW wind intesity
#' @param r_nw Radius for NW wind intesity
#' @param scale_radii Passed to the stat_hurricane function; allows scaling of
#' @param ... the parameters to be passed on to GeomHurricane
#' @return a geom for ggplot for a storm object
geom_hurricane<-function(mapping = NULL, data = NULL, stat = "identity",
                         position = "identity", na.rm=FALSE,
                         show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomHurricane, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' GeomHurricane_draw_panel_function
#' @description the draw_panel function for Geom_Hurricane
#' @param data the data as processed by Geom_Hurricane
#' @param panel_scales a function for use with coord
#' @param coord an object for use with ggplot2 functions
GeomHurricane_draw_panel_function<-function(data,panel_scales,coord) {
  center<- c(data$x[1],data$y[1])
  data2 <- data.frame(matrix(unlist(data[1,c('x','y','PANEL')]), ncol=3, nrow=2, byrow=TRUE))
  names(data2)<-c('x','y','PANEL')

  data2$x[2] <- data2$x[2]+1
  data2$y[2] <- data2$y[2]+1

  coords<-coord$transform(data2,panel_scales)
  center <- c(coords$x[1],coords$y[1])
  adjust <- (c(coords$x[2],coords$y[2]) - center)  # NPC / one degree (long,lat)
  adjust <- adjust * c((Haversine(0, center[2],1, center[2]  , unit = "nautical"))^-1, 1/60) # * degrees per mile
  adjust <- adjust * data$scale_radii[1]
  ord <- order(data$ne,decreasing  = TRUE)
  return(ggCyclone(rNE=data$ne[ord],rNW=data$nw[ord],rSW=data$sw[ord],rSE=data$se[ord], fill = data$fill[ord], center=center,adjust=adjust))
}

#' draw_my_key
#' @description draws the key for geom_hurricane (normal draw function was causing repeated errors)
#' @param data a single row data frame containing the scaled aesthetics to display in this key--interested in fill
#' @param params a list of additional parameters supplied to the geom
#' @param size Width and height of key in mm

draw_my_key <- function (data, params, size)
{
  rectGrob(gp = gpar(col = NA, fill = alpha(data$fill, 0.6), lty = 1))
}

## geom helper functions ####
#*******************************************************************
#' Haversine
#' @description Returns distance between lat-long coordinates in km, meters, miles, feel or nautical miles
#' @param long1 first longitude value
#' @param lat1 first latitude value
#' @param long2 second longitude value
#' @param lat2 second latitude value
#' @param unit the unit to put the distance in
#' @return the distance between the two points
#' @example Haversine(89,20,54,-30, unit="nautical")
#' @export
Haversine <- function(long1,lat1,long2,lat2, unit = "km"){
  long1 <- long1 * pi/180
  long2 <- long2 * pi/180
  lat1 <- lat1 * pi/180
  lat2 <- lat2 * pi/180
  R = 6361000     # earth radius in meters
  delta_phi=lat2-lat1
  delta_lambda=long2-long1

  a=sin(delta_phi/2.0)^2+
    cos(lat1)*cos(lat2)*
    sin(delta_lambda/2.0)^2
  c=2*atan2(sqrt(a),sqrt(1-a))
  r = R * c
  if (unit == 'meters') return (r)
  r = r / 1000
  if (unit %in% c('km', 'kilometers')) return (r)
  r = r * 0.621371
  if (unit == 'miles') return (r)
  r = r / 5280
  if (unit == 'feet')  return (r)
  r = r * 5280 *.8689758
  if (unit %in% c("nm","nautical")) return(r)
  message(paste('not set up to handle inputs of',unit))
}

#' ggCyclone_helper1
#' @description compiles a dataset for the ggCyclone data to make a hurricane object with
#' @param rNE a list of 3 wind radi for the NE
#' @param rNW a list of 3 wind radi for the NW
#' @param rSW a list of 3 wind radi for the SW
#' @param rSE a list of 3 wind radi for the SE
#' @return a dataset for use in ggCyclone
ggCyclone_helper1 <- function(rNE,rNW,rSW,rSE){
  radians <- pi/2 * 0:24/24
  rNE <- rep(rNE, each=25) #24 +1 = 25
  rNW <- rep(rNW, each=25)
  rSW <- rep(rSW, each=25)
  rSE <- rep(rSE, each=25)
  windSpeeds <-                   ggCyclone_helper2 (radians         , rNE)
  windSpeeds <- rbind(windSpeeds, ggCyclone_helper2 (radians +   pi/2, rNW) )
  windSpeeds <- rbind(windSpeeds, ggCyclone_helper2 (radians +   pi  , rSW) )
  windSpeeds <- rbind(windSpeeds, ggCyclone_helper2 (radians + 3*pi/2, rSE) )
  order <- c(1:25,76:100,151:175,226:250)
  order <- c(order,order+25,order+50)
  windSpeeds<-windSpeeds[order,]
  return(windSpeeds)
}
#' ggCyclone_helper2
#' @description given lists of radi and radians, returns a dataframe of xy coordinates
#' @param radians a list of radians, should be same length as radius list
#' @param radius a list of radi, shoudl be same length as radians list
#' @return a dataframe of xy coordinates
ggCyclone_helper2 <- function(radians, radius){
  xList<-cos(radians) * radius
  yList<-sin(radians) * radius
  return(cbind(xList,yList))
}


#' ggCyclone
#' @description returns a geom_hurricane object given windspeeds, an adjust value, and center
#' @param adjust what to multiple wind speed radi by for the given map
#' @param center where to center the object
#' @param ... rNE, rNW, rSW, rSE for use with ccGyclone_helper1()
#' @return a hurricane object for geom_hurricane
ggCyclone <- function(...,adjust = c(1,1), center = c(0.5,0.5), fill = c("red","orange","yellow"))
{
  windSpeeds<-ggCyclone_helper1(...)
  windSpeeds[,1]<-windSpeeds[,1] * adjust[1] + center[1]
  windSpeeds[,2]<-windSpeeds[,2] * adjust[1] + center[2]
  poly1<-pathGrob(x=windSpeeds[1:200,1],y=windSpeeds[1:200,2],
                  id=rep(1:2, each=100),
                  gp=gpar(col=NA,fill=fill[1],alpha=0.5),
                  rule="evenodd")
  poly2<-pathGrob(x=windSpeeds[101:300,1],y=windSpeeds[101:300,2],
                  id=rep(1:2, each=100),
                  gp=gpar(col=NA,fill=fill[2],alpha=0.5),
                  rule="evenodd")
  poly3<-pathGrob(x=windSpeeds[201:300,1],y=windSpeeds[201:300,2],
                  gp=gpar(col=NA,fill=fill[3],alpha=0.5),
                  rule="evenodd")

  hurricane <- gTree(children = gList(poly1, poly2, poly3))

  return(hurricane)
}



## GET DATA
ext_tracks_file <- "Coursera-JohnHopkinsCourse/data/StormData_1988_2015.txt"
ext_tracks_widths <- c( 7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3, 4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day", "hour", "year", "latitude","longitude",
                         "max_wind", "min_pressure", "rad_max_wind", "eye_diameter", "pressure_1", "pressure_2",
                         paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                         "storm_type", "distance_to_land", "final")
ext_tracks_coltypes <- "cciiiiddiiiiiiddddddddddddci?"

ext_tracks <-read_fwf(ext_tracks_file,fwf_widths(ext_tracks_widths,ext_tracks_colnames), na = "-99", col_types=ext_tracks_coltypes)

## Clean and Wrangle Data
# would have been much better to keep all windspeed data in the same row
ext_tracks %<>%
  mutate(longitude = -longitude) %>%
  mutate(storm_id = paste(storm_name,year,sep='-')) %>%
  select(-max_wind,-min_pressure,-rad_max_wind,-eye_diameter,-pressure_1,-pressure_2,-storm_type,-distance_to_land,-final) %>%
  mutate(date = make_datetime(year=year,month=month,day=day,hour=hour)) %>%
  select(-month,-day,-hour,-year) %>%
  arrange(storm_id,storm_name,date)

p1 <- ext_tracks%>%
  mutate(wind_speed = 50) %>%
  rename(ne=radius_50_ne, nw=radius_50_nw,sw=radius_50_sw,se=radius_50_se) %>%
  select(-radius_34_ne     ,-radius_34_nw  ,-radius_34_sw  ,-radius_34_se,
         -radius_64_ne     ,-radius_64_nw  ,-radius_64_sw  ,-radius_64_se)

ext_tracks<-ext_tracks %>%
  select(-radius_50_ne,-radius_50_nw,-radius_50_sw,-radius_50_se)

p2 <- ext_tracks%>%
  mutate(wind_speed = 64) %>%
  rename( ne=radius_64_ne, nw=radius_64_nw,sw=radius_64_sw,se=radius_64_se) %>%
  select(   -radius_34_ne   ,-radius_34_nw  ,-radius_34_sw  ,-radius_34_se)
ext_tracks %<>%
  mutate(wind_speed = 34) %>%
  select(-radius_64_ne     ,-radius_64_nw  ,-radius_64_sw  ,-radius_64_se) %>%
  rename( ne=radius_34_ne, nw=radius_34_nw,sw=radius_34_sw,se=radius_34_se) %>%
  rbind(p1,p2) %>%
  select(storm_id,storm_name,date,latitude,longitude,wind_speed,ne,se,sw,nw)

ext_tracks %>%
  filter(storm_name=="ALBERTO")
rm(p1)
rm(p2)
## Ike Data and Visuals####
#*******************************************************************
register_google(key="AIzaSyBYoTIS_NZiJBZdNS284I1bYniRWFk1Myo")
ike <- ext_tracks %>%
  filter(storm_name == "IKE") %>%
  arrange(date) %>%
  filter(ne>0, longitude< -83) %>%
  slice(1:3, 25:27, 43:45)
ike
loc <- unlist(ike[7,5:4])
somewhere <- get_map(loc, source='stamen',maptype="toner",zoom=5)

ggmap(somewhere)

ike$wind_speed <- as.character(ike$wind_speed)

pdf(file="HurricaneIke.pdf")
ggmap(somewhere, extent = "device") + ggtitle("Hurricane Ike, Single DateTime ") +
  geom_hurricane(data = ike[7:9,], aes(x=longitude,y=latitude,ne = ne, nw = nw, se = se, sw = sw, fill=wind_speed)) +
   scale_fill_manual(name = 'Wind speed (kts)',
                     values = c('red', 'orange', 'yellow'))

ggmap(somewhere, extent = "device") + ggtitle("Hurricane Ike, Single DateTime ") +
  geom_hurricane(data = ike[7:9,], aes(x=longitude,y=latitude,ne = ne, nw = nw, se = se, sw = sw, scale_radii=0.5,fill=wind_speed)) +
  scale_fill_manual(name = 'Wind speed (kts)',
                    values = c('red', 'orange', 'yellow'))

dev.off()
