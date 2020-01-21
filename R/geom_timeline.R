library(grid)
library(ggplot2)

# GeomHurricane<-ggproto("GeomHurricane",Geom,
#                        required_aes = c("ne","nw","sw","se"),
#                        default_aes = aes(x=0,y=0, scale_radii=1, lwd=1,
#                                          colour = c("red","orange","yellow"),fill=c("red","orange","yellow")),
#                        draw_key = draw_my_key,
#                        draw_panel = GeomHurricane_draw_panel_function)

# geom_hurricane<-function(mapping = NULL, data = NULL, stat = "identity",
#                          position = "identity", na.rm=FALSE,
#                          show.legend = NA, inherit.aes = TRUE, ...)


GeomTimelineLabel <- ggproto("GeomTimelineLabel", Geom,
                            required_aes = c('x'),
                            default_aes = c(y = 0L, n_max = 5L, labels = "", size = NA),
                            draw_key = draw_key_blank,
                            draw_panel = function(data, panel_scales, coord){
#                              print('howdy, labeling time')
#                              print(data)
#                              print(coord)
#                              print(coord$transform)
#                              print(panel_scales)
                              coords <- coord$transform(data, panel_scales)
                              print(str(coords))
                              print('howdy')
                              if (!all(is.na(coords$size))){
                                ranks <- rank(coords$size, ties.method = 'random')
                                coords <- coords[ranks <= data$n_max[1],]
                              }
                              coords <- coords[sample(1:length(coords$size),data$n_max[1]),]
                              segLength <- 1 / (5*(1 + max(coords$y, na.rm = TRUE)))
                              print(coords)
                              seg <- segmentsGrob(x0 = coords$x, x1 = coords$x, y0 = coords$y, y1 = coords$y + segLength, default.units='npc')
                              t <- textGrob(label = coords$label, x = coords$x, y = coords$y + segLength, just = 'left', rot = 45, default.units = 'npc')
                              rValue<- gTree(children = gList(seg, t))
                              return(rValue)
                            })

geom_timeline_label <- function(mapping = NULL, data = NULL, stat = 'identity',
                                position = 'identity', na.rm = FALSE,
                                show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimelineLabel, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

GeomTimeline <- ggproto("GeomTimeline", Geom,
                        required_aes = c('x'),
                        default_aes = aes(y = 0L, colour = 'red',size = 1, alpha = 1),
                        draw_key = draw_key_polygon,
                        draw_panel = draw_timeline_panel)

geom_timeline <- function(mapping = NULL, data = NULL, stat = 'identity',
                          position = 'identity', na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# draw_timeline_key <- function (data, params, size){
#   rectGrob(gp = gpar(col = NA, fill = alpha(data$fill, 0.6), lty = 1))
# }

draw_timeline_panel<- function(data, panel_scales, coord){
  coords <- coord$transform(data, panel_scales)
  print(str(coords))
  length <- max(coords$x) - min(coords$x)
  horizontal <- coords$y
  seg <- segmentsGrob(x0 = min(coords$x), x1 = max(coords$x),
               y0 = horizontal, y1 = horizontal,
               )
  coords$colour[is.na(coords$colour)] <- rgb(0.5,0,0)
  p <- pointsGrob(x = coords$x,
             y = horizontal,
             size = unit(2 * coords$size/(max(coords$size, na.rm=TRUE)), "char"),
             pch= 21,
             gp = gpar(col = coords$colour, fill = alpha(coords$colour, 0.5)),
             vp = NULL)
  t <- gTree(children = gList(seg, p))
  return(t)
}

ggplot(data = tremors[1000:1050,], aes(x = DATE, y = as.factor(COUNTRY),labels = COUNTRY,size = INTENSITY,color = DEATHS, alpha = 0.5)) +
  geom_timeline() + geom_timeline_label()

ggplot(data = tremors[900:1000,], aes(x = DATE, y=0, labels = COUNTRY,size = INTENSITY,color = DEATHS, alpha = 0.5)) +
  geom_timeline() + geom_timeline_label()


#ggplot(data = tremors[200:300,], aes(x = DATE, labels = COUNTRY,size = INTENSITY,color = DEATHS, alpha = 0.5)) +
#  geom_timeline() + geom_timeline_label()

