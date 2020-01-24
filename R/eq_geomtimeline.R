#### TIMELINE ####

#' draw_timeline_panel
#' @description panel function for geom_timeline.  NOT EXPORTED
#' @param data,panel_scales,coord data needed for panel function sent from ggproto instance
#' @importFrom grid pointsGrob
#' @importFrom grid segmentsGrob
#' @importFrom grid gTree
#' @importFrom grid gList
#' @importFrom grid gpar
#' @return timeline with points
draw_timeline_panel<- function(data, panel_scales, coord){
  coords <- coord$transform(data, panel_scales)
  print(str(coords))
  length <- max(coords$x) - min(coords$x)
  horizontal <- coords$y
  seg <- grid::segmentsGrob(x0 = min(coords$x), x1 = max(coords$x),
                            y0 = horizontal, y1 = horizontal,
  )
  coords$colour[is.na(coords$colour)] <- rgb(0.5,0,0)
  p <- grid::pointsGrob(x = coords$x,
                        y = horizontal,
                        size = unit(2 * coords$size/(max(coords$size, na.rm=TRUE)), "char"),
                        pch= 21,
                        gp = grid::gpar(col = coords$colour, fill = alpha(coords$colour, 0.5)),
                        vp = NULL)
  t <- grid::gTree(children = grid::gList(seg, p))
  return(t)
  return('y')
}

GeomTimeline <- ggproto("GeomTimeline", Geom,
                        required_aes = c('x'),
                        default_aes = aes(y = 0L, colour = 'red',size = 1, alpha = 1),
                        draw_key = draw_key_polygon,
                        draw_panel = draw_timeline_panel)

#' geom_timeline
#' @description creates a timeline from quake data.  Goes wtih ggproto GeomTimeline
#' @inheritParams ggplot2::stat_identity
#' @param stat argument for calling ggplot2::layer, defaults to 'identity', a statistical transformation to be applied to the data
#' @param na.rm whether to remove NAs from the data.  Defaults to FALSE
#' @param x the dates for the timeline
#' @param y factor for timeline, each y value creates seperate timeline.  Can cause errors if not at least set to 0 in aes
#' @param colour what var to use to determine quake colour
#' @param size how large to make each quake circle on the timeline
#' @return a timeline with points representing quakes
#' @export
#' @importFrom ggplot2 layer
#' @examples tremors <- eq_get_data()
#'           ggplot(data = tremors[1000:1050,], aes(x = DATE, y = as.factor(COUNTRY),labels = COUNTRY,size = INTENSITY,color = DEATHS, alpha = 0.5)) +
#'             geom_timeline()
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

############ TIMELINE LABEL ####

#' geom_timeline_label
#' @description creates labels for geom_timeline.  Goes with ggproto GeomTimelineLabel
#' @inheritParams ggplot2::stat_identity
#' @param stat argument for calling ggplot2::layer, defaults to 'identity', a statistical transformation to be applied to the data
#' @param na.rm whether to remove NAs from the data.  Defaults to FALSE
#' @param x the dates for the timeline
#' @param y factor for timeline, each y value creates seperate timeline.  Can cause errors if not at least set to 0 in aes
#' @param labels how to label different quakes.  Defaults to ""
#' @param size how large the quakes are.  Used with n_max to determine which quakes to label first
#' @param n_size how many quakes to label.  Largest quakes are labeled first-otherwise random
#' @return a graphical value containing labels and line-segments
#' @export
#' @importFrom ggplot2 layer

GeomTimelineLabel <- ggproto("GeomTimelineLabel", Geom,
                             required_aes = c('x'),
                             default_aes = c(y = 0L, n_max = 5L, labels = "", size = NA),
                             draw_key = draw_key_blank,
                             draw_panel = function(data, panel_scales, coord){
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
