#### TIMELINE ####

#' draw_timeline_panel
#' @description panel function for geom_timeline.  NOT EXPORTED
#' @param data,panel_scales,coord data needed for panel function sent from ggproto instance
#' @importFrom grid pointsGrob
#' @importFrom grid segmentsGrob
#' @importFrom grid gTree
#' @importFrom grid gList
#' @importFrom grid gpar
#' @importFrom grid unit
#' @importFrom scales alpha
#' @importFrom grDevices rgb
#' @return timeline with points
draw_timeline_panel<- function(data, panel_scales, coord){
  coords <- coord$transform(data, panel_scales)
  length <- max(coords$x) - min(coords$x)
  horizontal <- coords$y
  seg <- grid::segmentsGrob(x0 = min(coords$x), x1 = max(coords$x),
                            y0 = horizontal, y1 = horizontal,
  )
  coords$colour[is.na(coords$colour)] <- grDevices::rgb(0.5,0,0)
  p <- grid::pointsGrob(x = coords$x,
                        y = horizontal,
                        size = grid::unit(2 * coords$size/(max(coords$size, na.rm=TRUE)), "char"),
                        pch= 21,
                        gp = grid::gpar(col = coords$colour, fill = scales::alpha(coords$colour, 0.5)),
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
#' @note aesthetics: x: dates for timeline; y: factor for timeline; colour: var for color; size: var for how large to make quakes on timeline
#' @return a timeline with points representing quakes
#' @export
#' @importFrom ggplot2 layer
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


GeomTimelineLabel <- ggproto("GeomTimelineLabel", Geom,
                             required_aes = c('x'),
                             default_aes = c(y = 0L, n_max = 5L, labels = "", size = NA),
                             draw_key = draw_key_blank,
                             draw_panel = function(data, panel_scales, coord){
                               coords <- coord$transform(data, panel_scales)
                               if (!all(is.na(coords$size))){
                                 ranks <- rank(coords$size, ties.method = 'random')
                                 coords <- coords[ranks <= data$n_max[1],]
                               }
                               coords <- coords[sample(1:length(coords$size),data$n_max[1]),]
                               segLength <- 1 / (5*(1 + max(coords$y, na.rm = TRUE)))
                               seg <- grid::segmentsGrob(x0 = coords$x, x1 = coords$x, y0 = coords$y, y1 = coords$y + segLength, default.units='npc')
                               t <- grid::textGrob(label = coords$label, x = coords$x, y = coords$y + segLength, just = 'left', rot = 45, default.units = 'npc')
                               rValue<- grid::gTree(children = grid::gList(seg, t))
                               return(rValue)
                             })

#' geom_timeline_label
#' @description creates labels for geom_timeline.  Goes with ggproto GeomTimelineLabel
#' @param stat argument for calling ggplot2::layer, defaults to 'identity', a statistical transformation to be applied to the data
#' @param na.rm whether to remove NAs from the data.  Defaults to FALSE
#' @param data,mapping,position,show.legend,inherit.aes,... additional arguments for geom creation
#' @note aesthetics: x: dates for timeline; y: factor for multiple timelines; labels: labels to place; n_max: max number of labels--in order of size or random if size not included
#' @return a graphical value containing labels and line-segments
#' @export
#' @importFrom ggplot2 layer
#' @importFrom grid textGrob
#' @importFrom grid segmentsGrob
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
