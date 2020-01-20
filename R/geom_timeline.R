library(grid)
library(ggplot2)

GeomTimeline <- ggproto("GeomTimeline", geom,
                        required_aes = c('x'),
                        default_aes = aes(y = 0, color = 'grey',size = 1, alpha = 1),
                        draw_key = draw_timeline_key(),
                        draw_panel = draw_timeline_panel)

geom_timeline <- function(mapping = NULL, data = NULL, stat = 'identify',
                          position = 'identity', na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

draw_earthquake_key <- function (data, params, size){
  rectGrob(gp = gpar(col = NA, fill = alpha(data$fill, 0.6), lty = 1))
}

draw_timeline_panel<- function(data, panel_scales, coord){

  coords <- coord$transform(data, panel_scales)
  print(str(coords))
  length <- max(coords$x) - min(coords$x)
  seg <- segmentsGrob(x0 = min(coords$x), x1 = max(coords$x),
               y0 = coords$y, y1 = coords$y,
               )
  p <- pointsGrob(x = coords$x,
             y = coords$y,
             size = unit(coords$size, "char"),
             pch= 21,
             gp = gpar(col = coords$color, fill = coords$color, alpha = coords$alpha),
             vp = NULL)
  t <- gTree(children = gList(seg, p))
  return(t)
}

ggplot(data = quakes, aes(x = ))
