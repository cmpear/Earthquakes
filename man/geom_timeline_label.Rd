% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eq_geomtimeline.R
\name{geom_timeline_label}
\alias{geom_timeline_label}
\title{geom_timeline_label}
\usage{
geom_timeline_label(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
)
}
\arguments{
\item{data, mapping, position, show.legend, inherit.aes, ...}{additional arguments for geom creation}

\item{stat}{argument for calling ggplot2::layer, defaults to 'identity', a statistical transformation to be applied to the data}

\item{na.rm}{whether to remove NAs from the data.  Defaults to FALSE}
}
\value{
a graphical value containing labels and line-segments
}
\description{
creates labels for geom_timeline.  Goes with ggproto GeomTimelineLabel
}
\note{
aesthetics: x: dates for timeline; y: factor for multiple timelines; labels: labels to place; n_max: max number of labels--in order of size or random if size not included
}
\examples{
tremors \%>\%
  dplyr::filter(YEAR >= 2000 ) \%>\%
  dplyr::filter(COUNTRY \%in\% c('CHINA','JAPAN','KOREA')) \%>\%
  dplyr::select(DATE, COUNTRY, LOCATION_NAME, EQ_PRIMARY, DEATHS) \%>\%
  dplyr::mutate(COUNTRY = as.factor(COUNTRY)) \%>\%
  ggplot(aes(x = DATE, y = COUNTRY, labels = LOCATION_NAME,
   size = EQ_PRIMARY, col = DEATHS, n_max = 4)) +
   Earthquakes::geom_timeline() + Earthquakes:::geom_timeline_label()
}
