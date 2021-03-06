% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eq_map.R
\name{eq_map}
\alias{eq_map}
\title{eq_map}
\usage{
eq_map(data, annot_col = NA)
}
\arguments{
\item{data}{the dataset or a subset of it with the quakes you want plotted}

\item{annot_col}{the columns from the dataset to use for annotations.  Alternatively, type popup_text to get Date, Magnitude and Deaths}
}
\value{
a map (using leaflet) with the appropriate quake locations
}
\description{
creates a map marking the locations of earthquakes of the builtin tremors dataset
}
\examples{
m<-tremors \%>\%
   dplyr::select(COUNTRY, LATITUDE, LONGITUDE, EQ_PRIMARY, DATE, YEAR, DEATHS) \%>\%
   dplyr::filter(COUNTRY == "JAPAN", YEAR >= 2010) \%>\%
   Earthquakes::eq_map(annot_col = "popup_text")
   m
}
