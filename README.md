# Earthquakes
<!-- badges: start -->
[![Travis build status](https://travis-ci.com/cmpear/Earthquakes.svg?branch=master)](https://travis-ci.org/cmpear/Earthquakes)
<!-- badges: end -->
This package is built for retrieving, cleaning, analyzing and visualizing earthquake data gathered by the National Oceanic and Atmospheric Administration.  Presenting this package does three things:
# Retrieve and Clean Data
This package has the raw quake data from the NOAA stored as a text file in /inst/ext/quakes.txt; and a cleaned version of the data stored in /data/tremors.rda (there is already a builtin dataset named 'quakes').  This package also includes several functions for cleaning the raw data.
# Create geom_timeline and geom_timeline_labels
This package has two geoms for creating a timeline visualizing the quake data.  It also supports creating several timelines simultaneously by including a y variable in the aesthetics.  The labels geom simply adds labels to a n_max cases from the data (the n_max largest or random).
# Creates a map file for visualizing quake data
This package also includes an interacting mapping function using leaflet for visualizing quake data geospatially.
