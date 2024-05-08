# Load packages
library(osmdata)
library(ggspatial)
library(sf)
library(tidyverse)

# Define bbox for Utah County
bbox <- getbb("Utah County, Utah")

### LIBRARIES ###
# Define types of sustenance locations
library <- c('library')

# Get restaurant locations in Utah County
get_library <- opq(bbox) %>%
  add_osm_feature(key = 'amenity', value = library) %>%
  osmdata_sf() # Get a list of sf data frames for these tags

# Extract restaurant polygons
library_polygons <- pluck(get_library, "osm_polygons")

# Write polygons to GeoJSON file
st_write(library_polygons, "library.geojson")
