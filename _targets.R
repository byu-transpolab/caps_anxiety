library(targets)

# Parallelization
library(future)
library(future.apply)
library(furrr)
library(future.callr)
plan(callr)
threads <- future::availableCores() - 1

source("R/Optimize.R")
source("R/Mental Health Table.R")

# Set target-specific options such as packages.
# Install gpsactivs from github with remotes::install_github("byu-transpolab/gpsactivs")
tar_option_set(packages = c("dplyr","tools", "hms", "lubridate", "gpsactivs",
                            "tidyverse", "leaflet", "sf", "purrr", "stringr",
                            "ggspatial", "data.table", "plotly", "future.apply",
                            "viridis", "pomp", "stats", "osmdata"))

# List of target objects
list(
  
  # Read in and clean GPS data
  tar_target(fileslist,
    command =
      list.files("data/caps_data", pattern = "*.csv", full.names = TRUE)
    ),
  
  tar_target(cleaned_data, process_caps_data(fileslist), resources = tar_resources(
    future = tar_resources_future(resources = list(n_cores = threads))
  )),
  
  # Make activity clusters using the optimized parameters
  tar_target(clustered_data, makeClusters(cleaned_manual_table = cleaned_data,
                                                params = c(11.7,3,300,1))),
  
  # Determine the number of trips made on a given day by a given user
  tar_target(num_trips, addNumTrips(clustered_data)),
  
  # Read in location files for parks, groceries, and libraries
  tar_target(parks_file, "data/resources/parks.geojson", format = "file"),
  tar_target(parksSf, sf::st_read(parks_file) %>% sf::st_transform(32612)),
  
  tar_target(grocery_file, "data/resources/groceries.geojson", format = "file"),
  tar_target(grocerySf, sf::st_read(grocery_file) %>% sf::st_transform(32612)),
  
  tar_target(library_file, "data/resources/libraries.geojson", format = "file"),
  tar_target(librarySf, sf::st_read(library_file) %>% sf::st_transform(32612)),
  
  # Determine number of trips for each activity type
  tar_target(activity_types, addTripType(num_trips, parksSf, grocerySf, librarySf)),
  
  # Add the demographic data
  tar_target(demographics_table, addDemographicData(activity_types)),
  
  # Add the mental health responses
  tar_target(final_table, addMentalHealthResponses(demographics_table))
)
