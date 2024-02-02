library(targets)

# Parallelization
library(future)
library(future.apply)
library(furrr)
library(future.callr)
plan(callr)
threads <- future::availableCores() - 1

source("R/Process GPS to Trips.R")
source("R/Create Table.R")
source("R/Estimate Models.R")

# Set target-specific options such as packages.
# Install gpsactivs from github with remotes::install_github("byu-transpolab/gpsactivs")
tar_option_set(packages = c("dplyr","tools", "hms", "lubridate", "gpsactivs",
                            "tidyverse", "leaflet", "sf", "purrr", "stringr",
                            "ggspatial", "data.table", "plotly", "future.apply",
                            "viridis", "pomp", "stats", "osmdata"))

# List of target objects
list(
  
  # Create list of GPS csv files
  tar_target(fileslist,
    command =
      list.files("data/caps_data", pattern = "*.csv", full.names = TRUE)),
  
  # Read in the data
  tar_target(read_in, read_data(fileslist), resources = tar_resources(
    future = tar_resources_future(resources = list(n_cores = threads)))),
  
  # Remove the irrelevant userIds
  tar_target(cleaned_ids, clean_userids(read_in)),
  
  # Implement the scoring algorithm to have higher quality userId activityDay combos
  tar_target(scored_days, scoring(cleaned_ids)),
  
  # Clean the remaining GPS data before creating activity clusters
  tar_target(cleaned_data, gps_points_process(scored_days)),
  
  # Read in the demographic data
  tar_target(demographics, readDemographicData("data/mental_surveys/Demographic_Breakdown.xlsx")),
  
  # List of userIds that are not high scoring
  tar_target(high_scoring_ids, find_high_scorers(cleaned_data, column_name)),
  
  # Clean the demogrphic data by only keeping high scoring userIds
  tar_target(cleaned_demo, demo_data_process(demographics, high_scoring_ids)),
  
  # Create frame of userIds and date
  tar_target(frame, make_frame(cleaned_demo, "2019-05-01", "2022-03-01")),
  
  # Add the demographic data to the frame
  tar_target(demo_data, addDemographicData(frame, cleaned_demo)),
  
  # Add the mental health responses to the demographic data
  tar_target(survey_data, addMentalHealthResponses(demo_data)),

  # Make activity clusters using the optimized parameters
  tar_target(clustered_data, makeClusters(cleaned_data, params = c(11.7,3,300,1))),
  
  # Determine the number of trips made on a given day by a given user
  tar_target(num_trips, addNumTrips(clustered_data)),
  
  # Read in location files for parks, groceries, and libraries
  tar_target(parks_file, "data/resources/parks.geojson", format = "file"),
  tar_target(parksSf, sf::st_read(parks_file) %>% sf::st_transform(32612)),
  
  tar_target(grocery_file, "data/resources/groceries.geojson", format = "file"),
  tar_target(grocerySf, sf::st_read(grocery_file) %>% sf::st_transform(32612)),
  
  tar_target(library_file, "data/resources/libraries.geojson", format = "file"),
  tar_target(librarySf, sf::st_read(library_file) %>% sf::st_transform(32612)),
  
  # Determine the number of trips for each activity type
  tar_target(activity_types, addTripType(num_trips, parksSf, grocerySf, librarySf)),
  
  # Make a final table with all of the data
  tar_target(complete_table, combine_data(survey_data, activity_types)),
  
  # Keep rows that have survey responses
  tar_target(cleaned_table, clean_table(complete_table)),
  
  # Estimate models
  tar_target(models, estimate_models(cleaned_table))
)
