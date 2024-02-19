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
source("R/optimization.R")

# Set target-specific options such as packages.
# Install gpsactivs from github with remotes::install_github("byu-transpolab/gpsactivs")
tar_option_set(packages = c("dplyr","tools", "hms", "lubridate", "gpsactivs",
                            "tidyverse", "leaflet", "sf", "purrr", "stringr",
                            "ggspatial", "data.table", "plotly", "future.apply",
                            "viridis", "pomp", "stats", "osmdata",
                            "future", "furrr"),
               memory = "transient",
               garbage_collection = TRUE)

# set a seed for the dynamic branching (see "Optimization")
# https://books.ropensci.org/targets/random.html#reproducibility
tar_option_set(seed = 3)


# List of target objects
list(
  
  # Create list of GPS csv files
  tar_target(fileslist,
    command =
      list.files("data/caps_data", pattern = "*.csv", full.names = TRUE)),
  
  # Read in the data GPS csv files
  tar_target(read_in, read_data(fileslist), resources = tar_resources(
    future = tar_resources_future(resources = list(n_cores = threads)))),
  
  # Subset for TESTING
  # tar_target(subset, dplyr::slice_sample(read_in, prop = 0.01)),
  
  # Optimization ==========
  # This set of targets deals with the optimization analysis. First,
  # we load both the manually labeled and raw GPS data, and join them into 
  # a common nested dataframe
  tar_target(labeled_files, "data/labeled_pgons", format = "file"),
  tar_target(unlabeled_files, "data/unlabeled_geojson", format = "file"),
  tar_target(labeled_data, read_labeled_data(labeled_files)),
  tar_target(unlabeled_data, read_unlabeled_data(unlabeled_files)),
  tar_target(optim_frame, make_optim_frame(labeled_data, unlabeled_data)),
  
  # Run the simulated annealing algorithm with a set of starting parameters
  # we want to start the process in multiple different places, so we are going
  # to use target's dynamic branching, described in https://books.ropensci.org/targets/dynamic.html#example
  # first, generate a handful of starting values for each parameter
  tar_target(radius, sample(10:100, 3)),
  tar_target(minpts, sample(3:300, 3)),
  tar_target(deltat, sample(300:(12*3600), 3)),
  tar_target(entrop, sample(seq(1, 4, by = 0.2), 2)),
  
  # The `cross` function makes a set of parameters by combining all combinations of
  # the drawn values. So targets will run optimize_sann as many times as you 
  # give sets of parameters for. In this case, I am presently running 
  # 3*3*3*2 = 54
  # different optimizations
  tar_target(sann, 
             optimize_sann(optim_frame, radius, minpts,  
                           deltat, entrop),
             pattern = cross(radius, minpts, deltat, entrop),
  ),
  tar_target(sann_results, process_sann_results(sann)),
  
  
  # SUMMARIZE THE RAW DATA
  tar_target(raw_data_summary, summarize_raw_data(read_in)),
  
  # Remove the irrelevant userIds of those who weren't study participants
  tar_target(cleaned_ids, clean_userids(read_in)),
  
  # Preprocess the data
  tar_target(preprocessed, preprocessing(cleaned_ids)),
  
  # SUMMARIZE THE RAW DATA
  tar_target(filtered_data_summary, summarize_filtered_data(preprocessed)),
  
  # Implement the scoring algorithm to have higher quality userId activityDay combos
  tar_target(scored_days, scoring(preprocessed)),
  
  # SUMMARIZE THE SCORED DATA
  tar_target(scored_data_summary, summarize_scored_data(scored_days)),
  
  # Clean the remaining GPS data before creating activity clusters
  tar_target(prepared_data, gps_points_process(scored_days)),
  
  # SUMMARIZE THE PROCESSED DATA
  tar_target(prepared_data_summary, summarize_prepared_data(prepared_data)),
  
  # Make activity clusters using the optimized parameters
  tar_target(clustered_data, makeClusters(prepared_data, params = c(11.7,3,300,1))),
  
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
  
  # SUMMARIZE THE TRIP DATA
  tar_target(trip_data_summary, summarize_trip_data(activity_types)),
  
  
  
  # Read in the demographic data
  tar_target(demographics, readDemographicData("data/mental_surveys/Demographic_Breakdown.xlsx")),

  # Remove the irrelevant userIds of those who weren't study participants
  tar_target(demo_ids, clean_userids(demographics)),

  # Create frame of userIds and date
  tar_target(frame, make_frame(demo_ids, "2019-05-01", "2022-03-01")),

  # Add the demographic data to the frame
  tar_target(demo_data, addDemographicData(frame, demo_ids)),

  # Add the mental health responses to the demographic data
  tar_target(survey_data, addMentalHealthResponses(demo_data)),
  
  # SUMMARIZE THE SURVEY DATA
  tar_target(survey_data_summary, summarize_survey_data(survey_data)),
  
  

  # Make a final table with all of the data
  tar_target(complete_table, combine_data(survey_data, activity_types)),

  # Keep rows that have survey responses
  tar_target(final_table, clean_final_table(complete_table)),
  
  # SUMMARIZE THE FINAL DATA
  tar_target(final_data_summary, summarize_final_data(final_table)),
  
  
  
  # Estimate models
  tar_target(models, estimate_models(final_table))
)
