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
source("R/Figures.R")

# Set target-specific options such as packages.
# Install gpsactivs from github with remotes::install_github("byu-transpolab/gpsactivs")
tar_option_set(packages = c("dplyr","tools", "hms", "lubridate", "gpsactivs",
                            "tidyverse", "leaflet", "sf", "purrr", "stringr",
                            "ggspatial", "data.table", "plotly", "future.apply",
                            "viridis", "pomp", "stats", "osmdata", "modelsummary",
                            "plm", "tinytable"),
               memory = "transient",
               garbage_collection = TRUE)

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
  
  # SUMMARIZE THE RAW DATA
  tar_target(raw_data_summary, summarize_raw_data(read_in)),
  
  # Remove the irrelevant userIds of those who don't have demo data
  tar_target(cleaned_trip_ids, clean_trip_userids(read_in)),
  
  # Preprocess the data
  tar_target(preprocessed, preprocessing(cleaned_trip_ids)),
  
  # SUMMARIZE THE RAW DATA
  tar_target(filtered_data_summary, summarize_filtered_data(preprocessed)),
  
  # Implement the scoring algorithm to have higher quality userId activityDay combos
  tar_target(scored_days, scoring(preprocessed)),
  
  # Create a subset of the processed data
  tar_target(preprocessed_samp, scored_days_samp(preprocessed)),
  
  # Create a subset of the preprocessed_samp to avoid long vectors
  tar_target(preprocessed_subset, scored_days_subset(preprocessed_samp)),
  
  # Plot a sample of scored days
  tar_target(scored_days_samp_plot, scored_days_plot(preprocessed_samp)),

  # SUMMARIZE THE SCORED DATA
  tar_target(scored_data_summary, summarize_scored_data(scored_days)),
  
  # Clean the remaining GPS data before creating activity clusters
  tar_target(prepared_data, gps_points_process(scored_days)),
  
  # SUMMARIZE THE PROCESSED DATA
  tar_target(prepared_data_summary, summarize_prepared_data(prepared_data)),
  
  # Make activity clusters using the optimized parameters
  tar_target(clustered_data, makeClusters(prepared_data, params = c(16.8,207,1860,1.16))),
  
  # Determine the number of trips made on a given day by a given user
  tar_target(num_trips, addNumTrips(clustered_data)),
  
  # Read in location files for parks, groceries, and libraries
  tar_target(parks_file, "data/resources/parks.geojson", format = "file"),
  tar_target(parksSf, sf::st_read(parks_file) %>% sf::st_transform(32612)),
  
  tar_target(grocery_file, "data/resources/groceries.geojson", format = "file"),
  tar_target(grocerySf, sf::st_read(grocery_file) %>% sf::st_transform(32612)),
  
  tar_target(library_file, "data/resources/libraries.geojson", format = "file"),
  tar_target(librarySf, sf::st_read(library_file) %>% sf::st_transform(32612)),
  
  tar_target(social_rec_file, "data/resources/social_rec.geojson", format = "file"),
  tar_target(social_recSf, sf::st_read(social_rec_file) %>% sf::st_transform(32612)),
  
  # Determine the number of trips for each activity type
  tar_target(activity_types, addTripType(num_trips, parksSf, grocerySf, librarySf, social_recSf)),
  
  # SUMMARIZE THE TRIP DATA
  tar_target(trip_data_summary, summarize_trip_data(activity_types)),

  
  
  # Read in the demographic data
  tar_target(demographics, readDemographicData("data/mental_surveys/Demographic_Breakdown.xlsx")),

  # Remove the irrelevant userIds of those who don't have any activityDays
  tar_target(demo_ids, clean_demo_userids(demographics)),
  
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
  
  # Impute the trip data to render a more complete dataset
  tar_target(imputed_trips, imputation(complete_table)),

  # Keep rows that have survey responses
  tar_target(final_table, clean_final_table(imputed_trips)),
  
  # SUMMARIZE THE FINAL DATA
  tar_target(final_data_summary, summarize_final_data(final_table)),
  
  
  
  # Prepare the data for models
  tar_target(model_data, prep_models(final_table)),
  
  # Estimate the OLS Model
  tar_target(ols, ols_model(model_data)),
  
  # Estimate the Fixed Effects Model
  tar_target(fe, fixed_effects(model_data)),
  
  # Estimate the Random Effects Model
  tar_target(re, random_effects(model_data)),
  
  # Perform the Hausman Test
  tar_target(haus_result, hausman(fe, re)),
  
  # Create the Fixed Effects Linear Model
  tar_target(fe_model, fe_analysis(fe, demo_ids)),
  
  
  
  # Estimate the Fixed Effects Model for the sev_day_avg
  tar_target(fe_sevdayavg_mod, fe_sevdayavg(model_data)),
  
  # Estimate the Fixed Effects Model for numTrips
  tar_target(fe_numTrips_mod, fe_numTrips(model_data)),
  
  # Estimate the Fixed Effects Model for log(numTrips)
  tar_target(fe_numTrips_log, fe_numTrips_log(model_data)),
  
  # Estimate the Fixed Effects Model for numTrips^2
  tar_target(fe_numTrips_squared, fe_numTrips_squared(model_data)),

  # Estimate the Fixed Effects Model for the area
  tar_target(fe_area_mod, fe_area(model_data)),
  
  # Estimate the Fixed Effects Model for the length
  tar_target(fe_length_mod, fe_length(model_data)),
  
  # Estimate the Fixed Effects Model for the log(length)
  tar_target(fe_length_log, fe_length_log(model_data)),
  
  # Estimate the Fixed Effects Model for the length^2
  tar_target(fe_length_squared, fe_length_squared(model_data)),

  
  
  
  # Table for numeric descriptive statistics
  tar_target(descrip_stats, desc_stats(demo_ids))
)
