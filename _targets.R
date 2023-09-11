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
tar_option_set(packages = c("dplyr","tools", "hms", "lubridate", 
                            "tidyverse", "leaflet", "sf", "gpsactivs", 
                            "ggspatial", "data.table", "plotly", "future.apply",
                            "viridis", "pomp", "stats"))

# End this file with a list of target objects.
list(
  # Read in and clean GPS data
  tar_target(cleaned_data, cleanData("data/caps_data"), resources = tar_resources(
    future = tar_resources_future(resources = list(n_cores = threads))
  )),
  tar_target(manual_table, makeManualTable("manual_clusters20220826")),
  
  # Join cleaned data and manual data to allow for calibration
  tar_target(cleaned_manual_table, joinTables(manual_table, cleaned_data)),
  
  
  #tar_target(maps, makeAllMaps(cleaned_data)),
  #tar_target(optimized_sann_params, optimize(cleaned_manual_table)),
  #tar_target(optimized_optim_params, optimize2(cleaned_manual_table)),
  tar_target(accurate_sann_tibble, makeClusters(cleaned_manual_table = cleaned_data,
                                                params = c(11.7,3,300,1))),
  tar_target(num_trips, addNumTrips(accurate_sann_tibble)),
  tar_target(activity_types, addTripType(num_trips))
  #tar_target(final_table, addMentalHealthResponses(activity_types))
)