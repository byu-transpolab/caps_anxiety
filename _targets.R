# # Created by use_targets().
# # Follow the comments below to fill in this target script.
# # Then follow the manual to check and run the pipeline:
# #   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint
# 
# # Load packages required to define the pipeline:
# library(targets)
# # library(tarchetypes) # Load other packages as needed. # nolint
# 
# # Set target options:
# tar_option_set(
#   packages = c("tidyverse", "mlogit", "modelsummary"), # packages that your targets need to run
#   format = "rds" # default storage format
#   # Set other options as needed.
# )
# 
# # tar_make_clustermq() configuration (okay to leave alone):
# # options(clustermq.scheduler = "multicore")
# 
# # tar_make_future() configuration (okay to leave alone):
# # Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.
# 
# # Load the R scripts stored in R/ with your custom functions:
# for (file in list.files("R", full.names = TRUE)) source(file)
# # source("other_functions.R") # Source other scripts as needed. # nolint
# 
# # Replace the target list below with your own:
# list(
#   tar_target(
#     name = car_mlogit,
#     command = make_data()
#   ),
#   tar_target(
#     name = models,
#     command = estimate_models(car_mlogit)
#   )
# )

library(targets)

# parallelization
library(future)
library(future.apply)
library(furrr)
library(future.callr)
plan(callr)
threads <- future::availableCores() - 1

source("R/Optimize.R")
source("R/MakeMaps.R")
source("R/Mental Health Table.R")

tar_option_set(debug = "cleaned_manual_table")


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