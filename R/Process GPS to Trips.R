#' Function to read in csv files
#' 
#' This function reads multiple CSV files and combines them into a single
#' data.table. Each CSV file contains GPS data, and the function aggregates
#' them into a single table.
#'
#' @param file_names A character vector of file paths to CSV files.
#' @return gps_points A data.table containing the combined GPS data.
#'
#' @details The function reads each CSV file in the `file_names` vector, 
#' combining them into a single data.table. It uses `fread` for efficient file 
#' reading and parallel processing if available. The resulting data.table 
#' contains GPS data from all the input files.

read_data <- function(file_names) {
  # Read and combine all CSV files into a data.table
  gps_points <- data.table()
  for (file in file_names) {
    message("reading file: ", file)
    data <- fread(file, nThread = parallel::detectCores() - 1)
    gps_points <- rbind(gps_points, data)
  }
  return(gps_points)
}


#' Process GPS points data
#'
#' This function processes GPS points data by separating date and time, 
#' selecting relevant columns, grouping by specified variables, 
#' sampling points, and creating spatial features.
#' 
#' @param gps_points A tibble containing GPS points data.
#' 
#' @return A tibble containing processed GPS points data.

gps_points_process <- function(gps_points) {
  processed <- gps_points %>%
    # Separate date and time into columns
    mutate(
      timestamp = as_datetime(time),
      activityDay = yesterday(timestamp),
      hour = hour(timestamp),
      minute = minute(timestamp)) %>%
    # Keep these columns for analysis
    select(
      userId,
      activityDay,
      timestamp,
      hour,
      minute,
      lat,
      lon) %>% 
    arrange(userId, activityDay, hour, minute) %>% 
    group_by(userId, activityDay, hour, minute) %>%
    slice_sample(n = 10, replace = FALSE) %>%
    
    ungroup() %>% 
    group_by(userId, activityDay) %>%
    nest() %>% 
    ungroup() %>%
    rename(cleaned = data) %>%
    mutate(num_points = purrr::map_int(cleaned, nrow)) %>%
    filter(num_points > 1000)  %>% 
    mutate(cleaned = purrr::map(cleaned, makeSf))
    
  return(processed)
}



#' Adjust Timestamp to Previous Day
#'
#' This function adjusts a timestamp to the previous day if the recorded hour is
#' before 3 AM. It helps correct for instances where the timestamp corresponds to
#' the previous calendar date due to activities past midnight.
#'
#' @param timestamp A vector of timestamps.
#' @return A vector of dates adjusted for the previous day if
#' the recorded hour is before 3 AM.
#'
#' @details The function checks if the recorded hour in the timestamp is before
#' 3 AM. If true, it subtracts one day from the timestamp to adjust it to the
#' previous day. The resulting dates are returned as a vector.

yesterday <- function(timestamp) {
  is_previous_day <- hour(timestamp) < 3
  timestamp[is_previous_day] <- timestamp[is_previous_day] - ddays(1)
  as.Date(timestamp)
}


#' Make an sf object out of lat / long tabular data
#'
#' @param cleaned_data tibble of GPS points including columns `lat` and `lon`. These
#'   should be in WGS84 coordinate system.
#' @param crs The EPSG code for the desired projection. Defaults to 32612, UTM 
#'   Zone 12N (meters), which is appropriate for many locations in Utah.
#' @return an sf point object where the lat and long columns are replaced with a 
#'   single geometric coordinates column called "geometry"

makeSf <- function(df, crs = 32612) {
  df %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4327) %>%
    st_transform(crs)
}


#' Compute activity locations for one day
#'
#' @param df sf points object containing points and timepoints of GPS points for
#'   a single individual in a single day.
#' @param params a vector of parameters passed to the `gpsactivs::dbscan_te()`
#'   function.
#' @return clustered_data An sf points object with labeled and ordered imputed 
#'   activities, including start and end times.
#'   
#' @details param[1,2,3,4] are eps, minpts, delta_t, and entr_t respectively.
#'   This function is a convenience wrapper to the `gpsactivs::dbscan_te()` 
#'   function; more information can be found there.

makeClusters_1T <- function(df, params) {
  gpsactivs::dbscan_te(df, eps = params[1], minpts = params[2],
                       delta_t = params[3], entr_t = params[4])
}

makeClusters <- function(cleaned_manual_table, params) {
  print(params)
  cleaned_manual_table %>%
    ungroup() %>%
    # rename(timestamp = timestamp_new) %>% 
    mutate(algorithm = purrr::map(cleaned, makeClusters_1T, 
                           params = params))
}


#' Function to Add Number of Trips
#'
#' Calculates the total number of trips made by each user on each date.
#'
#' @param clustered_data A data frame containing user data.
#' @return num_trips A tibble with added 'numTrips' column representing the number of 
#' trips made with userId, data, sf, and algorithm
#' 
#' @details Converts the data frame to a tibble, selects specific columns 
#' (userId, date, cleaned, algorithm),groups the data by  userId and date, and 
#' then calculates the number of trips made by each user on each date.

addNumTrips <- function(df){
  tibble <- as_tibble(df) %>%
    select(userId, activityDay, cleaned, algorithm) %>%
    group_by(userId, activityDay) %>%
    rowwise() %>%
    # Calculate the total number of trips someone made in a day
    mutate(numTrips = length(algorithm[[1]]))
  return(tibble)
}


#' Function to Add Trip Types
#'
#' This function takes a tibble with numTrips and adds trip type based on 
#' spatial features.
#'
#' @param num_trips A tibble containing trip data.
#' @param parksSf Spatial features data for parks.
#' @param grocerySf Spatial features data for grocery stores.
#' @param librarySf Spatial features data for libraries.
#' @return activity_types A table including ID, date, sf, algorithm, the number 
#' of total clusters as its own column, and how many of those clusters were at 
#' a park, library, or grocery store
#'
#' @details The function first filters out rows where 'algorithm' is not a 
#' character, and removes the 'cleaned' column. It then adds trip type 
#' variables ('park', 'grocery', 'library') based location information.

addTripType <- function(tibble, parksSf, grocerySf, librarySf){
  
  t2 <- tibble %>%
    filter(!is.character(algorithm)) %>%
    select(-c(cleaned))
  
  # add more land use location variables
  t2$park = purrr::map_int(t2$algorithm, ~add_location(.x, variable = id, sf = parksSf))
  t2$grocery = purrr::map_int(t2$algorithm, ~add_location(.x, variable = SITE_NAME, sf = grocerySf))
  t2$library = purrr::map_int(t2$algorithm, ~add_location(.x, variable = fid, sf = librarySf))
  t2
}


#' Function to Add Location Information
#'
#' This function adds location information based on spatial features data.
#'
#' @param x A data frame containing location information.
#' @param variable The name of the location variable to be added.
#' @param sf Spatial features data to join with.
#' @return A vector with the added location information.
#'
#' @details The function performs a spatial join between the input data frame 
#' and the specified spatial features (sf). It calculates the number of 
#' non-missing entries in the specified variable for each row.

add_location <- function(x, variable, sf){
  if(is.null(nrow(x))) {
    a <- NA
  } else {
    a <- st_join(x, sf) %>% 
      st_set_geometry(NULL) %>% 
      ungroup() %>% 
      summarise(s = sum(!is.na({{variable}})) ) %>% 
      pull(s)
  }
  
  return(a)
}
