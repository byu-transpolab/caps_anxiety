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


#' Summarize raw data by counting unique user IDs.
#'
#' This function takes a tibble with raw data and calculates the count of unique
#' user IDs, providing a summary of the raw data.
#'
#' @param raw_data A tibble containing raw data with user ID information.
#'
#' @return A tibble with a summary count of unique user IDs.

summarize_raw_data <- function(raw_data) {
  summary <- raw_data %>% 
    summarize(unique_userIds = n_distinct(userId))
    
  return(summary)
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


#' Perform preprocessing on relevant user data.
#'
#' This function takes a dataset of the raw LBS data and performs
#' preprocessing steps, including modifying timestamp-related columns and selecting
#' specific columns for further analysis.
#'
#' @param relevant_ids A dataset containing relevant user IDs and associated information.
#'
#' @return A tibble with preprocessed data, including modified timestamp-related
#' columns and selected columns for analysis.

preprocessing <- function(read_data) {
  raw_data <- read_data %>% 
    # Modify some of the columns
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
      lon)
  
  return(raw_data)
}


#' Summarize raw data for each unique userId.
#'
#' This function takes a dataset of raw data (`raw_data`) and performs 
#' summary calculations, including the total number of unique activity days for each user.
#'
#' @param raw_data A dataset containing raw user data with userIds and timestamp information.
#'
#' @return A tibble summarizing raw data, including the total number of unique 
#' activity days for each unique userId.

summarize_filtered_data <- function(filtered_data) {
   summary <- filtered_data %>% 
    # summarize(unique_userIds = n_distinct(userId)) %>% 
    group_by(userId) %>%
    summarize(total_activityDays = n_distinct(activityDay)) %>% 
    # summarize(total_activityDays = total_activityDays,
    #           avg_activityDays = mean(total_activityDays),
    #           min_activityDays = min(total_activityDays),
    #           max_activityDays = max(total_activityDays))
              
  
  return(summary)
}


#' Calculate scores for relevant GPS points data.
#'
#' This function takes a tibble of relevant GPS points and calculates scores
#' based on the number and spread of GPS points during a day for each user and day.
#'
#' @param relevant_ids A tibble containing relevant GPS points data.
#'
#' @return A tibble with calculated scores for each user and day.

scoring <- function(raw_data) {
  scored <- raw_data %>%
    # Determine how many GPS points there are per userID-activityDay by hour
    ungroup() %>% 
    arrange(userId, activityDay, hour) %>% 
    group_by(userId, activityDay, hour) %>%
    nest() %>% 
    ungroup() %>% 
    rename(cleaned = data) %>%
    mutate(num_points = purrr::map_int(cleaned, nrow)) %>%
    # Calculate the score for the day based on number and spread of gps points
    mutate(
      hour_multiplier = ifelse(hour %in% 8:23, 3, 1),
      points_multiplier = case_when(
        num_points <= 500 ~ 0,
        num_points <= 1500 ~ 1,
        num_points <= 2500 ~ 2,
        TRUE ~ 3
      ),
      daily_score = hour_multiplier * points_multiplier
    ) %>% 
    group_by(userId, activityDay) %>%
    mutate(total_daily_score = sum(daily_score)) %>% 
    # Keep rows with total_daily_score >= 95
    ungroup() %>% 
    filter(total_daily_score >= 95) %>% 
    select(-c(total_daily_score, hour_multiplier, points_multiplier, daily_score, num_points)) %>% 
    unnest(cols = c(cleaned))
  
  return(scored)
}


#' Summarize scored data for each unique userId.
#'
#' This function takes a dataset of scored information (`scored`) and performs 
#' summary calculations on the data, including the total number of unique 
#' activity days for each user.
#'
#' @param scored A dataset containing scored information with userIds and associated data.
#'
#' @return A tibble summarizing scored data, including the total number of unique 
#' activity days for each unique userId.

summarize_scored_data <- function(scored) {
  summary <- scored %>% 
    # summarize(unique_userIds = n_distinct(userId)) %>% 
    group_by(userId) %>% 
    summarize(total_activityDays = n_distinct(activityDay))
  
  return(summary)
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
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(crs)
}


#' Process GPS points data
#' 
#' Process GPS points data by arranging, grouping, sampling, converting to SF objects,
#' calculating convex hull area, and determining distance traveled.
#' 
#' @param scored The GPS points data with scores.
#' 
#' @return A processed tibble with additional columns for area and distance.

gps_points_process <- function(scored) {
  processed <- scored %>%
    arrange(userId, activityDay, hour, minute) %>% 
    group_by(userId, activityDay, hour, minute) %>%
    # Keep 6 LBS points minute for each hour
    slice_sample(n = 6, replace = FALSE) %>%
    ungroup() %>% 
    group_by(userId, activityDay) %>%
    nest() %>% 
    ungroup() %>%
    rename(cleaned = data) %>%
    mutate(num_points = purrr::map_int(cleaned, nrow)) %>%
    filter(num_points > 1000)  %>% 
    # Convert the LBS points to an SF object
    mutate(cleaned = purrr::map(cleaned, makeSf)) %>% 
    # Calculate the area of the convex hull
    mutate(convex_hull = purrr::map(cleaned, ~st_convex_hull(st_union(.))), 
           area = purrr::map_dbl(convex_hull, st_area)) %>% 
    # Calculate the distance traveled 
    mutate(polyline = purrr::map(cleaned, ~st_linestring(st_coordinates(.))), 
           length = purrr::map_dbl(polyline, st_length))
  
  return(processed)
}


#' Summarize prepared data for each unique userId.
#'
#' This function takes a dataset of processed information (`processed`) and performs 
#' summary calculations, including the total number of unique activity days for each user.
#'
#' @param processed A dataset containing processed information with userIds and activityDay.
#'
#' @return A tibble summarizing processed data, including the total number of unique 
#' activity days for each unique userId.

summarize_prepared_data <- function(processed) {
  summary <- processed %>% 
    # summarize(unique_userIds = n_distinct(userId)) %>% 
    group_by(userId) %>% 
    summarize(total_activityDays = n_distinct(activityDay))
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
    select(userId, activityDay, cleaned, algorithm, area, length) %>%
    rowwise() %>%
    # Calculate the total number of trips someone made in a day
    mutate(numTrips = length(algorithm[[1]]))
  return(tibble)
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
#' variables ('park', 'grocery', 'library', 'social_rec') based location information.

addTripType <- function(tibble, parksSf, grocerySf, librarySf, social_recSf){
  
  t2 <- tibble %>%
    filter(!is.character(algorithm)) %>%
    select(-c(cleaned))
  
  # add more land use location variables
  t2$park = purrr::map_int(t2$algorithm, ~add_location(.x, variable = id, sf = parksSf))
  t2$grocery = purrr::map_int(t2$algorithm, ~add_location(.x, variable = SITE_NAME, sf = grocerySf))
  t2$library = purrr::map_int(t2$algorithm, ~add_location(.x, variable = osm_id, sf = librarySf))
  t2$social_rec = purrr::map_int(t2$algorithm, ~add_location(.x, variable = osm_id, sf = social_recSf))
  t2
}


#' Summarize trip data for each unique userId.
#'
#' This function takes a dataset of activities (`activities`) and performs 
#' summary calculations, including the total number of unique activity days for each user.
#'
#' @param activities A dataset containing activity information with userIds and activityDay.
#'
#' @return A tibble summarizing trip data, including the total number of unique 
#' activity days for each unique userId.

summarize_trip_data <- function(activities) {
  summary <- activities %>% 
    # summarize(unique_userIds = n_distinct(userId)) %>% 
    group_by(userId) %>% 
    summarize(total_activityDays = n_distinct(activityDay))
}


#' Imputation of Averages
#'
#' This function performs imputation on a dataset containing trips and activities. It calculates rolling averages for different time windows and for various activity types.
#'
#' @param trips_activities A tibble containing user trips and activities data.
#' @return A tibble with imputed averages for specified time windows and activity types.

imputation <- function(trips_activities) {
  imputed_trips <- trips_activities %>% 
    group_by(userId) %>%
    mutate(
      sev_day_avg = zoo::rollapply(numTrips, width = 7, partial = TRUE, FUN = function(x) ifelse(all(is.na(x)), NA, mean(x, na.rm = TRUE)), align = "left", fill = NA),
      fourteen_day_avg = zoo::rollapply(numTrips, width = 14, partial = TRUE, FUN = function(x) ifelse(all(is.na(x)), NA, mean(x, na.rm = TRUE)), align = "left", fill = NA),
      thirty_day_avg = zoo::rollapply(numTrips, width = 30, partial = TRUE, FUN = function(x) ifelse(all(is.na(x)), NA, mean(x, na.rm = TRUE)), align = "left", fill = NA),
      sev_day_avg_park = zoo::rollapply(park, width = 7, partial = TRUE, FUN = function(x) ifelse(all(is.na(x)), NA, mean(x, na.rm = TRUE)), align = "left", fill = NA),
      fourteen_day_avg_park = zoo::rollapply(park, width = 14, partial = TRUE, FUN = function(x) ifelse(all(is.na(x)), NA, mean(x, na.rm = TRUE)), align = "left", fill = NA),
      thirty_day_avg_park = zoo::rollapply(park, width = 30, partial = TRUE, FUN = function(x) ifelse(all(is.na(x)), NA, mean(x, na.rm = TRUE)), align = "left", fill = NA),
      sev_day_avg_grocery = zoo::rollapply(grocery, width = 7, partial = TRUE, FUN = function(x) ifelse(all(is.na(x)), NA, mean(x, na.rm = TRUE)), align = "left", fill = NA),
      fourteen_day_avg_grocery = zoo::rollapply(grocery, width = 14, partial = TRUE, FUN = function(x) ifelse(all(is.na(x)), NA, mean(x, na.rm = TRUE)), align = "left", fill = NA),
      thirty_day_avg_grocery = zoo::rollapply(grocery, width = 30, partial = TRUE, FUN = function(x) ifelse(all(is.na(x)), NA, mean(x, na.rm = TRUE)), align = "left", fill = NA),
      sev_day_avg_library = zoo::rollapply(library, width = 7, partial = TRUE, FUN = function(x) ifelse(all(is.na(x)), NA, mean(x, na.rm = TRUE)), align = "left", fill = NA),
      fourteen_day_avg_library = zoo::rollapply(library, width = 14, partial = TRUE, FUN = function(x) ifelse(all(is.na(x)), NA, mean(x, na.rm = TRUE)), align = "left", fill = NA),
      thirty_day_avg_library = zoo::rollapply(library, width = 30, partial = TRUE, FUN = function(x) ifelse(all(is.na(x)), NA, mean(x, na.rm = TRUE)), align = "left", fill = NA)
    ) %>%
    ungroup() %>% 
    arrange(userId, date)
  
  return(imputed_trips)
}

