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


#' Function to determine Num of GPS Points
#' 
#' This function takes a data.table of GPS data, separates date and time 
#' information, selects specific columns, groups the data by date, and 
#' summarizes the number of GPS points for each date.
#'
#' @param gps_points A data.table containing GPS data.
#' @return processed_data A summarized data.table with date and GPS point counts.
#'
#' @details The function processes the GPS data to obtain summary information 
#' about the number of GPS points recorded for each date. It extracts the date 
#' and time information from the original data, selects relevant columns, and 
#' groups the data by date. The resulting data.table contains two columns: 
#' "date" and "GPS_Points."

gps_points_process <- function(gps_points) {
  processed_data <- gps_points %>%
    # Separate date and time into columns
    mutate(
      timestamp = lubridate::as_datetime(time),
      date = lubridate::date(timestamp),
      minute = str_c(
        str_pad(lubridate::hour(timestamp), width = 2, pad = "0"),
        str_pad(lubridate::minute(timestamp), width = 2, pad = "0")
      )
    ) %>%
    # Select out only these columns
    select(date, lat, lon) %>% 
    group_by(date) %>% 
    summarise(GPS_Points = n())
  
  return(processed_data)
}


#' Function to clean the CAPS gps data
#'
#' Read in raw GPS data for participants in the CAPS survey. Each file
#' contains the GPS points for a single day and a single respondent.
#'
#' @param file_names A vector of file paths to CSV files containing CAPS data.
#' @return cleaned_data A nested tibble with preprocessed CAPS data.
#'
#' @details The function reads and combines all CSV files into a data table. 
#'    It then preprocesses the data by separating date and time, selecting 
#'    specific columns, sampling data to reduce the number of observations per 
#'    minute, and creating nested tibbles for each day/user combination.

process_caps_data <- function(file_names) {
  # Read and combine all CSV files into a data.table
  caps_data <- data.table()
  for (file in file_names) {
    message("reading file: ", file)
    data <- fread(file, nThread = parallel::detectCores()-1)
    caps_data <- rbind(caps_data, data)
  }
  
  new_caps_data <- caps_data %>%
    # Separate date and time into columns
    mutate(
      timestamp = lubridate::as_datetime(time),
      date = lubridate::date(timestamp),
      minute = str_c(
        str_pad(lubridate::hour(timestamp), width = 2, pad = "0"),
        str_pad(lubridate::minute(timestamp), width = 2, pad = "0")
      )
    ) %>%
    # Select out only these columns
    select(
      userId,
      time,
      lat,
      lon,
      timestamp,
      date,
      minute
    ) %>%
    
    # Sample down to get a few observations per minute rather than all observations
    arrange(userId, date, minute) %>% 
    group_by(userId, date, minute) %>% 
    slice_sample(n = 10, replace = FALSE) %>%
    
    # The travel day is not the same as the calendar date, because people 
    # frequently are out past midnight. See the 'yesterday()' function for details.
    mutate(activityDay = yesterday(timestamp)) %>%
    group_by(activityDay) %>% 
    mutate(date = min(date)) %>%
    
    # Make a nested tibble for each day / userId combination
    arrange(timestamp) %>% 
    group_by(userId, date) %>%
    nest() %>% 
    ungroup() %>%
    rename(cleaned = data) %>%
    mutate(num_points = purrr::map_int(cleaned, nrow)) %>%
    filter(num_points > 1000)  %>% 
    mutate(cleaned = purrr::map(cleaned, makeSf))
}


#' Function to compute meaningful day
#'
#'
#' @param timestamp vector of timestamp
#' @return the day number of the timestamp
#' 
#' @details if a timestamp occurs during midnight or 3 AM, it will be assigned to the
#'    previous calendar date

yesterday <- function(timestamp) {
  if (hour(timestamp) < 3) {
    timestamp <- timestamp - ddays(1)
  }
  as_date(timestamp)
}


#' Make an sf object out of lat / long tabular data
#'
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
    select(userId, date, cleaned, algorithm) %>%
    group_by(userId, date) %>%
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
