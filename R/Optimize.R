#' Function to clean GPS data
#' 
#' Read in raw GPS data for participants in the CAPS survey. Each file
#' contains the GPS points for a single day and a single respondent.
#'
#' @param folder of raw GPS data
#' @return tibble of cleaned data. Each row contains a nested sf points object.
#' @details cleaned_data can be made and loaded using the _targets.R file

process_caps_data <- function(file_names) {
  # Read and combine all CSV files into a data.table
  caps_data <- data.table()
  for (file in file_names) {
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
#' @details if a timestamp occurs during midnight or 3 AM, it will be assigned to the
#' previous calendar date

yesterday <- function(timestamp){
  x <- case_when(
    lubridate::hour(timestamp)< 3~lubridate::day(timestamp)-1L,
    lubridate::hour(timestamp)< 3 & lubridate::month(timestamp) %in% c(10,5,7,12) & lubridate::day(timestamp) == 1 ~ 30L,
    lubridate::hour(timestamp)< 3 & lubridate::month(timestamp) %in% c(2,4,6,8,9,11,1) & lubridate::day(timestamp) == 1 ~ 31L,
    lubridate::hour(timestamp)< 3 & lubridate::month(timestamp) ==3 & lubridate::day(timestamp) == 1 ~ 28L,
    lubridate::hour(timestamp)< 3 & lubridate::month(timestamp) ==3 & lubridate::day(timestamp) == 1 & lubridate::leap_year(timestamp) ~ 29L,
    TRUE ~ lubridate::day(timestamp)
  )
  
  str_c(x, lubridate::month(timestamp), sep = "-")
}


#' Make an sf object out of lat / long tabular data
#'
#'
#' @param df tibble of GPS points including columns `lat` and `lon`. These
#'   should be in WGS84 coordinate system.
#' @param crs The EPSG code for the desired projection. Defaults to 32612, UTM 
#'   Zone 12N (meters), which is appropriate for many locations in Utah.
#'   
#' @return an sf point object where the lat and long columns are replaced with a 
#'   single geometric coordinates column called "geometry"
#'   
#'   
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
#' @return An sf points object with labeled and ordered imputed activities,
#'   including start and end times.
#' @details param[1,2,3,4] are eps, minpts, delta_t, and entr_t respectively.
#'   This function is a convenience wrapper to the `gpsactivs::dbscan_te()` 
#'   function; more information can be found there.
#' 
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