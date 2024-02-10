# Needed packages
library(tidyverse)
library(data.table)
library(lubridate)
library(stringr)
library(sf)
library(purrr)

# Function to determine a meaningful day
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

# Function to make an sf object using the lat and lon provided
makeSf <- function(df, crs = 32612) {
  df %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4327) %>%
    st_transform(crs)
}


# Function to clean and process all of the CSV files to use in the algorithm 

process_caps_data <- function(folder_path) {
  # Get list of CSV file names in the folder
  file_names <- list.files(folder_path, pattern = "*.csv", full.names = TRUE)
  
  # Read and combine all CSV files into a data.table
  
  caps_data <- data.table()
  for (file in file_names) {
    data <- fread(file, nThread = parallel::detectCores()-1)
    caps_data <- rbind(caps_data, data)
  }
  
  
  # myread <- function(file_path) {
  #   fread(file_path, nThread = parallel::detectCores() - 1)
  # }
  # 
  # file_names <- list.files(folder_path, pattern = "*.csv", full.names = TRUE)
  # 
  # caps_data <- map_dfr(file_names, myread)
  
  
  # myread <- function(file_names) {
  # data_list <- lapply(file_names, function(file) {
  #   fread(file, nThread = parallel::detectCores()-1)
  # })
  # 
  # caps_data <- rbindlist(data_list)
  # }
    
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
    # caps_data <- caps_data[order(userId, date, minute), .SD[sample(.N, min(.N, 10), replace = FALSE)], by = .(userId, date, minute)] %>%
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
  
  return(new_caps_data)
}




start <- Sys.time()
# Call the function with your folder path
folder_path <- "data/caps_data/"
processed_data <- process_caps_data(folder_path)

end <- Sys.time()

end - start




# time using read_csv
start <- Sys.time()
read_csv("data/caps_data - Copy/5e1fd7fd2546514da66ad29d.csv")
end <- Sys.time()

end - start

# time using fread
start <- Sys.time()
fread(file=, "data/caps_data - Copy/5e1fd7fd2546514da66ad29d.csv")
end <- Sys.time()

end - start