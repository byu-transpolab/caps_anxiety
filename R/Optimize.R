#' Function to clean GPS data
#' 
#' Read in raw GPS data for participants in the CAPS survey. Each file
#' contains the GPS points for a single day and a single respondent.
#'
#' @param folder of raw GPS data
#' @return tibble of cleaned data. Each row contains a nested sf points object.
#' @details cleaned_data can be made and loaded using the _targets.R file
cleanData <- function(folder, nfiles = NULL) {
  
  # get a list of all the files in the data folder
  files_in_folder <- dir(folder, full.names = T)
  if(is.null(nfiles)){ nfiles <- length(files_in_folder) }
  
  # loop through the files in the folder
  # this is an embarrassingly parallel step implemented with the future library
  caps <- future_lapply(sample(files_in_folder, nfiles), function(x){
    
    # read CSV file and rename / simplify table
    readr::read_csv(x, col_types = list(userId = col_character())) %>%
      dplyr::transmute(
        id = userId,
        lat, lon,
        # Separate Date and Time columns
        timestamp = lubridate::as_datetime(time),
        date = lubridate::date(timestamp),   
        minute = str_c(
          str_pad(lubridate::hour(timestamp), width = 2, pad = "0"),
          str_pad(lubridate::minute(timestamp), width = 2, pad = "0")
        )
      ) %>% 
      # Want to sample down, and get at most a few observations per minute
      # create a group for each minute
      arrange(date, minute) %>% group_by(date, minute) %>% 
      # sample 10 rows in that group, or as many rows as exist
      slice_sample(n = 10, replace = FALSE ) 
    
  }, future.seed = 42) 
   
  caps %>%
    # combine all days for all participants into a single tibble
    dplyr::bind_rows() %>%
    ungroup %>%
    
    # The travel day is not the same as the calendar date, because people 
    # frequently are out past midnight. See the 'yesterday()' function for details.
    mutate(
      activityDay = yesterday(timestamp)
    )  %>%
    
    # want to have each group labeled by a date object, after grouping by the 
    # activityDay determined above
    group_by(activityDay) %>%
    mutate( date = min(date) ) %>%
    
    #' Make a nested tibble for each day / id combination
    arrange(timestamp) %>% group_by(id, date) %>%
    nest() %>% ungroup() %>%
    rename(cleaned = data) %>%
    dplyr::mutate(num_points = purrr::map_int(cleaned, nrow)) %>%
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

#' Read manually-defined activities
#'
#' @param folder of GeoJSON files named by Date_ID that include the number of clusters
#' @return A nested tibble with person ID, date, and an SF points object.
#' 
#' 
makeManualTable <- function(folder){
  files <- dir(folder, pattern = ".geojson")
  manualList <- lapply(files, function(file) {
    st_read(file.path(folder, file)) %>%
      st_transform(32612)
  })
  
  tibble(manual = manualList,
         name_of_file = file_path_sans_ext(files)) %>% 
    separate(name_of_file, c("date", "id"), sep = c("_")) 
}

#' Function to join the manual_table with the algorithm_table by ID and date
#'
#'
#' @param manual_table and algorithm_table targets
#' @return alg_manual_table target which includes the id, date
#' and the nested clusters column from both the manual table and algorithm table
joinTables <- function(manual_table,cleaned_data) {
  cleaned_data$date <- as.character(cleaned_data$date)
  inner_join(manual_table, cleaned_data, by = c("date","id")) %>%
    as_tibble() 
}


#' Function to return the error between algorithm and manual clusters
#'
#'
#' @param alg_manual_table target and initial set of params as defined in the optims 
#' function
#' 
#' @return Percent of gps 'points' that do not fall within both the algorithm buffer
#' and the manual buffer (FALSE)
#' 
#' @details This function is what is called in the optimization functions.
#' It returns the percentage of FALSE points as described in the 'number_of_points_
#' in_cluster' function. The goal of the optimization functions is to minimize 
#' the percent FALSE points between the number of points in the algorithm buffer
#' and the number of points in the manual buffer.
#' 
calculateError <- function(params, cleaned_manual_table) {
  clusters <- makeClusters(cleaned_manual_table, params) %>%
    filter(algorithm != "no clusters found") 
  
  raw_error <- lapply(seq_along(1:nrow(clusters)), function(i){
    # print(i)
    number_of_points_in_cluster(
      clusters$manual[[i]],
      clusters$algorithm[[i]],
      clusters$cleaned[[i]],
      buffer = 50)
  })
     
  error <- sum(unlist(raw_error), na.rm = TRUE)
  
  #' Create csv file of all the tested parameters and the associated error
  #' that were calculated through optimization process, 
  #' remember to change the name of the file for each kind of optimization
  
  write.table(cbind(params, error), 
              file="optim_scaled_20220920_2.csv",row.names=F,
              col.names=c('params','error'),
              append = TRUE)
  
  error
}


#' Calculate percent of correctly classified points
#' 
#' @param manual_centers An sf object of activity locations determined through hand-coding
#' @param algorithm_centers An sf object of activity locations determined through applying
#'   the DBSCAN-TE algorithm
#' @param points An sf object containing raw GPS points for the activities and trips 
#'   represented in the centers.
#' @param buffer The radius of the activity locations. This should be the same
#'   units as the projection of each sf (usually meters).
#'   
#' @return The percent of `points` that disagree between inclusion in `manual_centers` and 
#'   `algorithm_centers`
#'   
#' @details This function draws a circular buffer of the prescribed radius around
#'   the activity centers determined by two methods, one manual and one algorithm-based.
#'   The points are identified as being within each of the buffers, and the function
#'   returns the percent of points that are classified differently based on the
#'   the buffers in both methods.
#'   
number_of_points_in_cluster <- function(manual_centers, algorithm_centers, 
                                        points, buffer = 50){
  
  if(nrow(algorithm_centers) < 1){
    return(1) # there are no algorithm-defined clusters, so by definition nothing matches
  }
  
  # create buffers around activity points
  manual_buffer <- st_buffer(manual_centers, buffer) %>% st_union()
  algorithm_buffer <- st_buffer(algorithm_centers, buffer) %>% st_union()

  
  # determine whether the points are inside each set of buffers
  agree <- points %>% 
    mutate(
      manual = st_within(geometry, manual_buffer, sparse = FALSE, )[, 1],
      algori = st_within(geometry, algorithm_buffer, sparse = FALSE)[, 1],
      
      # are they the same?
      agree = manual == algori
    ) 
  
  # calculate percent of FALSE agreement
  stat <- table(agree$agree)
  
  # if there are no FALSE occurances, return FALSE percentage as 0 instead of
  # the default percent TRUE = 1
  if(stat[1] == nrow(agree)){
    return(0)
  }
  
  stat[1] / sum(stat)
  
  
   # map (for debugging)
   #pal <- colorFactor("Dark2", agree$agree)
  #leaflet() %>%
   #addProviderTiles(providers$CartoDB) %>%
   #addPolygons(data = manual_buffer %>% st_transform(4326), color = "red")  %>%
   #addPolygons(data = algorithm_buffer%>% st_transform(4326), color = "green")  %>%
   #addCircles(data = clusters$cleaned[[3]]%>% st_transform(4326), color = "black")
   #addCircles(data = agree %>% st_transform(4326), color = ~pal(agree))

}

#' Function to minimize the RMSE between algorithm clusters and manual clusters
#' and find the optimum values for each parameters that does so
#'
#'
#' @param initial vector for params and the calculateError function to be minimized
#' @return vector of optimized parameters 
#' @details param[1,2,3,4] are eps, minpts,delta_t, and entr_t respectively

optimize <- function(cleaned_manual_table, params = c(10,3,36000,1.3)) {
  sannbox(par = params, fn = calculateError, cleaned_manual_table = cleaned_manual_table,
        control = list(upper = c(100, 300, 24 * 3600, 4), lower = c(10,3,300, 1),
                       maxit = 100, parscale = c(25,75,21600,1)))
}
  
optimize2 <- function(cleaned_manual_table, params = c(10,3,36000,1.3)) {
  optim(par = params, fn = calculateError, cleaned_manual_table = cleaned_manual_table,
        method = "L-BFGS-B", upper = c(100, 300, 24 * 3600, 4), lower = c(10,3,300, 1),
        control = list(maxit = 100, parscale = c(25,75,21600,1)))
}
