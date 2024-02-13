#' Convert unlabeled CSV data into geojson format
#' 
#' @param directory directory with unlabeled CSV data
#' @param destination directory to place the new geosjon files in
#' 
#' @examples
#' directory <- "data/unlabeled_csv"
#' destination <- "data/unlabeled_geojson"
#' convert_csv2geojson(directory, destination)
#' 
convert_csv2geojson <- function(directory, destination){
  
  # make destination directory if it's not there
  if(!dir.exists(destination)){
    dir.create(destination) 
  }
  
  # get the list of files
  file_list <- dir(directory, pattern = ".csv", full.names = FALSE)
  
  # loop through, saving a geojson verson of each
  lapply(file_list, function(file){
    readr::read_csv(
      file.path(directory, file), 
      col_names = c(
        "x1", "x2", "x3",
        "id", 
        "x5", "x6", "x7",
        "timestamp", 
        "x9", "x10",
        "lat", "lon"
      )) |> 
      dplyr::select(id, timestamp, lat, lon) |> 
      sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |> 
      dplyr::mutate(clocktime = as.numeric(timestamp),
                    clocktime = clocktime - clocktime[1]) |> 
      sf::st_write(file.path(destination, gsub(".csv", ".geojson", file)), 
                   delete_dsn = TRUE)
    
  })
}


#' Read unlabeled geojson data into nested dataframe
#' 
#' @param directory Directory with unlabeled geojson files
#' 
#' @examples
#' directory <- "data/unlabeled_geojson/"
#' unlabeled_data <- read_unlabeled_data(directory)
#' 
#' 
read_unlabeled_data <- function(directory){
  # get the list of files
  file_list <- dir(directory, pattern = ".geojson", full.names = FALSE)
  
  lapply(file_list, function(file){
    sf::st_read(file.path(directory, file), quiet = TRUE) |> 
      dplyr::sample_frac(0.1) |> 
      sf::st_transform(32612) 
  }) |> 
    purrr::set_names(gsub(".geojson", "", file_list)) |> 
    dplyr::bind_rows(.id = "referenceid") |> 
    tidyr::nest(.by = referenceid) |> 
    dplyr::rename(raw = data)
}


#' Read labeled data to R
#' 
#' @param directory
#' 
#' @examples
#' directory <- "data/labeled_geojson"
#' labeled_data <- read_labeled_data(directory)
#' 
#' @details
#' 
#' @return An sf dataset with the labeled data
read_labeled_data <- function(directory){
  
  # List of shp file paths 
  file_list <- list.files(directory, pattern = ".shp")
  
  # Loop through each file
  lapply(file_list, function(file){
    sf_data <- sf::st_read(file.path(directory, file), quiet = TRUE) |> 
      sf::st_transform(32612)
  }) |> 
    purrr::set_names(gsub(".shp", "", file_list)) |> 
    dplyr::bind_rows(.id = "referenceid") |> 
    tidyr::nest(.by = referenceid) |> 
    dplyr::rename(labeled = data)
}

#' Join labeled and unlabeled data into a single dataframe
#' 
#' @param labeled_data A dataframe of labeled activity points, nested by reference ID
#' @param unlabeled_data A dataframe of unlabeled GPS data, nested by reference ID
#' 
#' @return A nested dataframe with a column of labeled activity points and a
#'   column of gps points, ready for optimization analysis.
#' 
make_optim_frame <- function(labeled_data, unlabeled_data) {
  left_join(labeled_data, unlabeled_data, by = "referenceid")
}



#' Identify optimal parameters
#' 
#' @param cleaned_table
#' @param params initial vector for params and the calculateError function to be minimized
#' @return vector of optimized parameters 
#' @details param[1,2,3,4] are eps, minpts,delta_t, and entr_t respectively
#' Function to minimize the RMSE between algorithm clusters and manual clusters
#' and find the optimum values for each parameters that does so

optimize_sann <- function(optim_frame, params = c(10,3,36000,1.3)) {
  results <- pomp::sannbox(par = params, 
                           fn = calculate_activity_error, 
                           optim_frame = optim_frame,
                           control = list(upper = c(100, 300, 12 * 3600, 4), 
                                          lower = c( 10,   3,       300, 1),
                                          maxit = 100, 
                                          parscale = c(25, 75,     3600, 1)))
  
  results
}

#' Apply dbscan_te
#' 
#' @param frame A nested data frame with raw points
#' @param params
apply_dbscante <- function(frame, params = c(25, 15, 300, 1.75)){
  future::plan(multisession, workers = 4)
  frame$predicted <- frame$raw |> 
    furrr::future_map(\(x) gpsactivs::dbscan_te(
      x,  eps = params[1], minpts = params[2], delta_t = params[3], entr_t = params[4])
    )
  
  frame
}

#' Calculate error between prediction and labeled points
#' 
#' @param optim_frame A dataframe with labeled clusters and raw gps points joined
#'   together.
#' 
calculate_activity_error <- function(optim_frame, params = c(25, 15, 300, 1.75)){
  future::plan(multisession, workers = 4)
  optim_frame$predicted <- optim_frame$raw |> 
    furrr::future_map(\(x) gpsactivs::dbscan_te(
      x,  eps = params[1], minpts = params[2], delta_t = params[3], entr_t = params[4])
      )
  
  raw_error <- lapply(seq_along(1:nrow(optim_frame)), function(i){
    # print(i)
    number_of_points_in_cluster(
      optim_frame$labeled[[i]],
      optim_frame$predicted[[i]],
      optim_frame$raw[[i]],
      buffer = 50)
  })
  
  error <- sum(unlist(raw_error), na.rm = TRUE)
  
  #' Create csv file of all the tested parameters and the associated error
  #' that were calculated through optimization process, 
  #' remember to change the name of the file for each kind of optimization
  line <- c(params, error) |> set_names(c('eps', 'minpts', 'delta_t', 'entr_t', 'error')) |> 
    bind_rows()
  readr::write_delim(line, 
              file="data/optim_scaled_2024-02-12.csv", delim = ",",
              append = TRUE)
  
  error
}


#' Calculate percent of correctly classified points
#' 
#' @param labelled An sf object of activity locations determined through hand-coding
#' @param predicted An sf object of activity locations determined through applying
#'   the DBSCAN-TE algorithm
#' @param raw An sf object containing raw GPS points for the activities and trips 
#'   represented in the centers.
#' @param buffer The radius of the activity locations. This should be the same
#'   units as the projection of each sf (usually meters).
#'   
#' @return The percent of `points` that disagree between inclusion in `labelled` and 
#'   `predicted`
#'   
#' @details This function draws a circular buffer of the prescribed radius around
#'   the activity centers determined by two methods, one manually labeled and one 
#'   algorithm-based.
#'   The points are identified as being within each of the buffers, and the function
#'   returns the percent of points that are classified differently based on the
#'   the buffers in both methods.
#'   
number_of_points_in_cluster <- function(labelled, predicted, 
                                        points, buffer = 50){
  
  if(nrow(predicted) < 1){
    return(1) # there are no algorithm-defined clusters, so by definition nothing matches
  }
  
  # create buffers around activity points
  labeled_buffer <- sf::st_buffer(labelled, buffer) %>% sf::st_union()
  predict_buffer <- sf::st_buffer(predicted, buffer) %>% sf::st_union()
  
  
  # determine whether the points are inside each set of buffers
  agree <- points %>% 
    dplyr::mutate(
      labeled = sf::st_within(geometry, labeled_buffer, sparse = FALSE, )[, 1],
      predict = sf::st_within(geometry, predict_buffer, sparse = FALSE)[, 1],
      
      # are they the same?
      agree = labeled == predict
    ) 
  
  # calculate percent that agree
  ptrue <- sum(agree$agree) / length(agree$agree)
  
  1 - ptrue
}

