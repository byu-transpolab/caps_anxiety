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
    sf::st_read(file.path(directory, file), quiet = TRUE)
  }) |> 
    set_names(gsub(".geojson", "", file_list)) |> 
    bind_rows(.id = "referenceid") |> 
    nest(.by = referenceid)
}


#' Read labeled data to R
#' 
#' @param directory
#' 
#' @examples
#' directory <- "data/labeled_geojson"
#' 
#' 
#' @details
#' 
#' @return An sf dataset with the labeled data
read_labelled_data <- function(directory){
  
  # List of shp file paths 
  file_list <- list.files(directory, pattern = ".shp")
  
  # Loop through each file
  lapply(file_list, function(file){
    sf_data <- sf::st_read(file.path(directory, file), quiet = TRUE)
  }) |> 
    set_names(gsub(".shp", "", file_list)) |> 
    bind_rows(.id = "referenceid") |> 
    nest(.by = referenceid)
}

#' Score
#' @param labeled_data 
#' @param matching 


#' Apply the algorithm to generate activity locations
#' 
#' @param 
#' 

#' Identify optimal parameters
#' 
#' @param cleaned_table
#' @param params initial vector for params and the calculateError function to be minimized
#' @return vector of optimized parameters 
#' @details param[1,2,3,4] are eps, minpts,delta_t, and entr_t respectively
#' Function to minimize the RMSE between algorithm clusters and manual clusters
#' and find the optimum values for each parameters that does so

optimize <- function(cleaned_table, params = c(10,3,36000,1.3)) {
  sannbox(par = params, fn = calculateError, cleaned_manual_table = cleaned_manual_table,
          control = list(upper = c(100, 300, 24 * 3600, 4), lower = c(10,3,300, 1),
                         maxit = 100, parscale = c(25,75,21600,1)))
}



