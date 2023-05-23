GetJeremyMaps <- function(folder = "manual_clusters"){
  files <- dir(folder, pattern = ".geojson")
  JeremyMaps <- lapply(files, function(file) {
    st_read(file.path(folder, file)) 
  })
  tibble(JeremyMaps = JeremyMaps,
         name_of_file = file_path_sans_ext(files)) %>% 
    separate(name_of_file, c("date", "id"), sep = c("_")) 
}

GetOurMaps <- function(folder = "maps"){
  files <- dir(folder, pattern = ".geojson")
  OurMaps <- lapply(files, function(file) {
    st_read(file.path(folder, file)) 
  })
  tibble(OurMaps = OurMaps,
         name_of_file = file_path_sans_ext(files)) %>% 
    separate(name_of_file, c("id", "date"), sep = c("_")) 
}

JeremyTable = GetJeremyMaps() 
OurTable = GetOurMaps()

df <- left_join(JeremyTable, OurTable, by = c("id","date")) %>%
  filter(OurMaps != "NULL")

lapply(seq_along(1:nrow(df)), function(i){
  print(i)
  leaflet() %>%
    addProviderTiles(providers$CartoDB) %>% 
    addCircleMarkers(data = df$JeremyMaps[[i]] %>% 
                       st_transform(4326), color = "blue") %>%
    addCircles(data = df$OurMaps[[i]] %>%
                 st_transform(4326), color = "red") %>%
    addControl(JeremyTable$id[[i]], position = "topleft", className="map-title") %>%
    addControl(JeremyTable$date[[i]], position = "topleft", className="map-title")
})


  