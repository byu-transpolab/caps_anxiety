### From Gillian's Mental_Health_Table.R file --> analysis/comparison

addMentalHealthResponses <- function(tibble){
  morningResponses <- read.csv("C:/Users/griches/Desktop/BigCellGPSDataToTrips/mental_surveys/Morning_Survey_220223.csv") %>%
    rename(id = Study.Id)
  eveningResponses <- read.csv("C:/Users/griches/Desktop/BigCellGPSDataToTrips/mental_surveys/Evening_Survey_220223.csv")
  full_join(tibble,morningResponses, by = 'id')
}

addDemographicData <- function(num_trips) {
  demographic <- readxl::read_excel("C:/Users/griches/Desktop/BigCellGPSDataToTrips/mental_surveys/Demographic_Breakdown.xlsx") %>%
    rename(id = Metricwire_ID)
  full_join(num_trips, demographic, by = 'id')
}

## INITIAL STATISTICAL ANALYSIS BETWEEN GROUPS 

demoTable <- addDemographicData(num_trips = addMentalHealthResponses(tibble = num_trips)) %>%
  select(id, date, Prescribed_Group, numTrips) %>%
  filter(Prescribed_Group != "No Group",
         Prescribed_Group != "NA")

ggplot(demoTable) +
  aes(x = Prescribed_Group, y = numTrips) +
  geom_boxplot() +
  labs(
    x = "Group",
    y = "Number of Activities"
  )

group_by(demoTable, Prescribed_Group) %>%
  summarise(
    median = median(numTrips, na.rm = TRUE),
    sd = sd(numTrips, na.rm = TRUE)
  )

ggplot(demoTable, aes(x = numTrips)) + 
  geom_histogram(bins = 30, color = "white") +
  theme_bw() +
  facet_wrap(~Prescribed_Group) +
  labs(
    x = "Number of Activities",
    y = "Count"
  ) +
  xlim(c(0,30))

res_aov <- aov(numTrips ~ Prescribed_Group,
               data = demoTable)

summary(res_aov)

TukeyHSD(res_aov)

res_lm <- lm(numTrips ~ Prescribed_Group,
             data = demoTable)

summary(res_lm)

## POISSON DISTRIBUTION
demoTable$Prescribed_Group = relevel(factor(demoTable$Prescribed_Group), ref = "Control") ## set control as reference
poisson <- list("Poisson Regression" = glm(numTrips ~  Prescribed_Group, family="poisson", 
                                           data=demoTable))
modelsummary(poisson, statistic = 'p.value')

## NUM ACTIVITIES ACROSS WHOLE DATASET
ggplot(num_trips, aes(x = numTrips)) + geom_histogram(bins = 30, color = "white") + theme_bw() +
  labs(x = "Number of Activites", y = "Count") +
  xlim(c(0,30))

## DENSITY OF ACTIVITIES ACROSS WHOLE DATASET
boundary <- st_read("Figures/SchultzBounds.shp")

num_trips %>%
  ungroup() %>%
  sample_n(100) %>%
  select(id, algorithm) %>%
  unnest(cols = c(algorithm)) %>%
  st_as_sf() %>%
  st_transform(4326) %>%
  st_crop(boundary) %>%
  transmute(
    id, cluster, 
    x = st_coordinates(geometry)[,1],
    y = st_coordinates(geometry)[,2]
  ) %>%
  ggplot() +
  annotation_map_tile("cartolight", zoom = 12) +
  stat_density2d(aes(x = x, y = y, fill = ..level..), 
                 alpha = 0.3, bins = 45, geom = "polygon")  +
  scale_fill_viridis_c("Activity Density") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=8)) #change legend text font size)






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


optimize2 <- function(cleaned_manual_table, params = c(10,3,36000,1.3)) {
  optim(par = params, fn = calculateError, cleaned_manual_table = cleaned_manual_table,
        method = "L-BFGS-B", upper = c(100, 300, 24 * 3600, 4), lower = c(10,3,300, 1),
        control = list(maxit = 100, parscale = c(25,75,21600,1)))
}