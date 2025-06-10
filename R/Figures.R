## METHODS
# Descriptive statistics
build_descriptives <- function(demo_ids, model_data) {

  descriptive_stats <- demo_ids |> 
       tibble() |>
    filter(!is.na(userId)) |> 
    select(userId, age, fsiq_2, prescribed_group, sex, race) |> 
    mutate(fsiq_2 = as.numeric(fsiq_2),
           prescribed_group = fct_recode(prescribed_group, Control = "No Group")) %>%
    rename(Age = age,
           IQ = fsiq_2,
           Group = prescribed_group,
           Sex = sex,
           Race = race)

       
  acts <- model_data |> 
       tibble() |> 
       filter(userId %in% descriptive_stats$userId) |> 
       select(userId, date, numTrips, sev_day_follow, sev_day_leadin) |> 
       group_by(userId) |> 
       summarise(`Avg Trips`= mean(numTrips, na.rm = TRUE),
                 `7-day Average trips - after survey` = mean(sev_day_follow, na.rm = TRUE),
                 `7-day Average trips - before survey`= mean(sev_day_leadin, na.rm = TRUE))

  left_join(descriptive_stats, acts, by = "userId")
}