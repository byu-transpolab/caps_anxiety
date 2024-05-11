## METHODS
# Descriptive statistics
desc_stats <- function(demo_ids) {
  descriptive_stats <- demo_ids %>% 
    filter(!is.na(userId)) %>% 
    select(age, fsiq_2, prescribed_group, sex, race) %>% 
    mutate(fsiq_2 = as.numeric(fsiq_2),
           prescribed_group = fct_recode(prescribed_group, Control = "No Group")) %>%
    rename(Age = age,
           IQ = fsiq_2,
           Group = prescribed_group,
           Sex = sex,
           Race = race)
}