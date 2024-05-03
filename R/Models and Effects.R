
library(targets)
library(tidyverse)
library(modelsummary)

tar_load(final_table)

data_models <- final_table %>% 
  mutate(sex = if(is.ordered(sex)) sex else relevel(as.factor(sex), ref = "Male"),
         fsiq_2 = as.numeric(fsiq_2),
         energy = as.numeric(energy_day_q1, na.rm = TRUE),
         motivation = as.numeric(motivation_day_q2, na.rm = TRUE),
         prescribed_group = fct_recode(final_table$prescribed_group, Control = "No Group")) %>% 
  filter(!is.na(sev_day_avg))

# Models to predict the sev day rolling average number of activities
models <- list(
  "Basic Demographics" = glm(sev_day_avg ~ sex + age + fsiq_2, data = data_models, family = "poisson"),
  "Demographics and Group" = glm(sev_day_avg ~ sex + age + fsiq_2 + prescribed_group, data = data_models, family = "poisson"),
  "Demographics, Group, and Suicide Ideation" = glm(sev_day_avg ~ sex + age + fsiq_2 + prescribed_group + suicidal_ideation_q31_even, data = data_models, family = "poisson"),
  "Demographics, Group, and Energy" = glm(sev_day_avg ~ sex + age + fsiq_2 + prescribed_group  + energy, data = data_models, family = "poisson"),
  "Demographics, Group, and Motivation" = glm(sev_day_avg ~ sex + age + fsiq_2 + prescribed_group  + motivation, data = data_models, family = "poisson"),
  "Demographics, Group, Suicide, and Energy" = glm(sev_day_avg ~ sex + age + fsiq_2 + prescribed_group  + suicidal_ideation_q31_even + energy + motivation, data = data_models, family = "poisson")
  )

modelsummary(models, 
             estimate = c("{estimate}({statistic}){stars}"),
             statistic = NULL, 
             coef_rename = c("sexFemale" = "Female", 
                             "age" = "Age", 
                             "fsiq_2" = "IQ", 
                             "prescribed_groupAutism" = "Autism", 
                             "prescribed_groupSocial Anxiety" = "Social Anxiety", 
                             "suicidal_ideation_q31_evenTRUE" = "Suicidal Ideation",
                             "energy" = "Energy",
                             "motivation" = "Motivation")
             )

# Models to predict the energy levels
s_base <- glm(energy ~ sex + age + fsiq_2 + prescribed_group, data = data_models, family = "gaussian")
s_distance <- update(s_base, .~. + length)
s_area <- update(s_base, .~. + area)
s_numTrips <- update(s_base, .~. + numTrips)
s_numTrips_lag <- update(s_numTrips, .~. + sev_day_avg)

s_models <- list(
  "Base Demo" = s_base,
  "Distance" = s_distance,
  "Area" = s_area,
  "Number of Activities" = s_numTrips,
  "7-Day Rolling Average " = s_numTrips_lag
  )

modelsummary(s_models,
             estimate = c("{estimate}({statistic}){stars}"),
             statistic = NULL,
             coef_rename = c("sexFemale" = "Female", 
                             "age" = "Age", 
                             "fsiq_2" = "IQ", 
                             "prescribed_groupAutism" = "Autism", 
                             "prescribed_groupSocial Anxiety" = "Social Anxiety", 
                             "length" = "Distance Traveled",
                             "area" = "Convex Hull Area",
                             "numTrips" = "Number of Activities",
                             "sev_day_avg" = "7-Day Rolling Average")
             )

# Distributions for numActivities, Area, and Length
ggplot(final_table, aes(x = numTrips)) +
  geom_histogram(binwidth = 1, fill = "darkgray", color = "black") +
  geom_text(stat = "count", aes(label = after_stat(count)),
            vjust = -0.5, color = "black", size = 3) +
  labs(title = "Distribution of Number of Activities", x = "Number of Activities", y = "Frequency") +
  theme_bw()

ggplot(final_table, aes(x = area/(1e6))) +
  geom_histogram(fill = "darkgray", color = "black") +
  scale_x_log10() +
  labs(title = "Distribution of Area Covered", x = "Area (mm^2)", y = "Frequency") +
  theme_bw()

ggplot(final_table, aes(x = length/1000)) +
  geom_histogram(fill = "darkgray", color = "black") +
  scale_x_log10() +
  labs(title = "Distribution of Distance Traveled", x = "Length (km)", y = "Frequency") +
  theme_bw()


## FIXED EFFECTS AND RANDOM EFFECTS

library(modelr)
library(tidyverse)
library(foreign)
library(plm)

effects <- data_models %>% 
  filter(!is.na(motivation))

# fixed model
fixed <- plm(motivation ~ sex + age + fsiq_2 + prescribed_group , index = c("userId", "activityDay"), data = effects, model = "within")  
fixef(fixed) 


fixed_dum <-lm(motivation ~ sex + age + fsiq_2 + prescribed_group + factor(userId) - 1, data = effects)
summary(fixed_dum)


#random model
random <- plm(motivation ~ activityDay, c("userId", "activityDay"), data = effects, model = "random")  

# Hausman test to compare the fixed and random
phtest(fixed,random) 



