#' Prepare Models Data
#'
#' This function prepares the input data for modeling by performing several preprocessing steps.
#'
#' @param final_table A tibble containing the raw data for modeling.
#'
#' @return A preprocessed data frame ready for modeling.

prep_models <- function(data) {
  data_models <- data %>% 
    mutate(sex = if(is.ordered(sex)) sex else relevel(as.factor(sex), ref = "Male"),
           fsiq_2 = as.numeric(fsiq_2),
           energy = as.numeric(energy_day_q1, na.rm = TRUE),
           motivation = as.numeric(motivation_day_q2, na.rm = TRUE),
           prescribed_group = fct_recode(prescribed_group, Control = "No Group")) %>%
    select(-algorithm) %>% 
    as.data.frame()
}


#' Estimate Models for Suicidal Ideation
#'
#' @param model_data A tibble containing the data for analysis.
#' @return A list of summaries for each fitted model.
#'
#' @details The function fits several logistic regression models to predict 
#' suicidal ideation. It returns a summary for each model

estimate_models <- function(data_models) {
  
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
                               "motivation" = "Motivation"),
               gof_omit = 'RMSE|AIC|BIC'
  )
}


#' Ordinary Least Squares (OLS) Model
#'
#' This function fits an Ordinary Least Squares (OLS) regression model.
#'
#' @param model_data A data frame containing the variables for modeling.
#'
#' @return An object of class "lm" representing the fitted OLS regression model.

ols_model <- function(model_data) {
  ols <- lm(motivation ~ sev_day_avg, data = model_data)
  return(ols)
}


#' Estimate Fixed Effects Model
#'
#' This function estimates a fixed effects model using the provided model data.
#'
#' @param model_data A data frame containing the data for modeling.
#'
#' @return A fixed effects model object.

fixed_effects <- function(model_data) {
  fixed <- plm(motivation ~ sev_day_avg, index = c("userId", "activityDay"), data = model_data, model = "within")
  return(fixed)
}


#' Estimate Random Effects Model
#'
#' This function estimates a random effects model using the provided model data.
#'
#' @param model_data A data frame containing the data for modeling.
#'
#' @return A random effects model object.

random_effects <- function(model_data) {
  random <- plm(motivation ~ sev_day_avg, index = c("userId", "activityDay"), data = model_data, model = "random") 
  return(random)
}
  
  glm4 <- glm(suicidal_ideation_q31_even ~ race, data = data, family = "binomial")
  
  glm5 <- glm(suicidal_ideation_q31_even ~ initial_group + gender + race, data = data, family = "binomial")
  
  glm6 <- update(glm5, formula = .~.+numTrips)
  
  summaries <- list(summary(glm1), summary(glm2), summary(glm3), summary(glm4), summary(glm5), summary(glm6))
  
  return(summaries)
}



