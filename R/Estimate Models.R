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


#' Perform Hausman Test
#'
#' This function performs the Hausman test to compare the efficiency of fixed effects 
#' and random effects models.
#'
#' @param fixed A fixed effects model object.
#' @param random A random effects model object.
#'
#' @return A p-value indicating the significance of the Hausman test.

hausman <- function(fixed, random) {
  phtest(fixed,random)
}


#' Summarize Regression Models
#'
#' This function summarizes the results of multiple regression models.
#'
#' @param ols An object of class "lm" representing the Ordinary Least Squares (OLS) regression model.
#' @param fe An object of class "plm" representing the Fixed Effects regression model.
#' @param re An object of class "plm" representing the Random Effects regression model.
#'
#' @return A summary table of regression model coefficients and statistics.

compare_models <- function(ols, fe, re) {
  
  models <- list(
    "Ordinary Least Squares Model" = ols,
    "Fixed Effects Model" = fe,
    "Random Effect Model" = re
  )
  
  modelsummary(models, 
               estimate = c("{estimate}({statistic}){stars}"),
               statistic = NULL, 
               coef_rename = c("sev_day_avg" = "7-Day Rolling Avg. Number of Activities"),
               gof_omit = 'RMSE|AIC|BIC'
  )
}


#' Fixed Effects Analysis
#'
#' This function conducts analysis using fixed effects models.
#'
#' @param fe A fixed effects model object.
#' @param demo A data frame containing demographic information.
#'
#' @return A summary of the linear regression model examining the relationship 
#' between the intercepts from the fixed effects model and demographic variables.

fe_analysis <- function(fe, demo) {
  fixef <- data.frame(userId = names(fixef(fe)), intercept = fixef(fe)) %>% 
    as_tibble()
  
  fe_model <- right_join(demo, fixef, by = "userId") %>% 
    mutate(sex = if(is.ordered(sex)) sex else relevel(as.factor(sex), ref = "Male"),
           fsiq_2 = as.numeric(fsiq_2),
           prescribed_group = fct_recode(prescribed_group, Control = "No Group"))
  
  int_model <- list(
    "Intercept Model" = lm(intercept ~ sex + age + fsiq_2 + prescribed_group, data = fe_model))
  
  modelsummary(int_model,
               estimate = c("{estimate} ({statistic}){stars}"),
               statistic = NULL, 
               coef_rename = c("sexFemale" = "Female", 
                               "age" = "Age", 
                               "fsiq_2" = "IQ", 
                               "prescribed_groupAutism" = "Autism", 
                               "prescribed_groupSocial Anxiety" = "Social Anxiety"),
               gof_omit = 'RMSE|AIC|BIC',
               fmt = 2
  )
}




