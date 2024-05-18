#Define transformations
transformations <- list(
  log = function(x) log(x + 0.001),  # Log transformation
  sqrt = sqrt,  # Square root transformation
  sqrd = function(x) x^2,
  cubic = function(x) x^3,
  rcs3 = function(x) rcs(x, 3),  # Restricted cubic spline with 3 knots
  rcs4 = function(x) rcs(x, 4),   # Restricted cubic spline with 4 knots
  rcs5 = function(x) rcs(x, 5)   # Restricted cubic spline with 5 knots
)

invisible("Saco los missings")
data_mine2_miss_proc2$lag_dias_treat_imp_sin_na_nomiss<-ifelse(is.na(data_mine2_miss_proc2$lag_dias_treat_imp_sin_na),.0001,data_mine2_miss_proc2$lag_dias_treat_imp_sin_na)

continuous_vars <- c("lag_dias_treat_imp_sin_na_nomiss", "edad_al_ing_1", "ano_nac_corr")

require(data.table)
# Function to build models and store results
build_and_evaluate_models <- function(data, continuous_vars, transformations) {
  models <- list()  # List to store model results
  
  # Base formula with time-dependent coefficients incorporated
  base_formula <- "Surv(lag_time,time,event) ~ susinidum_mar*time + psycom_dum_study*time + psycom_dum_with*time + cond_oc_dum_3unemp*time"
  
  # Iterate over specified continuous variables for transformation
  for (var in continuous_vars) {
    for (trans_name in names(transformations)) {
      # Apply transformation and fit model
      data_transformed <- data
      data_transformed[[var]] <- transformations[[trans_name]](data[[var]])
      full_formula <- as.formula(paste(base_formula, "+", paste(setdiff(names(data), c("time", "event", continuous_vars)), collapse = " + "), "+", trans_name, "(", var, ")+cluster(hash_key)+ strat(tipo_de_plan_2)"))
      model <- cph(full_formula, data = data_transformed, x=TRUE, y=TRUE, surv=TRUE)
      
      # Get AIC and test Schoenfeld residuals
      model_aic <- AIC(model)
      cox_zph <- cox.zph(model)
      cox_zph_global_test <- cox_zph$global
      
      # Store results
      models[[length(models) + 1]] <- list(
        model = model,
        transformation = trans_name,
        variable = var,
        AIC = model_aic,
        global_zph_test_statistic = cox_zph_global_test
      )
    }
  }
  
  return(models)
}

# Build models and store results
model_results <- build_and_evaluate_models(dplyr::mutate(data_mine2_miss_proc2) %>% as.data.table(), 
                                           continuous_vars, 
                                           transformations)
#Error in if (!length(fname) || !any(fname == zname)) { : 
#valor ausente donde TRUE/FALSE es necesario



# Function to build models and select the best one
select_best_model <- function(data, predictors, transformations) {
  best_aic <- Inf
  best_model <- NULL
  
  # Iterate over only specified predictors for transformation
  for (predictor in predictors) {
    for (trans_name in names(transformations)) {
      # Apply transformation and fit model
      data_transformed <- transform(data, !!predictor := transformations[[trans_name]](.[[predictor]]))
      formula <- as.formula(paste("Surv(lag_time,time,event) ~", 
                                  paste(predictors, collapse = " + "),"+ cluster(hash_key)+ strat(tipo_de_plan_2)"))
      model <- cph(formula, data = data_transformed, x=TRUE, y=TRUE, surv=TRUE)
      
      # Check AIC and update best model if current model is better
      current_aic <- AIC(model)
      if (current_aic < best_aic) {
        best_aic <- current_aic
        best_model <- model
      }
    }
  }
  
  return(best_model)
}

# Specify predictors for transformation
selected_predictors <- c("predictor1", "predictor2")

# Run the model selection
best_cox_model <- select_best_model(data, selected_predictors, transformations)

# Output the summary of the best model
summary(best_cox_model)