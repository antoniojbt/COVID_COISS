
############
# Assumptions for the Cox Proportional Hazards (PH) model

# Proportional Hazards Assumption
# The hazard ratios between any two individuals are constant over time

# Schoenfeld Residuals
  # If the residuals show no systematic patterns and the p-values are non-significant, the assumption holds
  cox_model <- coxph(Surv(time, status) ~ covariates, data = train_data, robust = TRUE)
  ph_test <- cox.zph(cox_model)
  print(ph_test)
  plot(ph_test)

# Log-Log survival curves for different levels of covariates
  # Parallel log-log curves indicate the proportional hazards assumption holds
  library(survminer)
  ggsurvplot(survfit(cox_model), fun = "cloglog")

# Linearity of Continuous Covariates
# The relationship between the log hazard and continuous covariates is linear

# Martingale Residuals for continuous covariates
  # No systematic pattern should be present; if non-linearity is detected, consider transforming the covariate
  martingale_residuals <- residuals(cox_model, type = "martingale")
  plot(train_data$continuous_covariate, martingale_residuals)


# No Interaction Between Covariates and Time
# The effect of covariates on the hazard is constant over time
# Time-Dependent Covariates
  # Include interaction terms between covariates and functions of time (e.g., `time` or `log(time)`) in the model and test their significance
  # Non-significant interaction terms suggest no time-varying effects
  cox_time_dep <- coxph(Surv(time, status) ~ covariate1 * log(time) + covariate2,
                        data = train_data)
  summary(cox_time_dep)


# No Significant Outliers or Influential Points
# No individual data point unduly influences the model fit
# Deviance Residuals to identify outliers
  # Large residuals may indicate outliers
  deviance_residuals <- residuals(cox_model, type = "deviance")
  plot(deviance_residuals)

# Influence Statistics (dfbeta, dfbetas, etc.)
  # High values indicate influential points
  influence_measures <- residuals(cox_model, type = "dfbeta")
  plot(influence_measures)

# Independence of Survival Times and Censoring
# Censoring is non-informative, meaning the survival times are independent of the censoring mechanism
# Plot the Kaplan-Meier survival curves and check if the curves for censored and uncensored data are similar
# Sensitivity Analysis: vary the censoring mechanism and observe its impact on the model results
############


############
# Diagnostics

# Cox model
cox_model <- coxph(Surv(time, status) ~ covariates, data = train_data, robust = TRUE)

# Proportional Hazards Assumption:
ph_test <- cox.zph(cox_model)
print(ph_test)
plot(ph_test)

# Linearity Assumption:
martingale_residuals <- residuals(cox_model, type = "martingale")
plot(train_data$continuous_covariate, martingale_residuals)

# Time-Dependent Covariates:
cox_time_dep <- coxph(Surv(time, status) ~ covariate1 * log(time) + covariate2, data = train_data)
summary(cox_time_dep)

# Outliers with Deviance Residuals:
deviance_residuals <- residuals(cox_model, type = "deviance")
plot(deviance_residuals)

# Influential Points:
influence_measures <- residuals(cox_model, type = "dfbeta")
plot(influence_measures)
############
