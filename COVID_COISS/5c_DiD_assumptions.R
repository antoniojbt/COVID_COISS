############
# COISS paper 1
# L. Bonifaz
# June 2024

# Difference in difference regression model variants and assumption checks

# Input is output from script 5c_DiD.R
# Outputs are various tables from Cox PH DiD regression analysis with assumption tests and model formula modifcation to meet Cox PH assumption. Mostly to screen. No rdata or full dataset.
############


############
project_loc <- '/Users/antoniob/Documents/work/science/projects/projects/ongoing/laura_bonifaz/COVID_COISS/p1_2021_intervention/results/'
getwd()
setwd(project_loc)
############


############
# Import libraries
library(data.table)
library(episcout)
library(tidyverse)
library(ggthemes)
library(ggplotify)
library(cowplot)
library(broom)
library(survival)
library(survminer)
library(car)
library(splines)
library(rsample)
############


############
# Load a previous R session, data and objects

# Load .RData file:
infile <- "../data/processed/5c_DiD_R_COVID19MEXICO_2021_2022_COVID-only_COISS-only.rdata.gzip"
load(infile, verbose = TRUE)
data_f <- data_f # just to get rid of RStudio warnings
dim(data_f)
str(data_f)
epi_head_and_tail(data_f)
colnames(data_f)

# For saving/naming outputs, should already come with the RData file though:
infile_prefix <- infile_prefix
infile_prefix
# Otherwise manually:
# infile_prefix <- 'COVID19MEXICO2021'

outfile <- outfile
outfile
############


############
# RStudio gives warnings of objects not in scope
# Should be in environment, remove warnings:

data_f_T1 <- data_f_T1
data_f_T2 <- data_f_T2
time_cuts <- time_cuts
deaths_periods_list <- deaths_periods_list
df_time_cuts <- df_time_cuts
covars <- covars
outcome_var <- outcome_var
time_var <- time_var
intervention_var <- intervention_var
df_name <- df_name
analysis_cut <- analysis_cut
events_analysis_cut <- events_analysis_cut
baseline_cut <- baseline_cut
events_baseline <- events_baseline
df <- df
df_analysis_cut <- df_analysis_cut
df_baseline <- df_baseline
analysis_var_df <- analysis_var_df
############


############
# Check some of the variables loaded
analysis_var_df
covars

# Re-run once the Cox PH assumptions are met in one
############


############
# Save screen outputs

# All the way down here as need variable names defined above:
script_n <- '5c_DiD_assumptions_R'
time_stamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
df_name
sink_fn <- sprintf("%s/sink_%s_%s_%s.txt", infile_prefix, script_n, df_name, time_stamp)
sink_fn
sink(sink_fn)
getwd()

timestamp()

print('File loaded:')
print(infile)
print('Loaded data and objects:')
ls()
# sink()
############



############
# Split data or run with cross-validation
# TO DO: Set manually

set.seed(123)

# Split the data into training and test sets:
split <- rsample::initial_split(df, prop = 0.7)
df_train <- rsample::training(split)
df_test <- rsample::testing(split)

df <- df_train

# # Cross validation:
# train_control <- caret::trainControl(method = "cv", number = 10)
# 
# # Train the model using cross-validation, e.g.:
# cox_model <- caret::train(
#   Surv(time, status) ~ age + sex,
#   data = df,
#   method = "coxph",
#   trControl = train_control
#   )
# # Print the results of cross-validation
# print(cox_model)
# 
# # Fit the final Cox PH model on the entire dataset
# final_fit <- coxph(Surv(time, status) ~ age + sex, data = lung)
# print(final_fit)
############


############
# Test of proportional hazard assumption
# Global and several variables do not fulfil PH assumption

###
# Set-up function to re-run:

epi_stats_run_coxph <- function(df, mod_form, ...) {
    
    # Print vars passed:
    print('Variables passed:')
    print(epi_head_and_tail(df))
    print(mod_form)
    
    # Initialise store:
    res <- list()
    
    # Fit:
    cox_did_covar <- coxph(mod_form,
                           data = df,
                           ... # additional parameters here
                           )

    # Summary:
    # str(cox_did_covar)
    # summary(cox_did_covar)
  
    # Tidy up:
    df_cox_did_covar <- tidy(cox_did_covar)
    df_cox_did_covar$odds_ratio <- exp(df_cox_did_covar$estimate)
    # df_cox_did_covar
  
    # Test proportional hazards assumption:
    ph_test <- cox.zph(cox_did_covar)
    # str(ph_test)
  
    # Schoenfeld residuals:
    plot_coxzph <- ggcoxzph(ph_test, variable = rownames(ph_test$table))
    # Plots, convert to ggplot objects so that they can be saved:
    plot_list <- list()
    # Combine plots into a single ggplot object:
    for (i in seq_along(plot_coxzph)) {
        plot_list[[i]] <- as.ggplot(plot_coxzph[[i]])
        }

    # Store:
    res <- list(cox_did_covar = cox_did_covar,
                df_cox_did_covar = df_cox_did_covar,
                ph_test = ph_test,
                plot_coxzph = plot_coxzph,
                ggplot_coxzph = plot_list
                )
    # res$plot_coxzph

    return(res)
}
###


###
# Stratify by pneumonia, age, etc

# Formula:
mod_form <- as.formula(Surv(d_days_to_death_30, d_death_30) ~
                           EDAD + SEXO + strata(NEUMONIA) + 
                           NACIONALIDAD + INDIGENA + EPOC + ASMA + INMUSUPR + HIPERTENSION + 
                           OTRA_COM + CARDIOVASCULAR + OBESIDAD + RENAL_CRONICA + TABAQUISMO + 
                           d_intervention_T2 * d_pre_post
                       )

print(mod_form)


pneum_strat <- epi_stats_run_coxph(df, mod_form)
pneum_strat$ph_test
# Much better but still doesn't meet assumption
# Check EDAD
###



###
# EDAD might not be linear with days to death
# Martingale residuals do not scatter randomly around 0
# Clear curve
# Try transforming 

# Transform EDAD, some are 0 values though so will result in Inf and error
# EDAD has NA's so handle these before conversion
df$EDAD_log <- log(df$EDAD + 1)
df <- df[!is.na(df$EDAD_log), ]
summary(df$EDAD_log)
any(df$log_EDAD < 0) # should be FALSE

mod_form <- as.formula(Surv(d_days_to_death_30, d_death_30) ~
                           EDAD_log +
                           strata(NEUMONIA) +
                           SEXO + NACIONALIDAD + INDIGENA +
                           EPOC + ASMA + INMUSUPR +
                           HIPERTENSION + OTRA_COM + CARDIOVASCULAR +
                           OBESIDAD + RENAL_CRONICA + TABAQUISMO +
                           d_intervention_T2 * d_pre_post
                       )


# Fit:
time_pneum_strat <- epi_stats_run_coxph(df, mod_form)
time_pneum_strat
time_pneum_strat$ph_test

# Recalculate and plot martingale residuals for the transformed model
resids_martingale <- residuals(time_pneum_strat, type = "martingale")
scatter.smooth(df$EDAD_log, resids_martingale, xlab = "Log Age (EDAD_log)", ylab = "Martingale Residuals", main = "Martingale Residuals vs. Log Age")

# Didn't do much for the model but the residuals now look more random
###


###
# Other transformations
# Scale continuous variables
df$EDAD_scaled <- scale(df$EDAD)
sum(is.na(df$EDAD_scaled))
summary(df$EDAD_scaled)

# Formula:
mod_form <- as.formula(Surv(d_days_to_death_30, d_death_30) ~
                           EDAD_scaled +
                           strata(NEUMONIA) +
                           strata(RENAL_CRONICA) +
                           SEXO + NACIONALIDAD + INDIGENA +
                           EPOC + ASMA + INMUSUPR +
                           HIPERTENSION + OTRA_COM + CARDIOVASCULAR +
                           OBESIDAD + TABAQUISMO +
                           d_intervention_T2 * d_pre_post
                       )

print(mod_form)

# Fit:
EDAD_scaled_fit <- epi_stats_run_coxph(df, mod_form)
EDAD_scaled_fit$ph_test
# Much better but global still significant 

# Recalculate and plot martingale residuals for the transformed model
resids_martingale <- residuals(EDAD_scaled_fit, type = "martingale")
scatter.smooth(df$EDAD_scaled, resids_martingale, xlab = "Age scaled (EDAD_scaled)", ylab = "Martingale Residuals", main = "Martingale Residuals vs. Age scaled")
# EDAD all over the place now, should work better, try modifying other variables
###


###
# Various tests with transformations, stratification, time-dependent covariates and simpler models
df$log_time <- log(df$d_days_to_death_30 + 1)
any(df$log_time < 0) # should be FALSE

# Different transformation for time-dependent covariate:
df$EDAD_time <- df$d_days_to_death * df$EDAD
summary(df$EDAD_time)

# Try other transformation for EDAD:
df$EDAD_squared <- df$EDAD^2
sum(is.na(df$EDAD_squared))
summary(df$EDAD_squared)

# Categorical:
df$EDAD_grouped <- cut(df$EDAD,
                       breaks = c(0, 20, 40, 60, 80, Inf),
                       labels = c("0-20", "21-40", "41-60", "61-80", "80+")
                       )

# Restricted Cubic Splines:
df$EDAD_spline <- splines::ns(df$EDAD, df = 4) # degrees of freedom
summary(df$EDAD_spline)

# Piecewise Linear Transformation:
# Divides the continuous variable into segments and fits separate slopes for each segment
df$EDAD_piecewise <- cut(df$EDAD,
                         breaks = c(0, 5, 15, 25, 35, 45, 55, 65, 75, Inf),
                         labels = c("<=5", "6-15", "16-25", "26-35",
                                    "36-45", "46-55", "56-65", "66-75",
                                    "75+")
                         )
summary(df$EDAD_piecewise)
summary(df$EDAD_grouped)

# Following didn't work:
# Fit:
# time_dep_pneum_strat <- epi_stats_run_coxph(df, mod_form)
# Substantially worse with d_pre_post:log_time

# renal_strata_pneum_strat_log_edad <- epi_stats_run_coxph(df, mod_form)
# Improved but still not meeting the assumption

# log_EDAD_log_time <- epi_stats_run_coxph(df, mod_form)
# Substantially worse log_EDAD_log_time$ph_test

# EDAD_scaled_only <- epi_stats_run_coxph(df, mod_form)
# Other vars worse, needs stratifying

# EDAD_scaled_pneum_strat <- epi_stats_run_coxph(df, mod_form)
# RENAL needs stratifying

# EDAD_piecewise looks good, but gave NaNs, and GLOBAL still significant

# Using Surv(d_days_to_death, d_death) compared to d_days_to_death_30, d_death_30 gave similar results, slightly better.

# +- OK but EDAD and global significant and NaNs:
# EDAD_piecewise
# EDAD_grouped

# Also worse:
  # EDAD_scaled:log_time
  # d_pre_post:log_time
  # EDAD_piecewise:log_time
  # EDAD_grouped:log_time
  # EDAD_time
###


###
# Checked for multicollinearity using VIF again, without interactions, all good

# Fit a preliminary Cox model without interactions to check VIF
mod_form_no_interactions <- Surv(d_days_to_death_30, d_death_30) ~ EDAD + SEXO + NACIONALIDAD + INDIGENA + EPOC + ASMA + INMUSUPR + HIPERTENSION + OTRA_COM + CARDIOVASCULAR + OBESIDAD + RENAL_CRONICA + TABAQUISMO + d_intervention_T2 + d_pre_post

cox_no_interactions <- coxph(mod_form_no_interactions, data = df)

# Extract the design matrix from the model
design_matrix <- model.matrix(cox_no_interactions)[, -1]  # Remove the intercept column


vif_results <- vif(design_matrix)
print(vif_results)
###


###
covars_final <- c(time_var,
                  outcome_var,
                  'EDAD_piecewise',
                  'NEUMONIA',
                  'RENAL_CRONICA',
                  'd_pre_post',
                  'CARDIOVASCULAR',
                  'OTRA_COM',
                  'SEXO',
                  'ASMA',
                  'INMUSUPR',
                  'HIPERTENSION',
                  'OBESIDAD',
                  intervention_var
                  )
# NACIONALIDAD + # removing as non-significant, parsimony
# INDIGENA + # removing as non-significant, parsimony
# EPOC + # removing as non-significant, parsimony
# TABAQUISMO + # removing as non-significant, parsimony

summary(df[[outcome_var]])
summary(df[[time_var]])
summary(df[[intervention_var]])
summary(df[, covars_final])

# Remove missing values:
dim(df)
df_clean <- na.omit(df[, covars_final])
summary(df_clean) # final covars only now
dim(df_clean)
###



###
# Final models

# Formula:
mod_form <- as.formula(Surv(d_days_to_death, d_death) ~
                           strata(EDAD_piecewise) +
                           strata(NEUMONIA) +
                           strata(RENAL_CRONICA) +
                           strata(d_pre_post) +
                           strata(CARDIOVASCULAR) +
                           OTRA_COM +
                           SEXO +
                           # NACIONALIDAD + # removing as non-significant, parsimony
                           # INDIGENA + # removing as non-significant, parsimony
                           # EPOC + # removing as non-significant, parsimony
                           ASMA + INMUSUPR +
                           HIPERTENSION +
                           OBESIDAD +
                           # TABAQUISMO + # removing as non-significant, parsimony
                           d_intervention_T1 * d_pre_post
                       )

print(mod_form)

# Fit, use Robust Standard Errors:
various_mods <- epi_stats_run_coxph(df_clean, mod_form, robust = TRUE) # final covars only
warnings()

#     Warning messages:
# 1: In sqrt(x$var[i, i] * seval) : NaNs produced
# 2: In sqrt(x$var[i, i] * seval) : NaNs produced
# Are coming from:
# plot_coxzph <- ggcoxzph(ph_test, variable = rownames(ph_test$table))

names(various_mods)
various_mods$cox_did_covar
various_mods$ph_test


# Save results:
file_n <- 'did_final_mod'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s_%s_%s.%s', infile_prefix, file_n, outcome_var, df_name, suffix)
outfile
epi_write(file_object = various_mods$df_cox_did_covar,
          file_name = outfile
          )


file_n <- 'did_final_mod_ph_test'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s_%s_%s.%s', infile_prefix, file_n, outcome_var, df_name, suffix)
outfile
epi_write(file_object = as.data.frame(various_mods$ph_test$table),
          file_name = outfile
          )

###
############


############
###
# Diagnostics

# Schoenfeld residuals plots:
various_mods$plot_coxzph # original object
various_mods$ggplot_coxzph  # converted to ggplot object

# Save the plot to disk
i <- 'Schoenfeld_residuals'
file_n <- 'plots'
suffix <- 'pdf'
infile_prefix
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
my_plot_grid <- epi_plots_to_grid(plot_list = various_mods$ggplot_coxzph)
epi_plot_cow_save(file_name = outfile,
                  plot_grid = my_plot_grid,
                  base_width = 25,
                  base_height = 25
                  )
# Interpretation:
# The cox.zph function provides a global test and individual tests for each covariate. The plots show the residuals over time, with a smooth curve. If the curve is approximately horizontal (=), the proportional hazards assumption holds for that covariate.
###


###
# TO DO:
# Check model diagnostics:
cox_snell <- resid(various_mods, type = "martingale")
plot(cox_snell, main = "Cox-Snell Residuals", ylab = "Residuals", xlab = "Time")
# Finally, evaluate the model to ensure that the proportional hazards assumption is satisfied and the model fits the data well.

# Recalculate and plot martingale residuals for the transformed model
resids_martingale <- residuals(various_mods, type = "martingale")
scatter.smooth(df$EDAD_grouped, resids_martingale, xlab = "Age (EDAD_grouped)", ylab = "Martingale Residuals", main = "Martingale Residuals vs. Age transformed")
###
############



############
# The end:
# No rdata or other to save

# Saving screen outputs:
sink()

# q()
############
