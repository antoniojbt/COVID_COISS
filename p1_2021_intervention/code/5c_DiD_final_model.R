############
# COISS paper 1
# L. Bonifaz
# June 2024

# Difference in difference regression final model and assumption tests

# Input is output from script 5c_DiD_assumptions.R
# Outputs are various tables from Cox PH DiD regression analysis with assumption tests and final model formula to meet Cox PH assumption. No rdata or full dataset.
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
# Variables passed are defined in 5c_DiD.R except for data split, done here

###
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
###

###
# Split data or run with cross-validation
# TO DO: Set manually

set.seed(123)
dim(df)
str(df)

# Split the data into training and test sets:
split <- rsample::initial_split(df, prop = 0.7)
df_train <- rsample::training(split)
df_test <- rsample::testing(split)

dim(df_train)
dim(df_test)


# Set manually:
# df <- df_train
df <- df_test

# data_run <- 'train'
data_run <- 'test'

dim(df)
###


###
# Check some of the variables loaded
analysis_var_df
covars
###
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
# Test of proportional hazard assumption
# Global and several variables do not fulfil PH assumption

###
# Function to re-run is in 5c_DiD_assumptions.R:
# epi_stats_run_coxph(df, mod_form, ...)
###


###
# Piecewise Linear Transformation, needs to be set for each df as not saved in original, full set:
# Divides the continuous variable into segments and fits separate slopes for each segment
df$EDAD_piecewise <- cut(df$EDAD,
                         breaks = c(0, 5, 15, 25, 35, 45, 55, 65, 75, Inf),
                         labels = c("<=5", "6-15", "16-25", "26-35",
                                    "36-45", "46-55", "56-65", "66-75",
                                    "75+")
                         )
summary(df$EDAD_piecewise)

# Needs re-setting here:
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
print(covars_final)
summary(factor(df[[outcome_var]]))
summary(factor(df[[time_var]]))
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
mod_spec <- sprintf('Surv(%s, %s) ~ %s + %s * d_pre_post',
                    time_var, outcome_var,
                    paste('strata(EDAD_piecewise) + strata(NEUMONIA) + strata(RENAL_CRONICA) + strata(d_pre_post) + strata(CARDIOVASCULAR) + OTRA_COM + SEXO + ASMA + INMUSUPR + HIPERTENSION + OBESIDAD'),
                    intervention_var
                    )
mod_spec

mod_form <- as.formula(mod_spec)
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
print(data_run)

file_n <- 'did_final_mod'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s_%s_%s_%s.%s',
                   infile_prefix,
                   file_n,
                   outcome_var,
                   df_name,
                   data_run,
                   suffix
                   )
outfile
epi_write(file_object = various_mods$df_cox_did_covar,
          file_name = outfile
          )


file_n <- 'did_final_mod_ph_test'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s_%s_%s_%s.%s',
                   infile_prefix,
                   file_n,
                   outcome_var,
                   df_name,
                   data_run,
                   suffix
                   )
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
# various_mods$plot_coxzph # original object
# various_mods$ggplot_coxzph  # converted to ggplot object

# Save the plot to disk
file_n <- 'plots_Schoenfeld_residuals'
suffix <- 'pdf'
outfile <- sprintf(fmt = '%s/%s_%s_%s_%s.%s',
                   infile_prefix,
                   file_n,
                   outcome_var,
                   df_name,
                   data_run,
                   suffix
                   )
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
# # TO DO:
# # Check model diagnostics:
# cox_snell <- resid(various_mods, type = "martingale")
# plot(cox_snell, main = "Cox-Snell Residuals", ylab = "Residuals", xlab = "Time")
# # Finally, evaluate the model to ensure that the proportional hazards assumption is satisfied and the model fits the data well.
# 
# # Recalculate and plot martingale residuals for the transformed model
# resids_martingale <- residuals(various_mods, type = "martingale")
# scatter.smooth(df$EDAD_grouped, resids_martingale, xlab = "Age (EDAD_grouped)", ylab = "Martingale Residuals", main = "Martingale Residuals vs. Age transformed")
###
############



############
# The end:
# No rdata or other to save

# Saving screen outputs:
sink()

# q()
############
