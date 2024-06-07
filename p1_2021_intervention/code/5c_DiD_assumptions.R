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
library(ggthemes)
library(cowplot)
library(tidyverse)
library(broom)
library(survival)
library(survminer)
library(car)
############


############
# Load a previous R session, data and objects

# Load .RData file:
infile <- "../data/processed/5c_DiD_R_COVID19MEXICO_2021_2022_COVID-only_COISS-only.rdata.gzip"
load(infile,
     verbose = TRUE
     )
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
# Check loaded objects:
# TO DO: continue here

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
############



############
# Test of proportional hazard assumtion
# Global and several variables do not fulfil PH assumption

###
# Set-up function to re-run:

epi_stats_run_coxph <- function(df, mod_form) {
    
    # Print vars passed:
    print('Variables passed:')
    print(epi_head_and_tail(df))
    print(mod_form)
    
    # Initialise store:
    res <- list()
    
    # Fit:
    cox_did_covar <- coxph(mod_form,
                           data = df
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
    # plot_coxzph <- plot(ph_test) # R plot so side effect only, no object
    plot_coxzph <- ggcoxzph(ph_test, variable = rownames(ph_test$table))
    
    # Store:
    res <- list(cox_did_covar = cox_did_covar,
                df_cox_did_covar = df_cox_did_covar,
                ph_test = ph_test,
                plot_coxzph = plot_coxzph
               )
    # res$plot_coxzph
    
    return(res)
}
###


###
# Stratify by pneumonia, age, etc

analysis_var_df
covars

# Formula:
mod_form <- as.formula(Surv(d_days_to_death_30, d_death_30) ~
                           EDAD + SEXO + strata(NEUMONIA) + 
                           NACIONALIDAD + INDIGENA + EPOC + ASMA + INMUSUPR + HIPERTENSION + 
                           OTRA_COM + CARDIOVASCULAR + OBESIDAD + RENAL_CRONICA + TABAQUISMO + 
                           d_intervention_T2 * d_pre_post
                       )

print(mod_form)


pneum_strat <- epi_stats_run_coxph(df, mod_form)
###



###
# EDAD might not be linear with days to death
# Martingale residuals do not scatter randomly around 0
# Clear curve
# Try transforming 

# Transform EDAD, some are 0 values though so will result in Inf and error
# Add a small constant to avoid log(0):
df$log_time <- log(df$d_days_to_death_30 + 1)
any(df$log_time < 0)

# EDAD has NA's so handle these before conversion:
df <- df[!is.na(df$log_EDAD), ]
# Transform:
df$log_EDAD <- log(df$EDAD + 1)
summary(df$log_EDAD)
any(df$log_EDAD < 0)


mod_form <- as.formula(Surv(d_days_to_death_30, d_death_30) ~
                           log_EDAD + SEXO + strata(NEUMONIA) + 
                           NACIONALIDAD + INDIGENA + EPOC + ASMA + INMUSUPR + HIPERTENSION + 
                           OTRA_COM + CARDIOVASCULAR + OBESIDAD + RENAL_CRONICA + TABAQUISMO + 
                           d_intervention_T2 * d_pre_post
                       )

# Fit:
time_pneum_strat <- epi_stats_run_coxph(df, mod_form)

time_pneum_strat
time_pneum_strat$ph_test

# Recalculate and plot martingale residuals for the transformed model
resids_martingale <- residuals(time_pneum_strat, type = "martingale")
scatter.smooth(df$log_EDAD, resids_martingale, xlab = "Log Age (log_EDAD)", ylab = "Martingale Residuals", main = "Martingale Residuals vs. Log Age")
###


###
# Include Time-Dependent Covariates

# Formula:
mod_form <- as.formula(Surv(d_days_to_death_30, d_death_30) ~
                           EDAD + SEXO + strata(NEUMONIA) + 
                           NACIONALIDAD + INDIGENA + EPOC + ASMA + INMUSUPR + HIPERTENSION + 
                           OTRA_COM + CARDIOVASCULAR + OBESIDAD + RENAL_CRONICA + TABAQUISMO + 
                           d_intervention_T2 * d_pre_post +
                           # HIPERTENSION:log_time + 
                           # CARDIOVASCULAR:log_time +
                           # RENAL_CRONICA:log_time +
                           d_pre_post:log_time
                       )

print(mod_form)

# Fit:
time_dep_pneum_strat <- epi_stats_run_coxph(df, mod_form)

time_dep_pneum_strat
time_dep_pneum_strat$ph_test

###
############


############
# The end:
# No rdata or other to save

# Saving screen outputs:
sink()

# q()
############
