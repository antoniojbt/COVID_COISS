############
# COISS paper 1
# L. Bonifaz
# June 2024
# Multiple variable regression setup

# Input is output from script 5_regression_setup.R
# Output are various tables from multi-variable regression analysis for each of the intervention groups (T1 group and T2 group). No rdata or full dataset.
############


############
# Save screen outputs:
sink("COVID19MEXICO_2021_2022_COVID-only_COISS-only/sink_output_5b_multivars_regs.R.txt")
getwd()
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
library(lme4)
library(survival)
library(survminer)
library(caret)
library(pROC)
############


############
# Load a previous R session, data and objects:
infile <- "../data/processed/5_regression_setup_COVID19MEXICO_2021_2022_COVID-only_COISS-only.rdata.gzip"
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
###
# Set-up analysis variables:
outcome_var <- 'd_death_30'
# outcome_var <- 'd_death'
time_var <- 'd_days_to_death_30'
# time_var <- 'd_days_to_death'

# Intervention var:
# intervention_var <- 'd_intervention_T1'
intervention_var <- 'd_intervention_T2'

# Set data frame to use:
data_f_T1 <- data_f_T1 # to remove RStudio warnings
data_f_T2 <- data_f_T2 # to remove RStudio warnings

# df <- data_f_T1
# df_name <- 'data_f_T1'

df <- data_f_T2
df_name <- 'data_f_T2'

# df <- data_f
# df <- data_f_sub

print('Data frame in use is:')
print(df_name)
dim(df)
print(df)
colnames(df)
###
############


############
# Set up and loop

# Total pop at time point:
summary(df$d_time_cuts_prev)

# Pop admitted at time-point:
summary(df$d_time_cuts_INGRESO)

# Pop suffered event at time-point, factor:
summary(df$d_time_cuts_DEF)

# Pop suffered event at time-point, integer. Already loaded:
deaths_periods_list <- deaths_periods_list

lapply(df[, deaths_periods_list], function(x) summary(as.factor(x)))
# 0's look balanced as coded for all if no event at time-period for individual
dim(df)
df[[intervention_var]]
###


###
# Analyse covariates and outcome vars to check colinearity and other issues
# See code snippets from log_reg_assumptions.R and covar_selection.R
# Done in covar_selection.R
###


###
# Set-up covariates
covars <- covars
print(covars)
# from 5_regression_setup.R
###


###
# Initialize storage for results, already have proportions, tables and chi-sq:
# tables_list <- list()
# props_list <- list()
# chi_tests_list <- list()
glm_results_list <- list()

# Keep track of indices:
count <- 0

# Simple regression:
# covars <- 'd_intervention'

# Get windows for analysis / time periods, but exclude NAs ('true_NA' is a place-holder)
# Already loaded:
time_cuts <- time_cuts

# GLM, no interaction terms:
for (i in time_cuts) {
    print(i)
    count <- count + 1
    # by_time_cuts[[i]] <- df[df$d_time_cuts == i, ]
    sub_df <- df[df$d_time_cuts_prev == i, ]
    outcome_var_time <- deaths_periods_list[count]
    summary(as.factor(sub_df[[outcome_var_time]]))
    
    # Outcomes for time cut subsets were saved as char as ifelse() was problematic, convert to integer:
    sub_df[[outcome_var_time]] <- as.integer(sub_df[[outcome_var_time]])
    summary(as.factor(sub_df[[outcome_var_time]]))
    # Check rows with NAs don't appear, issue from script 1:
    print(epi_head_and_tail(sub_df))
    
    # GLM spec:
    mod_spec <- sprintf("%s ~ %s", outcome_var_time, paste(covars, collapse = " + "))
    mod_spec
    mod_form <- as.formula(mod_spec)
    print(mod_form)
    
    mod <- glm(mod_form,
                   data = sub_df,
                   family = binomial
                   )
    
    # Store:
    glm_summary <- tidy(mod)
    glm_summary$time_cut <- i
    # Get odds ratio from the log odds:
    glm_summary$odds_ratio <- exp(glm_summary$estimate)
    glm_results_list[[i]] <- glm_summary
    # print(summary(mod))
    
}

# Combine GLM results into a single data frame
glm_results_df <- do.call(rbind, glm_results_list)

# View(glm_results_df)

# Save:
file_n <- 'glm_results_time_points_covars'
suffix <- 'txt'
outcome_var
df_name
outfile <- sprintf(fmt = '%s/%s_%s_%s.%s', infile_prefix, file_n, outcome_var, df_name, suffix)
outfile
epi_write(file_object = glm_results_df,
          file_name = outfile
          )
###
############


############
# No changes to the rdata file loaded, no need to save again.
# # The end:
# # Save objects, to eg .RData file:
# folder <- '../data/processed'
# script <- '5a_multivars_regs'
# infile_prefix
# suffix <- 'rdata.gzip'
# outfile <- sprintf(fmt = '%s/%s_%s.%s', folder, script, infile_prefix, suffix)
# outfile
# 
# # Check and remove objects that are not necessary to save:
# object_sizes <- sapply(ls(), function(x) object.size(get(x)))
# object_sizes <- as.matrix(rev(sort(object_sizes))[1:10])
# object_sizes
# objects_to_save <- (c('data_f', 'infile_prefix', 'outfile'))
# 
# # Save:
# save(list = objects_to_save,
#      file = outfile,
#      compress = 'gzip'
# )
# 
# # Remove/clean up session:
# all_objects <- ls()
# all_objects
# rm_list <- which(!all_objects %in% objects_to_save)
# all_objects[rm_list]
# rm(list = all_objects[rm_list])
# ls() # Anything defined after all_objects and objects_to_save will still be here
# 
# sessionInfo()

# Saving screen outputs:
sink()

# # q()
# 
# # Next: run the script for xxx
############
