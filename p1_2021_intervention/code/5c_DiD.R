############
# COISS paper 1
# L. Bonifaz
# June 2024

# Difference in difference regression model

# Input is output from script 5_regression_setup.R
# Output are various tables from DiD regression analysis for each of the intervention groups (T1 group and T2 group). No rdata or full dataset.
############

############
# Save screen outputs:
# sink("COVID19MEXICO_2021_2022_COVID-only_COISS-only/sink_output_5c_DiD.R.txt")
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
intervention_var <- 'd_intervention_T1'
# intervention_var <- 'd_intervention_T2'

# Set data frame to use:
data_f_T1 <- data_f_T1 # to remove RStudio warnings
data_f_T2 <- data_f_T2 # to remove RStudio warnings

df <- data_f_T1
df_name <- 'data_f_T1'

# df <- data_f_T2
# df_name <- 'data_f_T2'

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
# Data to test

# For both bust must come from either data_f_T2 or data_f_T2:
df_T0 <- df[df$d_time_cuts_prev == 'T0', ]

# For T1:
df_T1 <- df[df$d_time_cuts_prev == 'T1', ]

# For T2:
# df_T2 <- df[df$d_time_cuts_prev == 'T2', ]

df_T0 
df_T1
# df_2

df <- rbind(df_T0, df_T1)
# df <- rbind(df_T0, df_T2)

df[, 'd_time_cuts_prev']
summary(df$d_time_cuts_prev) # only T0 and T1 should have >0

summary(df[[intervention_var]]) # There shouldn't be an 'other' category
############


############
# Set-up covariates
covars <- covars
print(covars)
# from 5_regression_setup.R
############


############
# DiD setup

# Initialize storage for results, already have proportions, tables and chi-sq:
# tables_list <- list()
# props_list <- list()
# chi_tests_list <- list()
did_results_list <- list()

# Keep track of indices:
count <- 0


# TO DO: continue here
# Shouldn't need a loop, will be joined T0 and either T1 or T2, one regression
# Consider using pre-T0 instead of T0, unsure why they picked T0 as baseline
# Run a univariate version with a simple (T1_COISS - T0_COISS) - (T1_non-COISS - T0_non-COISS) model


# Already loaded:
time_cuts <- time_cuts

# DiD with interaction terms:
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
    
    # DiD spec:
    mod_spec <- sprintf("%s ~ %s + %s * %s", outcome_var_time,
                                             paste(covars, collapse = " + "),
                                             intervention_var,
                                             time_var
                        )
    mod_spec
    mod_form <- as.formula(mod_spec)
    print(mod_form)
    
    mod <- glm(mod_form,
                   data = sub_df,
                   family = binomial
                   )
    
    # Store:
    did_summary <- tidy(mod)
    did_summary$time_cut <- i
    # Get odds ratio from the log odds:
    did_summary$odds_ratio <- exp(did_summary$estimate)
    did_results_list[[i]] <- did_summary
    # print(summary(mod))
    
}

# Combine results into a single data frame:
did_results_df <- do.call(rbind, did_results_list)

# View(did_results_df)

# Save:
file_n <- 'did_results_time_points_covars'
suffix <- 'txt'
outcome_var
df_name
outfile <- sprintf(fmt = '%s/%s_%s_%s.%s', infile_prefix, file_n, outcome_var, df_name, suffix)
outfile
epi_write(file_object = did_results_df,
          file_name = outfile
          )
############



############
# No changes to the rdata file loaded, no need to save again.
# # The end:
# # Save objects, to eg .RData file:
# folder <- '../data/processed'
# script <- 'xxx'
# infile_prefix
# suffix <- 'rdata.gzip'
# outfile <- sprintf(fmt = '%s/%s_%s.%s', folder, script, infile_prefix, suffix)
# outfile
# 
# # Check and remove objects that are not necessary to save:
# object_sizes <- sapply(ls(), function(x) object.size(get(x)))
# object_sizes <- as.matrix(rev(sort(object_sizes))[1:10])
# object_sizes
# objects_to_save <- (c('df', 'infile_prefix', 'outfile'))
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
