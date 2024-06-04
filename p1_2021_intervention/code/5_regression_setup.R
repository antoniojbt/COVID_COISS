############
# COISS paper 1
# L. Bonifaz
# May 2024
# Simple regression set-up as there are different outcome, intervention and time variables.

# Set these up and run script
# For survival regressions in scripts 4, 4x, dynamic variable setting caused problems

# Input is output from script 2_df_subset.R
# Output is an rdta file that can be loaded in subsequent scripts.
############


############
project_loc <- '/Users/antoniob/Documents/work/science/projects/projects/ongoing/laura_bonifaz/COVID_COISS/p1_2021_intervention/results/'
getwd()
setwd(project_loc)
############


############
# Save screen outputs:
# sink("COVID19MEXICO_2021_2022_COVID-only_COISS-only/sink_output_5_regressions.R.txt")
# getwd()
############


############
# Import libraries
library(data.table)
library(episcout)
library(tidyverse)
############


############
# Load a previous R session, data and objects:
infile <- '../data/processed/2_df_subset_COVID19MEXICO_2021_2022_COVID-only_COISS-only.rdata.gzip'
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
# Set-up analyses variables

outcome_var <- 'd_death_30'
time_var <- 'd_days_to_death_30'
intervention_var <- 'd_intervention_T1'

# Analysis windows subsets:
data_f_T1 <- data_f[data_f$d_intervention_T1 == 'COISS' | data_f$d_intervention_T1 == 'non-COISS', ]
data_f_T2 <- data_f[data_f$d_intervention_T2 == 'COISS' | data_f$d_intervention_T2 == 'non-COISS', ]

# Store:
dfs_interv <- list(data_f_T1, data_f_T2)

lapply(dfs_interv, function(x) dim(x))
lapply(dfs_interv, function(x) table(x[[outcome_var]]))
lapply(dfs_interv, function(x) prop.table(table(x[[outcome_var]])))
lapply(dfs_interv, function(x) table(x[[time_var]]))
lapply(dfs_interv, function(x) prop.table(table(x[[time_var]])))

# Drop intervention var that doesn't match df subset to avoid confusion:
data_f_T1$d_intervention_T2 <- NULL
data_f_T2$d_intervention_T1 <- NULL

lapply(dfs_interv, function(x) colnames(x))

# Check intervention vars:
summary(data_f_T1$d_intervention_T1)
summary(data_f_T2$d_intervention_T2)

typeof(data_f_T1$d_intervention_T1)
typeof(data_f_T2$d_intervention_T2)

str(data_f_T1$d_intervention_T1)
str(data_f_T2$d_intervention_T2)

# Drop 'other' from intervention vars as now 0:
# Also copying function here until I update episcout:
epi_clean_drop_zero_levels_vector <- function(factor_var) {
    # Ensure the input is a factor
    if (!is.factor(factor_var)) {
        stop("The input variable is not a factor.")
    }

    # Get the levels that are present in the factor
    present_levels <- levels(factor_var)[table(factor_var) > 0]

    # Drop levels that are zero
    cleaned_factor <- factor(factor_var, levels = present_levels)

    return(cleaned_factor)
}

data_f_T1$d_intervention_T1 <- epi_clean_drop_zero_levels_vector(data_f_T1$d_intervention_T1)
data_f_T2$d_intervention_T2 <- epi_clean_drop_zero_levels_vector(data_f_T2$d_intervention_T2)

summary(data_f_T1$d_intervention_T1)
summary(data_f_T2$d_intervention_T2)
str(data_f_T1$d_intervention_T1)
str(data_f_T2$d_intervention_T2)
###
############


############
###
df <- data_f
# By d_intervention and date:
summary(df$d_time_cuts_prev)

# Subset manually

# Get windows for analysis / time periods, but exclude NAs ('true_NA' is a placeholder):
time_cuts <- levels(df$d_time_cuts_prev)
time_cuts <- time_cuts[time_cuts != 'true_NA']
time_cuts

pre_T0 <- df[df$d_time_cuts_prev == 'pre_T0', ]
T0 <- df[df$d_time_cuts_prev == 'T0', ]
gap_T0_T1 <- df[df$d_time_cuts_prev == 'gap_T0_T1', ]
T1 <- df[df$d_time_cuts_prev == 'T1', ]
gap_T1_T2 <- df[df$d_time_cuts_prev == 'gap_T1_T2', ]
T2 <- df[df$d_time_cuts_prev == 'T2', ]
post_T2 <- df[df$d_time_cuts_prev == 'post_T2', ]

df_time_cuts <- list(pre_T0, T0, gap_T0_T1, T1, gap_T1_T2, T2, post_T2)
names(df_time_cuts) <- time_cuts
names(df_time_cuts)
df_time_cuts

lapply(df_time_cuts, dim)

epi_head_and_tail(pre_T0)
###

###
# Total pop at time point:
summary(df$d_time_cuts_prev)

# Pop admitted at time-point:
summary(df$d_time_cuts_INGRESO)

# Pop suffered event at time-point, factor:
summary(df$d_time_cuts_DEF)

# Pop suffered event at time-point, integer:
deaths_periods_list <- c("d_pre_T0_outcome", "d_T0_outcome",
                         "d_gap_T0_T1_outcome", "d_T1_outcome",
                         "d_gap_T1_T2_outcome", "d_T2_outcome",
                         "d_post_T2_outcome"
                         )
lapply(df[, deaths_periods_list], function(x) summary(as.factor(x)))
# 0's look balanced as coded for all if no event at time-period for individual
dim(df)
###
############


############
# # TO DO:
# # Create sub-samples if needed and save them in rdata file
# # Run sub_sampling script as taking too long to get regressions
# # Test code first with sub-sample:
# perc_needed <- 0.10
# source('/Users/antoniob/Documents/work/science/devel/github/antoniojbt/episcout/R/sub_sampling.R')
# ls()
# data_f_sub <- data_f_sub # to remove RStudio warnings
# 
# sum(data_f_sub[[outcome_var]])
# 
# # Should now have data_f_sub:
# epi_head_and_tail(data_f_sub)
# table(data_f[[outcome_var]])
# table(data_f_sub[[outcome_var]])
# prop.table(table(data_f[[outcome_var]]))
# prop.table(table(data_f_sub[[outcome_var]]))
############


############
# The end:
# Save objects, to eg .RData file:
folder <- '../data/processed'
script <- '5_regression_setup'
infile_prefix
suffix <- 'rdata.gzip'
outfile <- sprintf(fmt = '%s/%s_%s.%s', folder, script, infile_prefix, suffix)
outfile

# Check and remove objects that are not necessary to save:
object_sizes <- sapply(ls(), function(x) object.size(get(x)))
object_sizes <- as.matrix(rev(sort(object_sizes))[1:10])
object_sizes
objects_to_save <- (c('data_f', 'data_f_T1', 'data_f_T2', 'infile_prefix', 'outfile',
                      'time_cuts', 'deaths_periods_list', 'df_time_cuts'
                      ))

# Save:
save(list = objects_to_save,
     file = outfile,
     compress = 'gzip'
     )

# Remove/clean up session:
all_objects <- ls()
all_objects
rm_list <- which(!all_objects %in% objects_to_save)
all_objects[rm_list]
rm(list = all_objects[rm_list])
ls() # Anything defined after all_objects and objects_to_save will still be here

sessionInfo()

# Saving screen outputs:
# sink()

# # q()
#
# # Next: run the script for xxx
############
