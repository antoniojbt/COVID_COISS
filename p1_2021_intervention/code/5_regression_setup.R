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
objects_to_save <- (c('data_f', 'data_f_T1', 'data_f_T2', 'infile_prefix', 'outfile'))

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
