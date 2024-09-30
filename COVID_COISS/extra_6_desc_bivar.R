############
# COISS paper 1
# L. Bonifaz
# May 2024
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
infile <- '../data/processed/4a_surv_outcome_bivariate_COVID19MEXICO2021_COVID-only_COISS-only.rdata.gzip'
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
#
# # Descriptive statistics # by group (intervention)
# # Split the data:
# split_data <- split(data_f, data_f$intervention)
# typeof(split_data)
# 
# 
# # TO DO: continue here
# 
# descriptive_stats <- lapply(split_data, summary)
# str(descriptive_stats)
# str(descriptive_stats$COISS)
# descriptive_stats$COISS
# descriptive_stats$`non-COISS`
# 
# 
# # Desc stats for num vars:
# 
# 
# 
# 
# 
# 
# # Desc stats for fact vars:
# 
# 
# 
# 
# 
# mean_days_to_death <- mean(data_f$days_to_death)
# median_days_to_death <- median(data_f$days_to_death)
# events <- sum(as.factor(data_f$death) == 1)
# censored <- sum(as.factor(data_f$death) == 0)
# prop_events <- mean(data_f$death == 1)
# prop_censored <- mean(data_f$death == 0)
# 
# outcome <- factor(data_f$death, levels = c(0, 1), labels = c("censored", "non-survival"))
# table(data_f$intervention, outcome)
# round(prop.table(table(data_f$intervention, outcome)), digits = 2)
# 


############


############
# The end:
# Save objects, to eg .RData file:
folder <- '../data/processed'
script <- '5_bivariate'
infile_prefix
suffix <- 'rdata.gzip'
outfile <- sprintf(fmt = '%s/%s_%s.%s', folder, script, infile_prefix, suffix)
outfile

# Check and remove objects that are not necessary to save:
object_sizes <- sapply(ls(), function(x) object.size(get(x)))
object_sizes <- as.matrix(rev(sort(object_sizes))[1:10])
object_sizes
objects_to_save <- (c('data_f', 'infile_prefix', 'outfile'))

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
# q()

# Next: run the script for xxx
############
