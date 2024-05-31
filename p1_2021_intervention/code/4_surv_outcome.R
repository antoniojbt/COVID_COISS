############
# COISS paper 1
# L. Bonifaz
# May 2024

# Survival outcome analysis: KM plots and tables
# Input is output from script 2_df_subset.R
# Output are various descriptive plots and tables, no rdata or full dataset.
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
library(survival)
library(survminer)
library(summarytools)
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
# Simple survival analysis setup
# Univariate analysis

###
# Descriptive stats survival:
dim(data_f)
str(data_f)
summary(data_f$d_days_to_death)
summary(factor(data_f$d_death))
# check: have fewer NAs, need to check
summary(data_f$d_intervention)

summary(data_f$FECHA_SINTOMAS)
# check as max symptoms here is after FECHA_ACTUALIZACION
summary(data_f$FECHA_ACTUALIZACION)
summary(data_f$FECHA_INGRESO)
summary(data_f$FECHA_DEF)


mean(data_f$d_days_to_death)
median(data_f$d_days_to_death)
range(data_f$d_days_to_death)
sd(data_f$d_days_to_death)
# These are already saved in stats_sum so just explore

# Frequency table for outcome (d_death):
table(data_f$d_death)

# Proportion of events and censored cases
prop.table(table(data_f$d_death))

# Detailed summary: 
summarytools::descr(data_f$d_days_to_death)
###
############


############
###
# Plot K-M
# Create a Surv object:
summary(data_f$d_intervention)
str(data_f$d_intervention)

surv_object <- survival::Surv(time = data_f$d_days_to_death,
                              event = data_f$d_death
                              )
str(surv_object)


# Fit a Kaplan-Meier survival curve
surv_fit <- survival::survfit(surv_object ~ 1,
                              data = data_f
                              )
str(surv_fit)
surv_fit

# Plot KM survival curve:
km_1 <- survminer::ggsurvplot(surv_fit,
                   data = data_f,
                   conf.int = TRUE,
                   xlab = "Time (days)",
                   ylab = "Survival Probability",
                   title = "Kaplan-Meier Survival Curve for COVID-19 Deaths",
                   linetype = "strata",
                   surv.median.line = "hv"
                   )

i <- 'd_days_to_death'
file_n <- 'plots_KM'
suffix <- 'pdf'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
ggplot2::ggsave(filename = outfile, plot = km_1$plot)

# Save the tables from survminer as well:
file_n <- 'survival_table'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
epi_write(file_object = km_1$data.survtable,
          file_name = outfile
          )

file_n <- 'data_survival_plot'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
epi_write(file_object = km_1$data.survplot,
          file_name = outfile
          )
###


###
# KM with zoom to x days and y axis:
trunc_x <- c(0, 30)
trunc_y <- c(0.95, 1.00)

km_zoom <- ggsurvplot(surv_fit,
           data = data_f,
           conf.int = TRUE,
           xlab = "Time (days)",
           ylab = "Survival Probability",
           title = "Kaplan-Meier Survival Curve (Zoomed In)",
           xlim = trunc_x, # zoom in
           ylim = trunc_y
           # risk.table = TRUE
           )

# This is for all individuals so not particularly interesting

i <- 'd_days_to_death_zoom'
file_n <- 'plots_KM'
suffix <- 'pdf'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
ggplot2::ggsave(filename = outfile, plot = km_zoom$plot)

# Save the tables from survminer as well:
file_n <- 'survival_table_zoom'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
epi_write(file_object = km_zoom$data.survtable,
          file_name = outfile
          )

file_n <- 'data_survival_plot_zoom'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
epi_write(file_object = km_zoom$data.survplot,
          file_name = outfile
          )

###


###
# Truncate follow up time so that plots can be visualised better
# Means re-running the fit
# Effect? Plots are more readable, but we lose some data; survival probably now goes from 0 to 1, eg looks dramatic, but overall death proportion is ~3%
# Events are not re-censored though, hence the drop in survival
# Re-censoring below, keep these for comparison

max_followup_time <- 30
# max_followup_time <- 60

# Truncate data:
data_f_truncated <- data_f %>%
  filter(d_days_to_death <= max_followup_time)

# Re-run fit Kaplan-Meier model:
surv_object <- survival::Surv(time = data_f_truncated$d_days_to_death,
                              event = data_f_truncated$d_death
                              )
str(surv_object)


# Fit a Kaplan-Meier survival curve
surv_fit_truncated <- survival::survfit(surv_object ~ 1,
                                        data = data_f_truncated
                                       )
str(surv_fit_truncated)
surv_fit_truncated

# Plot the Kaplan-Meier survival curve
km_x_days <- ggsurvplot(surv_fit_truncated,
           data = data_f_truncated,
           conf.int = TRUE,
           xlab = "Time (days)",
           ylab = "Survival Probability",
           title = "Kaplan-Meier Survival Curve (Truncated Follow-Up)",
           risk.table = TRUE)

i <- sprintf('d_days_to_death_%s', max_followup_time)
file_n <- 'plots_KM'
suffix <- 'pdf'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
ggplot2::ggsave(filename = outfile, plot = km_x_days$plot)

# Save the tables from survminer as well:
file_n <- 'survival_table_trunc'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
epi_write(file_object = km_x_days$data.survtable,
          file_name = outfile
          )

file_n <- 'data_survival_plot_trunc'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
epi_write(file_object = km_x_days$data.survplot,
          file_name = outfile
          )
###
############


############
###
# Fit Kaplan-Meier model with censoring
surv_obj <- Surv(time = data_f$d_days_to_death_30,
                 event = data_f$d_death_30
                 )

surv_fit_censored <- survfit(surv_obj ~ 1,
                             data = data_f
                             )
###


###
# Plot the Kaplan-Meier survival curve with censoring

# Define new censoring time
censoring_time <- 30 # for new cut-off for follow-up time, e.g. 30 days

ylim <- c(0.95, 1.00)

km_censor <- ggsurvplot(surv_fit_censored,
           data = data_f,
           conf.int = TRUE,
           xlab = "Time (days)",
           ylab = "Survival Probability",
           title = "Kaplan-Meier Survival Curve (Censored Follow-Up)",
           ylim = ylim,
           risk.table = TRUE)

i <- sprintf('d_days_to_death_censor_%s', censoring_time)
file_n <- 'plots_KM'
suffix <- 'pdf'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
ggplot2::ggsave(filename = outfile, plot = km_1$plot)

# Save the tables from survminer as well:
file_n <- 'survival_table'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
epi_write(file_object = km_1$data.survtable,
          file_name = outfile
          )

file_n <- 'data_survival_plot'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
epi_write(file_object = km_1$data.survplot,
          file_name = outfile
          )
###
############


############
# No changes to the rdata file loaded, no need to save again.
# # The end:
# # Save objects, to eg .RData file:
# folder <- '../data/processed'
# script <- '4_surv_outcome'
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
#      )
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
# # q()
# 
# # Next: run the script for xxx
############
