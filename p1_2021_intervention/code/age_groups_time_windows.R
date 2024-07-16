############
# COISS paper 1
# L. Bonifaz
# July 2024
# Descriptive plots and statistics, age groups by intervention tables for T0 (based on T2 groups) and T2
# Input is output from script 5_regression_setup.R as have the subset and selected groups there alread
# Outputs are the tables only, saved to disk, no rdata or other.
############


############
project_loc <- '/Users/antoniob/Documents/work/science/projects/projects/ongoing/laura_bonifaz/COVID_COISS/p1_2021_intervention/results/'
getwd()
setwd(project_loc)
dir()
############


############
# Import libraries
library(data.table)
library(episcout)
library(tidyverse)
library(knitr)
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

objects()
############


############
# Check T0 and T2:
colnames(data_f)
df_time_cuts <- df_time_cuts
time_cuts <- time_cuts

# Time windows:
epi_head_and_tail(df_time_cuts$T0)
epi_head_and_tail(df_time_cuts$T2)

# Intervention group  by window:
summary(df_time_cuts$T0$d_intervention_T2)
summary(df_time_cuts$T2$d_intervention_T2)

# Outcome by window:
summary(factor(df_time_cuts$T0$d_T0_outcome))
summary(factor(df_time_cuts$T2$d_T2_outcome))


# Outcome by window and intervention group:
table(factor(df_time_cuts$T0$d_T0_outcome),
      factor(df_time_cuts$T0$d_intervention_T2)
      )


table(factor(df_time_cuts$T2$d_T2_outcome),
      factor(df_time_cuts$T2$d_intervention_T2)
      )
############


############
###
# Age groups for time windows:
colnames(data_f)

summary(df_time_cuts$T0$EDAD)
summary(df_time_cuts$T2$EDAD)

# Only T2 data needed, so only T2 intervention groups
# T0 are the same states
summary(factor(df_time_cuts$T0$d_intervention_T2))
summary(factor(df_time_cuts$T2$d_intervention_T2))
# data_f_T1 and data_f_T2 have 'others' excluded
# will get excluded here as will subset based on T0 and T2, COISS and non-COISS
###

###
# Get age groups:
df_T0 <- df_time_cuts$T0
df_T2 <- df_time_cuts$T2


# Bin:
df_T0$d_age_groups <- cut(df_T0$EDAD,
                          breaks = c(0, 19, 44, 64, Inf),
                          labels = c('0-19', '20-44', '45-64', '65+'),
                          right = TRUE,
                          include.lowest = TRUE
                          )

dim(df_T0)
summary(df_T0$d_age_groups)
summary(df_T0$EDAD)
length(which(df_T0$EDAD < 20))
length(which(df_T0$EDAD > 19 & df_T0$EDAD < 45))
length(which(is.na(df_T0$EDAD)))
###


###
df_T2$d_age_groups <- cut(df_T2$EDAD,
                          breaks = c(0, 19, 44, 64, Inf),
                          labels = c('0-19', '20-44', '45-64', '65+'),
                          right = TRUE,
                          include.lowest = TRUE
                          )

dim(df_T2)
summary(df_T2$d_age_groups)
###
############


############
# Table by age group and intervention group with percentage:

###
# T0 states as defined by T2 intervention groups:
tab_age_grps_T0 <- as.data.frame(table(df_T0$d_age_groups, df_T0$d_intervention_T2))
# Drop 'other' as not needed:
tab_age_grps_T0 <- tab_age_grps_T0[tab_age_grps_T0$Var2 != 'other', ]
# Column names:
colnames(tab_age_grps_T0) <- c('Age group', 'Intervention group', 'Count')
# Add percentage:
tab_age_grps_T0$Percentage <- round(tab_age_grps_T0$Count / sum(tab_age_grps_T0$Count) * 100, 2)
tab_age_grps_T0

# Save:
file_n <- 'age_intervent_grp_table'
suffix <- 'txt'
df_name <- 'T0_for_T2'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, df_name, suffix)
outfile
epi_write(file_object = tab_age_grps_T0,
          file_name = outfile
          )
###


###
tab_age_grps_T2 <- as.data.frame(table(df_T2$d_age_groups, df_T2$d_intervention_T2))
tab_age_grps_T2 <- tab_age_grps_T2[tab_age_grps_T2$Var2 != 'other', ]
# Column names:
colnames(tab_age_grps_T2) <- c('Age group', 'Intervention group', 'Count')
# Add percentage:
tab_age_grps_T2$Percentage <- round(tab_age_grps_T2$Count / sum(tab_age_grps_T2$Count) * 100, 2)
tab_age_grps_T2

# Save:
file_n <- 'age_intervent_grp_table'
suffix <- 'txt'
df_name <- 'T2'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, df_name, suffix)
outfile
epi_write(file_object = tab_age_grps_T2,
          file_name = outfile
          )
###

# ###
# # Create a pretty table using knitr::kable
# knitr::kable(tab_age_grps_T0,
#       caption = "Distribution by age group and intervention",
#       format = "html",
#       align = c('l', 'l', 'r', 'r'),
#       digits = 2) %>%
#   knitr::kable_styling(full_width = FALSE,
#                        position = "center",
#                        bootstrap_options = c("striped", "hover", "condensed")
#                        )
# ###


###
# Tables by outcome and age group for events only

# T0
# Subset for events only:
summary(factor(df_T0$d_T0_outcome))
df_T0_events <- df_T0[df_T0$d_T0_outcome == 1, ]
df_T0_events

tab_age_grps_T0_events <- as.data.frame(table(df_T0_events$d_age_groups,
                                              df_T0_events$d_intervention_T2)
                                        )
# Drop 'other' as not needed:
tab_age_grps_T0_events <- tab_age_grps_T0_events[tab_age_grps_T0_events$Var2 != 'other', ]
# Column names:
colnames(tab_age_grps_T0_events) <- c('Age group', 'Intervention group', 'Count')
# Add percentage:
tab_age_grps_T0_events$Percentage <- round(tab_age_grps_T0_events$Count / sum(tab_age_grps_T0_events$Count) * 100, 2)
tab_age_grps_T0_events


# Save:
file_n <- 'age_intervent_grp_table_events'
suffix <- 'txt'
df_name <- 'T0_for_T2'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, df_name, suffix)
outfile
epi_write(file_object = tab_age_grps_T0_events,
          file_name = outfile
          )
###


###
# T2:
# Subset for events only:
summary(factor(df_T2$d_T2_outcome))
df_T2_events <- df_T2[df_T2$d_T2_outcome == 1, ]
df_T2_events

tab_age_grps_T2_events <- as.data.frame(table(df_T2_events$d_age_groups,
                                              df_T2_events$d_intervention_T2)
                                        )
tab_age_grps_T2_events <- tab_age_grps_T2_events[tab_age_grps_T2_events$Var2 != 'other', ]
# Column names:
colnames(tab_age_grps_T2_events) <- c('Age group', 'Intervention group', 'Count')
# Add percentage:
tab_age_grps_T2_events$Percentage <- round(tab_age_grps_T2_events$Count / sum(tab_age_grps_T2_events$Count) * 100, 2)
tab_age_grps_T2_events




# Save:
file_n <- 'age_intervent_grp_table_events'
suffix <- 'txt'
df_name <- 'T2'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, df_name, suffix)
outfile
epi_write(file_object = tab_age_grps_T2_events,
          file_name = outfile
          )
###
############


############
# No changes to the rdata file loaded, no need to save again.
# # The end:
q()
############
