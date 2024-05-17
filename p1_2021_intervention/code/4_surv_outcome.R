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
library(survival)
library(survminer)
library(summarytools)
############


############
# Load a previous R session, data and objects:
infile <- '../data/processed/3_desc_plots_COVID19MEXICO2021_COVID-only_COISS-only.rdata.gzip'
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
summary(data_f$days_to_death)
summary(factor(data_f$death))
# check: have fewer NAs, need to check
summary(data_f$intervention)

summary(data_f$time_cuts)
summary(data_f$FECHA_SINTOMAS)
# check as max symptoms here is after FECHA_ACTUALIZACION
summary(data_f$FECHA_ACTUALIZACION)
summary(data_f$FECHA_INGRESO)
summary(data_f$FECHA_DEF)


mean(data_f$days_to_death)
median(data_f$days_to_death)
range(data_f$days_to_death)
sd(data_f$days_to_death)
# These are already saved in stats_sum so just explore


# Frequency table for outcome (death):
table(data_f$death)

# Proportion of events and censored cases
prop.table(table(data_f$death))

# Detailed summary: 
summarytools::descr(data_f$days_to_death)
###
############


############
###
# Plot K-M
# Create a Surv object
summary(data_f$intervention)
str(data_f$intervention)

surv_object <- survival::Surv(time = data_f$days_to_death,
                              event = data_f$death
                              )
str(surv_object)


# Fit a Kaplan-Meier survival curve
surv_fit <- survival::survfit(surv_object ~ 1,
                              data = data_f
                              )
str(surv_fit)
surv_fit

# Plot the Kaplan-Meier survival curve
km_1 <- survminer::ggsurvplot(surv_fit,
                   data = data_f,
                   conf.int = TRUE,
                   xlab = "Time (days)",
                   ylab = "Survival Probability",
                   title = "Kaplan-Meier Survival Curve for COVID-19 Deaths",
                   linetype = "strata",
                   surv.median.line = "hv"
                   )

i <- 'days_to_death'
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
# The end:
# Save objects, to eg .RData file:
folder <- '../data/processed'
script <- '4_surv_outcome'
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
