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
infile <- '../data/processed/'
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


# TO DO: continue here
############
# Survival analysis setup for group comparisons
# Bivariate analysis


###
# by group (intervention)

# Descriptive statistics by group
data_f %>%
    group_by(intervention) %>%
    summarise(
        mean_days_to_death = mean(days_to_death),
        median_days_to_death = median(days_to_death),
        events = sum(as.factor(death) == 1),
        censored = sum(as.factor(death) == 0),
        prop_events = mean(death == 1),
        prop_censored = mean(death == 0)
    )

table(data_f$intervention, data_f$death)
###
############


############
# Histogram of survival times
ggplot(data_f, aes(x = days_to_death)) +
    geom_histogram(binwidth = 5, fill = "blue", alpha = 0.7) +
    labs(title = "Histogram of Survival Times", x = "Time", y = "Frequency")

# Kaplan-Meier plot
km_fit <- survfit(Surv(days_to_death, death) ~ intervention, data = data_f)
ggsurvplot(km_fit, data = data_f, pval = TRUE, conf.int = TRUE, 
           ggtheme = theme_minimal(), 
           title = "Kaplan-Meier Survival Curves")

# Boxplot of survival times by group
ggplot(data_f, aes(x = intervention, y = days_to_death, fill = intervention)) +
    geom_boxplot() +
    labs(title = "Boxplot of Survival Times by Group", x = "Group", y = "Time")

# Cumulative hazard plot
ggsurvplot(km_fit, data = data_f, fun = "cumhaz", 
           ggtheme = theme_minimal(), 
           title = "Cumulative Hazard Plot")

# Stratified summaries
data_f %>%
    group_by(intervention) %>%
    summarise(
        mean_time = mean(days_to_death),
        median_time = median(days_to_death),
        events = sum(death == 1),
        censored = sum(death == 0),
        prop_events = mean(death == 1),
        prop_censored = mean(death == 0)
    )
############


############
###
# Plot COISS vs non-COISS this is over the whole study period
# COISS states were worse off
summary(data_f$intervention)
# group <- data_f$intervention
surv_fit_group <- survival::survfit(surv_object ~ intervention,
                                    data = data_f
)
ggsurvplot(surv_fit_group,
           data = data_f
)
###


###
# Use only intervention periods:
summary(data_f$time_cuts)
# intervention_points <- data_f$time_cuts
# data_f$time_cuts <- factor(data_f$time_cuts)
str(data_f$time_cuts)
levels(data_f$time_cuts)

# Levels with values of 0 will error:
data_f$time_cuts <- droplevels(data_f$time_cuts)
levels(data_f$time_cuts)
summary(data_f$time_cuts)

# Also:
data_f$intervention <- droplevels(data_f$intervention)
levels(data_f$intervention)
summary(data_f$intervention)

table(data_f$intervention, data_f$time_cuts)

# There shouldn't be NAs, result should be FALSE:
any(is.na(data_f$time_cuts)) # is at least one value TRUE?
any(is.na(data_f$intervention))


# Re-run the surv object:
surv_fit_point <- survival::survfit(Surv(days_to_death, death) ~ intervention + time_cuts,
                                    data = data_f
)
str(surv_fit_point)
surv_fit_point$strata
summary(surv_fit_point)

ggsurvplot(surv_fit_point,
           data = data_f,
           xlab = "Time (days)",
           ylab = "Survival Probability",
           title = "Kaplan-Meier Survival Curves by Hospital and Study Point",
           # facet.by = 'intervention', #"time_cuts",
           risk.table = TRUE,  # Add risk table
           pval = TRUE
)  # Add p-value
# Still errors with faceting
###

###
# TO DO: save each plot:
# Plot instead separately for each time cut-off:
for (level in levels(data_f$time_cuts)) {
    surv_fit_level <- survfit(Surv(days_to_death, death) ~ intervention,
                              data = subset(data_f, time_cuts == level)
    )
    print(ggsurvplot(
        surv_fit_level,
        data = subset(data_f, time_cuts == level),
        xlab = "Time (days)",
        ylab = "Survival Probability",
        title = paste("Kaplan-Meier Survival Curves -", level),
        risk.table = TRUE,
        pval = TRUE
    ))
}
###


###
# Try with ggplot2:
# Function to convert survfit object to data frame:
# TO DO: double check this function and summary(surv_fit_point) give the same output
surv_summary <- function(survfit_obj, time_var) {
    data.frame(
        time = survfit_obj$time,
        n.risk = survfit_obj$n.risk,
        n.event = survfit_obj$n.event,
        n.censor = survfit_obj$n.censor,
        surv = survfit_obj$surv,
        strata = rep(names(survfit_obj$strata), times = survfit_obj$strata)
    )
}


# Convert survfit object to data frame:
# Re-run the surv object and fit:
surv_fit_point <- survival::survfit(Surv(days_to_death, death) ~ intervention + time_cuts,
                                    data = data_f
)
str(surv_fit_point)
surv_fit_point$n
surv_fit_point$time


surv_data <- surv_summary(surv_fit_point, "time")
epi_head_and_tail(surv_data, cols = 6)
summary(surv_data)

# Extract hospital and time_cuts from strata:
surv_data <- surv_data %>%
    mutate(
        intervention = sapply(strsplit(as.character(strata), ","), function(x) trimws(sub("intervention=", "", x[1]))),
        time_cuts = sapply(strsplit(as.character(strata), ","), function(x) trimws(sub("time_cuts=", "", x[2])))
    )
epi_head_and_tail(surv_data, cols = 8)
surv_data$time_cuts
surv_data$intervention
# Trimmed of whitespaces now

# Set order for time-cuts:
surv_data$time_cuts <- factor(surv_data$time_cuts,
                              levels = c('pre-T0', 'T0', 'gap_T0_T1', 'T1',
                                         'gap_T1_T2', 'T2', 'post-T2'),
                              ordered = TRUE)
summary(surv_data$time_cuts)

# Plot with ggplot2 and faceting:
# TO DO: save plot
ggplot(surv_data, aes(x = time, y = surv, color = intervention)) +
    geom_step() +
    facet_wrap(~time_cuts, scales = "free_y") +
    labs(
        title = "Kaplan-Meier Survival Curves by Intervention and Study Point",
        x = "Time (days)",
        y = "Survival Probability"
    ) +
    theme_minimal() +
    theme(legend.title = element_blank())

surv_data
###
############


############
# TO DO: create and save table
# Extract risk table data:
surv_summary <- summary(surv_fit_point)
head(surv_summary)
str(surv_summary)

# Extract risk table data
risk_table <- data.frame(
    strata = surv_summary$strata,
    time = surv_summary$time,
    n.risk = surv_summary$n.risk,
    n.event = surv_summary$n.event
)
epi_head_and_tail(risk_table, cols = 4)
epi_head_and_tail(surv_data, cols = 8)

# Extract hospital and time_cuts from strata
risk_table <- risk_table %>%
    mutate(
        hospital = sapply(strsplit(as.character(strata), ","), function(x) trimws(sub("intervention=", "", x[1]))),
        time_cuts = sapply(strsplit(as.character(strata), ","), function(x) trimws(sub("time_cuts=", "", x[2])))
    ) %>%
    select(-strata)

# Summarize risk table data by time_cuts
risk_summary <- risk_table %>%
    group_by(time_cuts) %>%
    summarize(
        total_risk = sum(n.risk, na.rm = TRUE),
        total_events = sum(n.event, na.rm = TRUE)
    )

# Print the risk summary table
knitr::kable(risk_summary, caption = "Risk Table by Time Cuts and Events (Death)")
############


############
# The end:
# Save objects, to eg .RData file:
folder <- '../data/processed'
script <- '4a_surv_outcome_bivariate'
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
