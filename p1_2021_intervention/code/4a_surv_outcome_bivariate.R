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
infile <- '../data/processed/4_surv_outcome_COVID19MEXICO2021_COVID-only_COISS-only.rdata.gzip'
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
# Survival analysis setup for group comparisons
# Bivariate analysis

###
outcome <- factor(data_f$death, levels = c(0, 1), labels = c("censored", "non-survival"))

counts <- table(data_f$intervention, outcome)
i <- ''
file_n <- 'survival_counts'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
epi_write(file_object = counts,
          file_name = outfile
          )


props <- round(prop.table(table(data_f$intervention, outcome)), digits = 2)
i <- ''
file_n <- 'survival_proportions'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
epi_write(file_object = props,
          file_name = outfile
          )

###
############



############
# Plot survival by group

###
# Histogram of survival times
ggplot(data_f, aes(x = days_to_death)) +
    geom_histogram(binwidth = 5, fill = "blue", alpha = 0.7) +
    labs(title = "Histogram of Survival Times", x = "Time", y = "Frequency")
###


###
# Kaplan-Meier plot
km_fit <- survfit(Surv(days_to_death, death) ~ intervention, data = data_f)

km_grp <- ggsurvplot(km_fit, data = data_f, pval = TRUE, conf.int = TRUE, 
           ggtheme = theme_minimal(), 
           title = "Kaplan-Meier Survival Curves")

km_grp$plot
km_grp$data.survtable
km_grp$data.survplot

i <- 'days_to_death'
file_n <- 'plots_KM_group'
suffix <- 'pdf'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
ggplot2::ggsave(filename = outfile, plot = km_grp$plot)

# Save the tables from survminer as well:
file_n <- 'survival_table'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
epi_write(file_object = km_grp$data.survtable,
          file_name = outfile
          )

file_n <- 'data_survival_plot'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
epi_write(file_object = km_grp$data.survplot,
          file_name = outfile
          )
###


###
# Boxplot of survival times by group
box_surv <- epi_plot_box(data_f,
                         var_y = 'days_to_death',
                         var_x = 'intervention',
                         fill = 'intervention'
                         ) +
    labs(title = "Boxplot of Survival Times by Group",
         x = "Group",
         y = "Time") + # remove legend
    theme(legend.position = "none")
# TO DO: remove diamond legends in plot
# box_surv

i <- 'days_to_death'
file_n <- 'plots_surv_box'
suffix <- 'pdf'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
ggplot2::ggsave(filename = outfile, plot = box_surv)
###


###
# Cumulative hazard plot
cum_haz <- ggsurvplot(km_fit, data = data_f, fun = "cumhaz", 
           ggtheme = theme_minimal(), 
           title = "Cumulative Hazard Plot")

cum_haz$plot
cum_haz$data.survtable
cum_haz$data.survplot

i <- 'days_to_death'
file_n <- 'plots_cum_haz_group'
suffix <- 'pdf'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
ggplot2::ggsave(filename = outfile, plot = cum_haz$plot)

# Save the tables from survminer as well:
file_n <- 'survival_table_cum_haz'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
epi_write(file_object = cum_haz$data.survtable,
          file_name = outfile
          )

file_n <- 'data_survival_plot_cum_haz'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
epi_write(file_object = cum_haz$data.survplot,
          file_name = outfile
          )
###
############


############
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
summary(data_f$intervention)
# Should be fine
# data_f$intervention <- droplevels(data_f$intervention)
# levels(data_f$intervention)
# summary(data_f$intervention)
###


###
# Desc stats by time cut-offs:
counts_by_dates <- table(data_f$intervention, data_f$time_cuts)
counts_by_dates
i <- ''
file_n <- 'counts_by_dates'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
epi_write(file_object = counts_by_dates,
          file_name = outfile
          )

props_by_dates <- round(prop.table(counts_by_dates), digits = 2)
props_by_dates
i <- ''
file_n <- 'proportions_by_dates'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
epi_write(file_object = props_by_dates,
          file_name = outfile
          )
###


###
# Desc stats by time cut-offs, outcome and intervention:
counts_by_dates_outcome <- table(data_f$intervention, data_f$time_cuts, outcome)
str(counts_by_dates_outcome)

i <- ''
file_n <- 'counts_by_dates_outcome'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
epi_write(file_object = as.data.frame(counts_by_dates_outcome),
          file_name = outfile
          )

props_by_dates_outcome <- round(prop.table(counts_by_dates_outcome), digits = 2)
str(props_by_dates_outcome)

i <- ''
file_n <- 'props_by_dates_outcome'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
epi_write(file_object = as.data.frame(props_by_dates_outcome),
          file_name = outfile
          )
###


###
# There shouldn't be NAs, result should be FALSE:
any(is.na(data_f$time_cuts)) # is at least one value TRUE?
any(is.na(data_f$intervention))


# Survival object by groups (intervention) and dates (time_cuts):
surv_fit_point <- survival::survfit(Surv(days_to_death, death) ~ intervention + time_cuts,
                                    data = data_f
                                    )
str(surv_fit_point)
surv_fit_point$strata
summary(surv_fit_point)

km_grps_dates <- ggsurvplot(surv_fit_point,
                            data = data_f,
                            xlab = "Time (days)",
                            ylab = "Survival Probability",
                            title = "Kaplan-Meier Survival Curves by Intervention and Study Points",
                            # facet.by = 'intervention', #"time_cuts",
                            risk.table = TRUE,  # Add risk table
                            pval = TRUE  # Add p-value
                            )
# Still errors with faceting

i <- 'days_to_death'
file_n <- 'plots_km_group_dates'
suffix <- 'pdf'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
ggplot2::ggsave(filename = outfile,
                plot = km_grps_dates$plot,
                units = 'in',
                height = 15,
                width = 15
                )

# Save the tables from survminer as well:
file_n <- 'survival_table_km_group_dates'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
epi_write(file_object = km_grps_dates$data.survtable,
          file_name = outfile
          )

file_n <- 'data_survival_plot_km_group_dates'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
epi_write(file_object = km_grps_dates$data.survplot,
          file_name = outfile
          )
###

###
# Plot instead separately for each time cut-off:
date_cuts <- levels(data_f$time_cuts)
km_dates_by_levels <- epi_plot_list(vars_to_plot = date_cuts)

for (i in date_cuts) {
    # print(i)
    surv_fit_level <- survfit(Surv(days_to_death, death) ~ intervention,
                              data = subset(data_f, time_cuts == i)
                              )
    
    plot_1 <- ggsurvplot(surv_fit_level,
                         data = subset(data_f, time_cuts == i),
                         xlab = "Time (days)",
                         ylab = "Survival Probability",
                         title = paste("KM Survival Curves -", i),
                         risk.table = TRUE,
                         pval = TRUE
                         )
    # print(plot_1$plot)
    km_dates_by_levels[[i]] <- plot_1$plot
    }

# Save plots
# Plot all together:
    file_n <- 'plots_km_group_dates_facet'
    suffix <- 'pdf'
    outfile <- sprintf(fmt = '%s/%s.%s', infile_prefix, file_n, suffix)
    outfile
    my_plot_grid <- epi_plots_to_grid(km_dates_by_levels)
    epi_plot_cow_save(file_name = outfile,
                      plot_grid = my_plot_grid,
                      base_width = 15,
                      base_height = 15
                      )

# Could get risk table and data behind each plot but already elsewhere
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
