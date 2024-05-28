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
library(ggrepel)
############


############
# Load a previous R session, data and objects:
# infile <- '../data/processed/4_surv_outcome_COVID19MEXICO2021_COVID-only_COISS-only.rdata.gzip'
infile <- '../data/processed/4_surv_outcome_COVID19MEXICO2021_2022_COVID-only_COISS-only.rdata.gzip'
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
# Describe based on censoring definition, e.g. max follow-up time, 30-day in-hospital, etc.
outcome_var <- 'd_death_30'
time_var <- 'd_days_to_death_30'

# Original follow-up to max period in database (FECHA_INGRESO):
# outcome <- factor(data_f[[outcome_var]], levels = c(0, 1), labels = c("censored", "non-survival"))

# 30 day mortality follow-up:
outcome <- factor(data_f[[outcome_var]], levels = c(0, 1), labels = c("censored", "non-survival"))

# Naming var for saving to disk:
i <- outcome_var

counts <- table(data_f$d_intervention, outcome)
file_n <- 'survival_counts'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
epi_write(file_object = counts,
          file_name = outfile
          )


props <- round(prop.table(table(data_f$d_intervention, outcome)), digits = 2)
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
# Naming var for saving to disk:
i <- time_var

plot_hist <- ggplot(data_f, aes(x = !!sym(time_var))) +
    geom_histogram(binwidth = 5, fill = "blue", alpha = 0.7) +
    labs(title = "Histogram of Survival Times", x = "Time", y = "Frequency")


file_n <- 'plots_hist_surv_times'
suffix <- 'pdf'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
ggplot2::ggsave(filename = outfile, plot = plot_hist)
###


###
# Count events by days to death:
# Naming var for saving to disk:
i <- time_var

events_by_time <- as.data.frame(table(data_f[[time_var]]))
colnames(events_by_time) <- c("Time", "Freq")
# Add proportions column:
events_by_time$prop <- round(events_by_time$Freq / sum(events_by_time$Freq), digits = 4)
# Add cumulative proportions:
events_by_time$cum_prop <- cumsum(events_by_time$prop)
epi_head_and_tail(events_by_time, cols = 4)
events_by_time
# Virtually all deaths occur within 30 days (~3%)

file_n <- 'events_by_time'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
epi_write(file_object = events_by_time,
          file_name = outfile
          )
###


###
# Kaplan-Meier plot
# time_var <- 'd_days_to_death_30'
# outcome_var <- 'd_death_30'

summary(data_f[[time_var]])
summary(factor(data_f[[outcome_var]]))
table(data_f[[time_var]], data_f[[outcome_var]])
str(data_f)

km_fit <- survfit(Surv(data_f[[time_var]], data_f[[outcome_var]]) ~ d_intervention,
                  data = data_f
                  )

ylim <- c(0.90, 1.00)

km_grp <- ggsurvplot(km_fit, data = data_f, pval = TRUE, conf.int = TRUE, 
           ggtheme = theme_minimal(), 
           title = "Kaplan-Meier Survival Curves",
           ylim = ylim)

km_grp$plot
km_grp$data.survtable
km_grp$data.survplot

i <- time_var
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
                         var_y = time_var,
                         var_x = 'd_intervention',
                         fill = 'd_intervention',
                         ) +
    labs(title = "Boxplot of Survival Times by Group",
         x = "Group",
         y = "Time") + # remove legend
    theme(legend.position = "none")  +
    guides(fill = "none") # remove within plot symbols
box_surv

i <- time_var
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

i <- time_var
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
# Use only d_intervention periods:
summary(data_f$d_time_cuts)
# intervention_points <- data_f$d_time_cuts
# data_f$d_time_cuts <- factor(data_f$d_time_cuts)
str(data_f$d_time_cuts)
levels(data_f$d_time_cuts)

# Levels with values of 0 will error:
# Had this issue with 2021 database but once joined should be fine
# data_f$d_time_cuts <- droplevels(data_f$d_time_cuts)
# levels(data_f$d_time_cuts)
# summary(data_f$d_time_cuts)

# Also:
summary(data_f$d_intervention)
# Should be fine
# data_f$d_intervention <- droplevels(data_f$d_intervention)
# levels(data_f$d_intervention)
# summary(data_f$d_intervention)
###


###
# Desc stats by time cut-offs:
counts_by_dates <- table(data_f$d_intervention, data_f$d_time_cuts)
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
# Desc stats by time cut-offs, outcome and d_intervention:
counts_by_dates_outcome <- table(data_f$d_intervention, data_f$d_time_cuts, data_f[[outcome_var]])
str(counts_by_dates_outcome)

i <- outcome_var
file_n <- 'counts_by_dates_outcome'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
epi_write(file_object = as.data.frame(counts_by_dates_outcome),
          file_name = outfile
          )

props_by_dates_outcome <- round(prop.table(counts_by_dates_outcome), digits = 4)
str(props_by_dates_outcome)
props_by_dates_outcome

i <- outcome_var
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
any(is.na(data_f$d_time_cuts)) # is at least one value TRUE?
any(is.na(data_f$d_intervention))


# Survival object by groups (d_intervention) and dates (d_time_cuts):
surv_obj <- Surv(data_f[[time_var]], data_f[[outcome_var]])
surv_fit_point <- survival::survfit(surv_obj ~ d_intervention + d_time_cuts, data = data_f)

str(surv_fit_point)
surv_fit_point$strata
summary(surv_fit_point)

ylim <- c(0.90, 1.00)
km_grps_dates <- ggsurvplot(surv_fit_point,
                            data = data_f,
                            xlab = "Time (days)",
                            ylab = "Survival Probability",
                            title = "Kaplan-Meier Survival Curves by Intervention and Study Points",
                            # facet.by = 'd_intervention', #"d_time_cuts",
                            risk.table = TRUE,  # Add risk table
                            pval = TRUE,  # Add p-value
                            ylim = ylim
                            )
# Still errors with faceting so print separately

i <- time_var
file_n <- 'plots_km_group_dates'
suffix <- 'pdf'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
ggplot2::ggsave(filename = outfile,
                plot = km_grps_dates$plot,
                units = 'in',
                height = 25,
                width = 25
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
# # Print labels next to lines
# # Extract the ggplot object from the ggsurvplot object:
# km_grps_dates_gg <- km_grps_dates$plot
# 
# # Extract the plot data and the labels ('strata') info:
# str(km_grps_dates_gg)
# km_grps_dates_gg$data$surv
# strata_info <- km_grps_dates_gg$data$strata
# strata_info <- as.factor(strata_info)
# data_info <- km_grps_dates_gg$data
# epi_head_and_tail(data_info)
# colnames(data_info)
# 
# # Add labels to the lines using geom_label_repel
# km_grps_dates_labels <- km_grps_dates_gg +
#   geom_label_repel(data = data_info,
#                    aes(x = n.event, y = surv, label = strata_info, color = strata_info),
#                    nudge_x = 1,
#                    nudge_y = 0.02,
#                    segment.color = 'grey50',
#                    direction = "y",
#                    show.legend = FALSE
#                    # check_overlap = TRUE
#                    ) +
#     theme(legend.position = "none")
# 
# # Print the updated plot
# print(km_grps_dates_labels)
# 
# # Works but hard to get the labels in the right places
###

###
# Plot instead separately for each time cut-off
# Plotting below misbehaving because Surv() and survfit have issues with string variables and variable passing, plotting the same every time if within loop with dynamic variables
# Plots were the same, risk tables had the same numbers
# Resorted to subsetting before and passing vars directly to survfit

# Re-coded:

# Plot with zoom on y axis:
ylim <- c(0.90, 1.00)

# Subset data:
pre_T0 <- data_f[data_f$d_time_cuts == 'pre-T0', ]
T0 <- data_f[data_f$d_time_cuts == 'T0', ]
gap_T1_T0 <- data_f[data_f$d_time_cuts == 'gap_T0_T1', ]
T1 <- data_f[data_f$d_time_cuts == 'T1', ]
gap_T1_T2 <- data_f[data_f$d_time_cuts == 'gap_T1_T2', ]
T2 <- data_f[data_f$d_time_cuts == 'T2', ]
post_T2 <- data_f[data_f$d_time_cuts == 'post-T2', ]

subsets_by_cut <- list(pre_T0, T0, gap_T1_T0, T1, gap_T1_T2, T2, post_T2)
titles <- levels(data_f$d_time_cuts)
lapply(subsets_by_cut, dim)

# Set-up lists to hold loop info:
plots_km_dates_by_levels <- epi_plot_list(vars_to_plot = titles)
data_plot_km_dates_by_levels <- list() # plot_1$data.survplot
surv_table_km_dates_by_levels <- list() # plot_1$data.survtable

# Outcome and time vars are hard-coded as couldn't get them to work in the loop if passed as variables
# outcome_var <- 'd_death'
# time_var <- 'd_days_to_death'

counter <- 0
for (df in subsets_by_cut) {
    counter <- counter + 1
    plot_title <- titles[counter]
    # Each df is a data subset, other methods didn't work properly
    # Fit model:
    surv_fit_level <- survfit(Surv(time = d_days_to_death, #d_days_to_death_30,
                                   event = d_death, #d_death_30
                                   ) ~ d_intervention,
                              data = df
                              )

    # Check underlying data:
    print(nrow(df))
    # Check model specs:
    print(summary(surv_fit_level,
                  times = c(1, median(df[["d_days_to_death"]], #df[["d_days_to_death_30"]],
                                      na.rm = TRUE)
                            )
                  )
          )  # Print summary at specific times

    plot_1 <- ggsurvplot(surv_fit_level,
                         data = df,
                         xlab = "Time (days)",
                         ylab = "Survival Probability",
                         title = paste("KM Survival Curves -", plot_title),
                         risk.table = TRUE,
                         pval = TRUE,
                         ylim = ylim
                         )
    # print(plot_1$plot)
    plots_km_dates_by_levels[[plot_title]] <- plot_1$plot
    surv_table_km_dates_by_levels[[plot_title]] <- plot_1$data.survtable
    data_plot_km_dates_by_levels[[plot_title]] <- plot_1$data.survplot
    
    }

# Plots:
plot_info <- plots_km_dates_by_levels[[plot_title]]
plot_info
# Survival tables:
surv_table_info <- surv_table_km_dates_by_levels[[plot_title]]
surv_table_info$n.event
# Data behind plots:
data_plot_info <- data_plot_km_dates_by_levels[[plot_title]]
data_plot_info$n.event


# Save plots
z <- '' # zoom or nothing
file_n <- sprintf('plots_km_dates_by_levels_facet_%s', z)
suffix <- 'pdf'
outfile <- sprintf(fmt = '%s/%s_%s_%s.%s', infile_prefix, file_n, outcome_var, time_var, suffix)
outfile
my_plot_grid <- epi_plots_to_grid(plots_km_dates_by_levels)
epi_plot_cow_save(file_name = outfile,
                  plot_grid = my_plot_grid,
                  base_width = 25,
                  base_height = 25
                  )

# Save the tables from survminer as well, needs looping:
for (i in titles) {
    file_n <- 'surv_table_km_dates_by_levels'
    suffix <- 'txt'
    outfile <- sprintf(fmt = '%s/%s_%s_%s_%s.%s', infile_prefix, file_n, outcome_var, time_var, i, suffix)
    outfile
    epi_write(file_object = surv_table_km_dates_by_levels[[i]],
              file_name = outfile
              )
    
    file_n <- 'data_plot_km_dates_by_levels'
    suffix <- 'txt'
    outfile <- sprintf(fmt = '%s/%s_%s_%s_%s.%s', infile_prefix, file_n, outcome_var, time_var, i, suffix)
    outfile
    epi_write(file_object = data_plot_km_dates_by_levels[[i]],
              file_name = outfile
              )
    }

###
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
