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
library(ggrepel)
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
# Describe based on censoring definition, e.g. max follow-up time, 30-day in-hospital, etc.
outcome_var <- 'd_death_30'
time_var <- 'd_days_to_death_30'

# outcome_var <- 'd_death'
# time_var <- 'd_days_to_death'

intervention_vars <- c('d_intervention_T1', 'd_intervention_T2')
############


############
# Survival analysis setup for group comparisons
# Bivariate analysis

###

# Original follow-up to max period in database (FECHA_INGRESO):
# outcome <- factor(data_f[[outcome_var]], levels = c(0, 1), labels = c("censored", "non-survival"))

# 30 day mortality follow-up:
outcome <- factor(data_f[[outcome_var]], levels = c(0, 1), labels = c("censored", "non-survival"))

# Naming var for saving to disk:
i <- outcome_var

counts <- table(data_f$d_intervention_T1, outcome)
file_n <- 'survival_counts_T1'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
epi_write(file_object = counts,
          file_name = outfile
          )

counts <- table(data_f$d_intervention_T2, outcome)
file_n <- 'survival_counts_T2'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
epi_write(file_object = counts,
          file_name = outfile
          )


props <- round(prop.table(table(data_f$d_intervention_T1, outcome)), digits = 2)
file_n <- 'survival_proportions_T1'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
epi_write(file_object = props,
          file_name = outfile
          )

props <- round(prop.table(table(data_f$d_intervention_T2, outcome)), digits = 2)
file_n <- 'survival_proportions_T2'
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

# Plots will have 'others' from intervention bars, because:
# Can't exclude 'other' rows in d_intervention_T1 
# and
# d_intervention_T2 as effectively have two datasets now and would remove all admissions from excluded states at either T1 analysis or T2.
# Exclude at each analysis separately

summary(data_f[['d_intervention_T1']])
summary(data_f[['d_intervention_T2']])

# Duplicating code as passing formulas to ggsurvplot seems to be a nightmare and errors frequently
summary(data_f[[time_var]])
summary(factor(data_f[[outcome_var]]))
table(data_f[[time_var]], data_f[[outcome_var]])
str(data_f)

# # Basic formula string construction
# mod_spec <- sprintf("Surv(%s, %s) ~ %s", time_var, outcome_var, tx)
# mod_spec
# mod_form <- as.formula(mod_spec)
# mod_form

# km_fit <- survfit(mod_form, data = data_f)
km_fit1 <- survfit(Surv(data_f[[time_var]], data_f[[outcome_var]]) ~ d_intervention_T1,
                  data = data_f
                  )
# km_fit$call

ylim <- c(0.90, 1.00)

d_intervention_T1_p <- ggsurvplot(km_fit1, data = data_f, pval = TRUE, conf.int = TRUE, 
                                  ggtheme = theme_minimal(), 
                                  title = "Kaplan-Meier Survival Curves",
                                  ylim = ylim)

d_intervention_T1_p$plot
d_intervention_T1_p$data.survtable
d_intervention_T1_p$data.survplot

km_fit2 <- survfit(Surv(data_f[[time_var]], data_f[[outcome_var]]) ~ d_intervention_T2,
                  data = data_f
                  )

d_intervention_T2_p <- ggsurvplot(km_fit2, data = data_f, pval = TRUE, conf.int = TRUE, 
                                  ggtheme = theme_minimal(), 
                                  title = "Kaplan-Meier Survival Curves",
                                  ylim = ylim)

interv_plots <- list(d_intervention_T1_p, d_intervention_T2_p)
interv_fits <- list(km_fit1, km_fit2)

counter <- 0
for (tx in interv_plots) {
    counter <- counter + 1
    tx_name <- intervention_vars[counter]

    # Save plot:
    i <- time_var
    file_n <- 'plots_KM_group'
    suffix <- 'pdf'
    outfile <- sprintf(fmt = '%s/%s_%s_%s.%s', infile_prefix, file_n, i, tx_name, suffix)
    outfile
    ggplot2::ggsave(filename = outfile, plot = tx$plot)
    
    # Save the tables from survminer as well:
    file_n <- 'survival_table'
    suffix <- 'txt'
    outfile <- sprintf(fmt = '%s/%s_%s_%s.%s', infile_prefix, file_n, i, tx_name, suffix)
    outfile
    epi_write(file_object = tx$data.survtable,
              file_name = outfile
              )
    
    file_n <- 'data_survival_plot'
    suffix <- 'txt'
    outfile <- sprintf(fmt = '%s/%s_%s_%s.%s', infile_prefix, file_n, i, tx_name, suffix)
    outfile
    epi_write(file_object = tx$data.survplot,
              file_name = outfile
    )
    }

###


###
# Boxplot of survival times by group

for (tx in intervention_vars) {
    i <- time_var
    box_surv <- epi_plot_box(data_f,
                             var_y = time_var,
                             var_x = tx,
                             fill = tx,
                             ) +
        labs(title = "Boxplot of Survival Times by Group",
             x = "Group",
             y = "Time") + # remove legend
        theme(legend.position = "none")  +
        guides(fill = "none") # remove within plot symbols
    box_surv

    file_n <- 'plots_surv_box'
    suffix <- 'pdf'
    outfile <- sprintf(fmt = '%s/%s_%s_%s.%s', infile_prefix, file_n, i, tx, suffix)
    outfile
    ggplot2::ggsave(filename = outfile, plot = box_surv)
    }
###


###
# Cumulative hazard plot

cum_haz1 <- ggsurvplot(km_fit1, data = data_f, fun = "cumhaz", 
           ggtheme = theme_minimal(), 
           title = "Cumulative Hazard Plot - T1")

cum_haz2 <- ggsurvplot(km_fit2, data = data_f, fun = "cumhaz", 
           ggtheme = theme_minimal(), 
           title = "Cumulative Hazard Plot - T2")

cum_haz1$plot
cum_haz1$data.survtable
cum_haz1$data.survplot

cum_haz_plots <- list(cum_haz1, cum_haz2)

counter <- 0
for (tx in cum_haz_plots) {
    counter <- counter + 1
    tx_name <- intervention_vars[counter]
    
    i <- time_var
    file_n <- 'plots_cum_haz_group'
    suffix <- 'pdf'
    outfile <- sprintf(fmt = '%s/%s_%s_%s.%s', infile_prefix, file_n, i, tx_name, suffix)
    outfile
    ggplot2::ggsave(filename = outfile, plot = tx$plot)
    
    # Save the tables from survminer as well:
    file_n <- 'survival_table_cum_haz'
    suffix <- 'txt'
    outfile <- sprintf(fmt = '%s/%s_%s_%s.%s', infile_prefix, file_n, i, tx_name, suffix)
    outfile
    epi_write(file_object = tx$data.survtable,
              file_name = outfile
              )
    
    file_n <- 'data_survival_plot_cum_haz'
    suffix <- 'txt'
    outfile <- sprintf(fmt = '%s/%s_%s_%s.%s', infile_prefix, file_n, i, tx_name, suffix)
    outfile
    epi_write(file_object = tx$data.survplot,
              file_name = outfile
    )
    }
###
############


############
###
# Use only d_intervention periods:
summary(data_f$d_time_cuts_prev)
# intervention_points <- data_f$d_time_cuts_prev
# data_f$d_time_cuts_prev <- factor(data_f$d_time_cuts_prev)
str(data_f$d_time_cuts_prev)
levels(data_f$d_time_cuts_prev)

# Levels with values of 0 will error:
# Had this issue with 2021 database but once joined should be fine
# data_f$d_time_cuts_prev <- droplevels(data_f$d_time_cuts_prev)
# levels(data_f$d_time_cuts_prev)
# summary(data_f$d_time_cuts_prev)

# Also:
summary(data_f$d_intervention)
# Should be fine
# data_f$d_intervention <- droplevels(data_f$d_intervention)
# levels(data_f$d_intervention)
# summary(data_f$d_intervention)
###


###
# Desc stats by time cut-offs:

for (i in intervention_vars) {
    counts_by_dates <- table(data_f[[i]], data_f$d_time_cuts_prev)
    counts_by_dates
    file_n <- 'counts_by_dates'
    suffix <- 'txt'
    outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
    outfile
    epi_write(file_object = counts_by_dates,
              file_name = outfile
              )
    
    props_by_dates <- round(prop.table(counts_by_dates), digits = 2)
    props_by_dates
    file_n <- 'proportions_by_dates'
    suffix <- 'txt'
    outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
    outfile
    epi_write(file_object = props_by_dates,
              file_name = outfile
    )
    }
###


###
# Desc stats by time cut-offs, outcome and d_intervention:


for (i in intervention_vars) {
    counts_by_dates_outcome <- table(data_f[[i]],
                                     data_f$d_time_cuts_prev,
                                     data_f[[outcome_var]]
                                     )
    str(counts_by_dates_outcome)
    
    file_n <- 'counts_by_dates_outcome'
    suffix <- 'txt'
    outfile <- sprintf(fmt = '%s/%s_%s_%s.%s', infile_prefix, file_n, outcome_var, i, suffix)
    outfile
    epi_write(file_object = as.data.frame(counts_by_dates_outcome),
              file_name = outfile
              )
    
    props_by_dates_outcome <- round(prop.table(counts_by_dates_outcome), digits = 4)
    str(props_by_dates_outcome)
    props_by_dates_outcome
    
    file_n <- 'props_by_dates_outcome'
    suffix <- 'txt'
    outfile <- sprintf(fmt = '%s/%s_%s_%s.%s', infile_prefix, file_n, outcome_var, i, suffix)
    outfile
    epi_write(file_object = as.data.frame(props_by_dates_outcome),
              file_name = outfile
    )
    }
###


###
# There shouldn't be NAs, result should be FALSE:
any(is.na(data_f$d_time_cuts_prev)) # is at least one value TRUE?
any(is.na(data_f$d_intervention_T1))
any(is.na(data_f$d_intervention_T2))

# Survival object by groups (d_intervention) and dates (d_time_cuts_prev):
surv_obj <- Surv(data_f[[time_var]], data_f[[outcome_var]])
surv_fit_point1 <- survival::survfit(surv_obj ~ d_intervention_T1 + d_time_cuts_prev, data = data_f)
surv_fit_point2 <- survival::survfit(surv_obj ~ d_intervention_T2 + d_time_cuts_prev, data = data_f)

str(surv_fit_point1)
surv_fit_point1$strata
summary(surv_fit_point1)

ylim <- c(0.90, 1.00)

km_grps_dates1 <- ggsurvplot(surv_fit_point1,
                            data = data_f,
                            xlab = "Time (days)",
                            ylab = "Survival Probability",
                            title = "Kaplan-Meier Survival Curves by Intervention and Study Points - T1",
                            # facet.by = 'd_intervention', #"d_time_cuts_prev",
                            risk.table = TRUE,  # Add risk table
                            pval = TRUE,  # Add p-value
                            ylim = ylim
                            )

km_grps_dates2 <- ggsurvplot(surv_fit_point2,
                            data = data_f,
                            xlab = "Time (days)",
                            ylab = "Survival Probability",
                            title = "Kaplan-Meier Survival Curves by Intervention and Study Points - T2",
                            # facet.by = 'd_intervention', #"d_time_cuts_prev",
                            risk.table = TRUE,  # Add risk table
                            pval = TRUE,  # Add p-value
                            ylim = ylim
                            )
# Still errors with faceting so print separately
km_by_dates <- list(km_grps_dates1, km_grps_dates2)

counter <- 0
for (tx in km_by_dates) {
    counter <- counter + 1
    tx_name <- intervention_vars[counter]

    # Save plot:
    i <- time_var
    file_n <- 'plots_km_group_dates'
    suffix <- 'pdf'
    outfile <- sprintf(fmt = '%s/%s_%s_%s.%s', infile_prefix, file_n, i, tx_name, suffix)
    outfile
    ggplot2::ggsave(filename = outfile, plot = tx$plot,
                    units = 'in',
                    height = 25,
                    width = 25
                    )
    
    # Save the tables from survminer as well:
    file_n <- 'survival_table_km_group_dates'
    suffix <- 'txt'
    outfile <- sprintf(fmt = '%s/%s_%s_%s.%s', infile_prefix, file_n, i, tx_name, suffix)
    outfile
    epi_write(file_object = tx$data.survtable,
              file_name = outfile
              )
    
    file_n <- 'data_survival_plot_km_group_dates'
    suffix <- 'txt'
    outfile <- sprintf(fmt = '%s/%s_%s_%s.%s', infile_prefix, file_n, i, tx_name, suffix)
    outfile
    epi_write(file_object = tx$data.survplot,
              file_name = outfile
    )
    }

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
pre_T0 <- data_f[data_f$d_time_cuts_prev == 'pre_T0', ]
T0 <- data_f[data_f$d_time_cuts_prev == 'T0', ]
gap_T1_T0 <- data_f[data_f$d_time_cuts_prev == 'gap_T0_T1', ]
T1 <- data_f[data_f$d_time_cuts_prev == 'T1', ]
gap_T1_T2 <- data_f[data_f$d_time_cuts_prev == 'gap_T1_T2', ]
T2 <- data_f[data_f$d_time_cuts_prev == 'T2', ]
post_T2 <- data_f[data_f$d_time_cuts_prev == 'post_T2', ]
###


###
# For T1:

subsets_by_cut <- list(pre_T0, T0, gap_T1_T0, T1, gap_T1_T2, T2, post_T2)
titles <- levels(data_f$d_time_cuts_prev)
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
    surv_fit_level <- survfit(Surv(time = d_days_to_death_30, #d_days_to_death,
                                   event = d_death_30, #d_death
                                   ) ~ d_intervention_T1,
                              data = df
                              )

    # Check underlying data:
    print(nrow(df))
    # Check model specs:
    print(summary(surv_fit_level,
                  times = c(1, median(df[["d_days_to_death_30"]], #df[["d_days_to_death"]],
                                      na.rm = TRUE)
                            )
                  )
          )  # Print summary at specific times

    plot_1 <- ggsurvplot(surv_fit_level,
                         data = df,
                         xlab = "Time (days)",
                         ylab = "Survival Probability",
                         title = paste("KM Survival Curves at T1 -", plot_title),
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
tx_name <- 'T1'
file_n <- sprintf('plots_km_dates_by_levels_facet_%s_%s', z, tx_name)
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
    outfile <- sprintf(fmt = '%s/%s_%s_%s_%s_%s.%s', infile_prefix, file_n, outcome_var, time_var, i, tx_name, suffix)
    outfile
    # Check list is not empty, skip if empty:
    if(!is.null(surv_table_km_dates_by_levels[[i]])) {
        epi_write(file_object = surv_table_km_dates_by_levels[[i]],
                  file_name = outfile
                  )
        } else {
            print(i)
            print("Empty list, not saving")
            }
        
    
    file_n <- 'data_plot_km_dates_by_levels'
    suffix <- 'txt'
    outfile <- sprintf(fmt = '%s/%s_%s_%s_%s_%s.%s', infile_prefix, file_n, outcome_var, time_var, i, tx_name, suffix)
    outfile
    if(!is.null(data_plot_km_dates_by_levels[[i]])) {
        epi_write(file_object = data_plot_km_dates_by_levels[[i]],
                  file_name = outfile
                  )
    } else {
            print(i)
            print("Empty list, not saving")
            }
    }
###


###
# For T2:

subsets_by_cut <- list(pre_T0, T0, gap_T1_T0, T1, gap_T1_T2, T2, post_T2)
titles <- levels(data_f$d_time_cuts_prev)
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
    surv_fit_level <- survfit(Surv(time = d_days_to_death_30, #d_days_to_death,
                                   event = d_death_30, #d_death
                                   ) ~ d_intervention_T2,
                              data = df
                              )

    # Check underlying data:
    print(nrow(df))
    # Check model specs:
    print(summary(surv_fit_level,
                  times = c(1, median(df[["d_days_to_death_30"]], #df[["d_days_to_death"]],
                                      na.rm = TRUE)
                            )
                  )
          )  # Print summary at specific times

    plot_1 <- ggsurvplot(surv_fit_level,
                         data = df,
                         xlab = "Time (days)",
                         ylab = "Survival Probability",
                         title = paste("KM Survival Curves at T2 -", plot_title),
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
tx_name <- 'T2'
file_n <- sprintf('plots_km_dates_by_levels_facet_%s_%s', z, tx_name)
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
    outfile <- sprintf(fmt = '%s/%s_%s_%s_%s_%s.%s', infile_prefix, file_n, outcome_var, time_var, i, tx_name, suffix)
    outfile
    # Check list is not empty, skip if empty:
    if(!is.null(surv_table_km_dates_by_levels[[i]])) {
        epi_write(file_object = surv_table_km_dates_by_levels[[i]],
                  file_name = outfile
                  )
        } else {
            print(i)
            print("Empty list, not saving")
            }
        
    
    file_n <- 'data_plot_km_dates_by_levels'
    suffix <- 'txt'
    outfile <- sprintf(fmt = '%s/%s_%s_%s_%s_%s.%s', infile_prefix, file_n, outcome_var, time_var, i, tx_name, suffix)
    outfile
    if(!is.null(data_plot_km_dates_by_levels[[i]])) {
        epi_write(file_object = data_plot_km_dates_by_levels[[i]],
                  file_name = outfile
                  )
    } else {
            print(i)
            print("Empty list, not saving")
            }
    }

###
############


############
# No changes to the rdata file loaded, no need to save again.
# # The end:
# # Save objects, to eg .RData file:
# folder <- '../data/processed'
# script <- '4a_surv_outcome_bivariate'
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
