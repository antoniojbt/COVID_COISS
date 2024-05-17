############
# COISS paper 1
# L. Bonifaz
# April 2024
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
############


############
# Load a previous R session, data and objects:
infile <- '../data/processed/2_df_subset_COVID19MEXICO2021_COVID-only_COISS-only.rdata.gzip'
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
# Missing data
na_perc <- epi_stats_na_perc(df = data_f, margin = 2)
na_perc
# 99% of FECHA_DEF is missing/censored
# Claves 97, 98, 99, 997, 998, 999, 36 are NA or NA equivalent labels

# Variables with missing values above x:
# missing_val <- na_perc[na_perc$na_perc > 5 , ]
ord <- order(na_perc$na_perc, decreasing = T)
ord
na_perc <- na_perc[c(ord), ]
na_perc

# Variables above x% missing data:
nrow(na_perc[na_perc$na_perc > 0, ])
# 27 2024 db
nrow(na_perc[na_perc$na_perc > 5, ])
# 8 2024 db
nrow(na_perc[na_perc$na_perc > 15, ])
# 8 2024 db

rownames(na_perc)

# Complete cases
epi_head_and_tail(data_f)
table(complete.cases(data_f))
# No row is complete

# Cols with > 20% missing data:
cols_miss <- c("PAIS_ORIGEN",
               "MIGRANTE",
               "FECHA_DEF",
               "UCI",
               "INTUBADO",
               "RESULTADO_LAB",
               "EMBARAZO",
               "RESULTADO_ANTIGENO"
               )
table(complete.cases(data_f[ ,which(!colnames(data_f) %in% cols_miss)
                             ])
      )
# About 10% have a missing entry once problem variables are excluded


# outfile <- epi_output_name(input_name = infile, suffix = '.txt')
infile_prefix
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/na_perc.%s', infile_prefix, suffix)
outfile
dir()

epi_write(na_perc,
          outfile,
          row.names = TRUE,
          col.names = TRUE
          )
############


############
# Summary statistics

###
# Numeric columns:
epi_clean_count_classes(data_f)
dplyr::glimpse(data_f)
str(data_f)

# Get numeric columns for stats:
# col_nums <- list()
# i <- 'EDAD'
# for (i in colnames(data_f)) {
#   if (epi_clean_cond_numeric(data_f[[i]])) {
#     col_nums <- c(col_nums, i)
#   }
# }
# col_nums

stats_num <- epi_stats_summary(df = data_f, class_type = 'int_num')
stats_num

# Numeric data summary doesn't need tidying but could be formatted:
stats_num <- epi_stats_format(stats_num, digits = 2)
# View(stats_num)
# EDAD is only numeric variable

# Save as table
# pwd already in results folder:
infile_prefix
file_n <- 'sum_stats'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s.%s', infile_prefix, file_n, suffix)
outfile

epi_write(file_object = stats_num, file_name = outfile)

# Factor
str(data_f)
###


###
# Factor columns:
# Remove ID row as will be picked up:
col_facts <- data_f[, c(-1)]

# Include outcome data as factor:
col_facts$death <- as.factor(col_facts$death)

# Get desc stats:
stats_fct <- epi_stats_summary(df = col_facts, class_type = 'chr_fct')
stats_fct
unique(stats_fct$id)
# View(stats_fct)
dim(data_f)
colnames(data_f)

# Add total for percentage calculation and order column to tidy up results:
perc_n <- nrow(col_facts)
order_by <- 'percent'
stats_fct_tidy <- epi_stats_tidy(sum_df = stats_fct,
                                 order_by = order_by,
                                 perc_n = perc_n
                                 )
stats_fct_tidy
# Format them if needed:
stats_fct_tidy <- epi_stats_format(stats_fct_tidy, digits = 0)
stats_fct_tidy

# Save as table
infile_prefix
file_n <- 'stats_factors'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s.%s', infile_prefix, file_n, suffix)
outfile

epi_write(file_object = stats_fct_tidy,
          file_name = outfile
          )
str(data_f)
###
############


############
# Plots

num_vars <- list()
for (i in colnames(data_f)) {
  if (epi_clean_cond_numeric(data_f[[i]])) {
    num_vars <- c(num_vars, i)
  }
}
num_vars


###
# Numeric, boxplots:
epi_plot_box(df = data_f, var_y = 'EDAD')

# i <- "EDAD"
num_list <- epi_plot_list(vars_to_plot = num_vars)
for (i in names(num_list)) {
  num_list[[i]] <- epi_plot_box(df = data_f, var_y = i)
  }

# Save plots
# Plot 4 per page or so:
per_file <- 4
jumps <- seq(1, length(num_list), per_file)
length(jumps)

# i <- 2
for (i in jumps) {
  # infile_prefix
  file_n <- 'plots_box'
  suffix <- 'pdf'
  outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
  # outfile
  start_i <- i
  end_i <- i + 3
  my_plot_grid <- epi_plots_to_grid(num_list[start_i:end_i])
  epi_plot_cow_save(file_name = outfile, plot_grid = my_plot_grid)
}
###


###
# Histograms:
epi_plot_hist(df = data_f, var_x = 'EDAD')

num_list <- NULL
i <- NULL
num_list <- epi_plot_list(vars_to_plot = num_vars)
for (i in names(num_list)) {
  print(i)
  num_list[[i]] <- epi_plot_hist(df = data_f, var_x = i)
  }
# num_list

# Save plots
# Plot 4 per page or so:
per_file <- 4
jumps <- seq(1, length(num_list), per_file)
length(jumps)

# i <- 2
for (i in jumps) {
  # infile_prefix
  file_n <- 'plots_hist'
  suffix <- 'pdf'
  outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
  # outfile
  start_i <- i
  end_i <- i + 3
  my_plot_grid <- epi_plots_to_grid(num_list[start_i:end_i])
  epi_plot_cow_save(file_name = outfile, plot_grid = my_plot_grid)
}
###


###
# Plots for factors
str(data_f)
# summary(data_raw$ORIGEN)
plot_bar <- epi_plot_bar(df = data_f, 'ORIGEN')
plot_bar
# plot_bar_raw <- epi_plot_bar(df = data_raw, 'ORIGEN')
# plot_bar_raw

# summary(data_raw$SECTOR)
plot_bar <- epi_plot_bar(df = data_f, 'SECTOR')
plot_bar
# plot_bar_raw <- epi_plot_bar(df = data_raw, 'SECTOR')
# plot_bar_raw

# Plot all factors 
# Include outcome as factor
colnames(col_facts)

# Bar plots:
fact_vars <- colnames(col_facts)
i <- "ENTIDAD_RES"
epi_plot_bar(df = data_f, var_x = i, ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

bar_list <- epi_plot_list(vars_to_plot = fact_vars)
for (i in names(bar_list)) {
    # print(i)
    bar_list[[i]] <- epi_plot_bar(df = data_f, var_x = i) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
}
# bar_list

# Save plots
# Plot 4 per page or so:
per_file <- 4
jumps <- seq(1, length(bar_list), per_file)
length(jumps)

# i <- 2
for (i in jumps) {
    # infile_prefix
    file_n <- 'plots_bar'
    suffix <- 'pdf'
    outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
    # outfile
    start_i <- i
    end_i <- i + 3
    my_plot_grid <- epi_plots_to_grid(bar_list[start_i:end_i])
    epi_plot_cow_save(file_name = outfile,
                      plot_grid = my_plot_grid,
                      base_width = 15,
                      base_height = 15
    )
    }
###
############


############
# Descriptive stats for dates
data_f$FECHA_ACTUALIZACION
data_f$FECHA_INGRESO
data_f$FECHA_SINTOMAS
data_f$FECHA_DEF
typeof(data_f$FECHA_INGRESO)

col_dates <- data_f %>% select(matches("^FECHA")) %>% colnames(.)
col_dates

# Loop:
sum_dates_df <- data.frame('variable' = character(0),
                           'Min' = numeric(0),
                           'q25%' = numeric(0),
                           'Median' = numeric(0),
                           'q75%' = numeric(0),
                           'Max' = numeric(0),
                           'IQR' = numeric(0),
                           stringsAsFactors = FALSE
)
# sum_dates_df

for (i in col_dates) {
    sum_dates <- epi_stats_dates(data_f[[i]])
    # print(sum_dates)
    stats_vector <- sum_dates$Value  # Extract the values

    # Data frame row to append:
    new_row <- data.frame(
        Variable = i,
        Min = stats_vector[1],
        `q25%` = stats_vector[2],
        Median = stats_vector[3],
        `q75%` = stats_vector[4],
        Max = stats_vector[5],
        IQR = stats_vector[6]
    )
    sum_dates_df <- rbind(sum_dates_df,
                          new_row)
}
sum_dates_df


infile_prefix
file_n <- 'desc_dates'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s.%s', infile_prefix, file_n, suffix)
outfile

epi_write(sum_dates_df, outfile)
############


############
# Get frequency tables
dates_list <- list()
for (i in col_dates) {
    # Calculate differences between consecutive dates to find gaps and clusters:
    date_ord <- as.numeric(data_f[[i]])
    inds <- order(date_ord,
                  decreasing = FALSE,
                  na.last = TRUE
    )
    date_ord <- date_ord[inds]
    date_diffs <- diff(date_ord)
    # date_diffs
    # Convert numeric differences back to an interpretable form (e.g., days):
    # date_diffs_days <- as.Date(date_diffs, origin = "1970-01-01") - as.Date("1970-01-01") # although diff() was already working on days, so same result
    # date_diffs_days
    # Frequency table by month-year:
    date_frequencies <- table(format(data_f[[i]], "%Y-%m"))  # Counts by year and month
    # date_frequencies
    dates_list[[i]] <- list(
        Date_Differences = date_diffs,
        Frequencies = date_frequencies
    )
}
names(dates_list)
names(dates_list$FECHA_INGRESO)


# Save:
for (i in names(dates_list)) {
    # print(i)
    infile_prefix
    file_n <- 'freq'
    file_n2 <- 'Date_Differences'
    suffix <- 'txt'
    outfile <- sprintf(fmt = '%s/%s_%s_%s.%s', infile_prefix, file_n, i, file_n2, suffix)
    outfile

    # dates_list$FECHA_ACTUALIZACION$Date_Differences
    df_out <- as.data.frame(dates_list[[i]][[1]])
    epi_write(df_out, outfile)

    infile_prefix
    file_n <- 'freq'
    file_n2 <- 'Frequencies'
    suffix <- 'txt'
    outfile <- sprintf(fmt = '%s/%s_%s_%s.%s', infile_prefix, file_n, i, file_n2, suffix)
    outfile
    # dates_list$FECHA_ACTUALIZACION$Frequencies
    df_out <- as.data.frame(dates_list[[i]][[2]])
    epi_write(df_out, outfile)
    }

############


############
# Plot dates
col_dates
str(data_f[, col_dates])
typeof(data_f$FECHA_ACTUALIZACION)
typeof(as.Date(data_f$FECHA_ACTUALIZACION))


####
# Histograms of date frequencies:
ggplot(data = data_f, aes(x = FECHA_INGRESO)) +
    geom_histogram()

ggplot(data = data_f, aes(x = FECHA_ACTUALIZACION)) +
    geom_histogram()
table(data_f$FECHA_ACTUALIZACION)

i <- 'FECHA_INGRESO'
epi_plot_hist(df = data_f, var_x = i) +
    geom_density(col = 2)

hist_list <- epi_plot_list(vars_to_plot = col_dates)
for (i in names(hist_list)) {
    # print(i)
    hist_list[[i]] <- epi_plot_hist(df = data_f, var_x = i) +
        geom_density(col = 2)
}
# hist_list

# Save plots
# Plot 4 per page or so:
per_file <- 4
jumps <- seq(1, length(hist_list), per_file)
length(jumps)

# i <- 2
for (i in jumps) {
    # infile_prefix
    file_n <- 'plots_hist'
    suffix <- 'pdf'
    outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
    # outfile

    start_i <- i
    end_i <- i + 3
    my_plot_grid <- epi_plots_to_grid(hist_list[start_i:end_i])
    epi_plot_cow_save(file_name = outfile, plot_grid = my_plot_grid)
}
####


####
# Box Plots: visualise range, median, quartiles, and outliers in date distributions:
i <- 'FECHA_INGRESO'
epi_plot_box(data_f, var_y = i)

box_list <- epi_plot_list(vars_to_plot = col_dates)
for (i in names(box_list)) {
    # print(i)
    box_list[[i]] <- epi_plot_box(df = data_f, var_y = i)
    }
# box_list

# Save plots
# Plot 4 per page or so:
per_file <- 4
jumps <- seq(1, length(box_list), per_file)
length(jumps)

# i <- 2
for (i in jumps) {
    # infile_prefix
    file_n <- 'plots_box'
    suffix <- 'pdf'
    outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
    # outfile

    start_i <- i
    end_i <- i + 3
    my_plot_grid <- epi_plots_to_grid(box_list[start_i:end_i])
    epi_plot_cow_save(file_name = outfile, plot_grid = my_plot_grid)
}
####


####
# Time Series Plot: Visualise distribution over time:

###
# Needs dates ordered, increasing:
i <- 'FECHA_INGRESO'
ord_i <- sort(as.Date(data_f[[i]]))
# ord_i <- data_f[ord_i, i]
# ord_i <- as.vector(ord_i)
str(ord_i)
head(ord_i)
tail(ord_i)
length(ord_i)

# Get a subset to plot:
rand_indices <- sort(sample(length(ord_i), size = 1000))
# rand_indices <- rand_indices[order(rand_indices)]
head(rand_indices)
tail(rand_indices)

# Subset the data frame:
date <- ord_i[rand_indices]
value <- seq_along(date)
plot_data <- data.frame(i = date, value = value)
colnames(plot_data)[1] <- i
str(plot_data)
epi_head_and_tail(plot_data, cols = 2)

# Plot:
ggplot(plot_data, aes(x = !!sym(i), y = value)) +
    geom_line() +
    geom_point()
###

###
time_list <- epi_plot_list(vars_to_plot = col_dates)
for (i in names(time_list)) {
    # Get date, sort it, pass it as vector, add value var for plotting:
    date <- sort(as.Date(data_f[[i]]))
    value <- seq_along(date)
    plot_data <- data.frame(i = date, value = value)
    colnames(plot_data)[1] <- i
    # Plot:
    time_list[[i]] <- ggplot(plot_data, aes(x = !!sym(i), y = value)) +
        geom_line() +
        geom_point()
}
# time_list

# Save plots
# Plot 4 per page or so:
per_file <- 4
jumps <- seq(1, length(time_list), per_file)
length(jumps)

# i <- 2
for (i in jumps) {
    # infile_prefix
    file_n <- 'plots_time'
    suffix <- 'pdf'
    outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
    # outfile

    start_i <- i
    end_i <- i + 3
    my_plot_grid <- epi_plots_to_grid(time_list[start_i:end_i])
    epi_plot_cow_save(file_name = outfile, plot_grid = my_plot_grid)
}
####
############


############
# The end:
# Save objects, to eg .RData file:
folder <- '../data/processed'
script <- '3_desc_plots'
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
