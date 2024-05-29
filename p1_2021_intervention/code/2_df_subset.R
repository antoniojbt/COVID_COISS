############
# COISS paper 1
# L. Bonifaz
# April 2024

# This script is specific to COISS vs non-COISS 2021-2022 COVID positive analysis
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
# infile <- '../data/processed/1_setup_COVID19MEXICO2021.rdata.gzip'
infile <- '../data/processed/1_setup_COVID19MEXICO_2021_2022.rdata.gzip'
load(infile, verbose = TRUE)

data_f <- data_f # just to get rid of RStudio warnings
dim(data_f)
str(data_f)
epi_head_and_tail(data_f)
colnames(data_f)
summary(factor(data_f$d_death))
summary(data_f$d_days_to_death)

# For saving/naming outputs, should already come with the RData file though:
infile_prefix <- infile_prefix
infile_prefix
# Otherwise manually:
# infile_prefix <- 'COVID19MEXICO2021'
############


############
# Do manually if sub-setting
# Re-run code below as is but with subset of COVID-only rows:
# Use "CLASIFICACION_FINAL" codes 1, 2, 3:
data_f_COVID <- data_f[which(as.integer(data_f$CLASIFICACION_FINAL) < 4), ]
dim(data_f)
dim(data_f_COVID)

table(data_f$CLASIFICACION_FINAL)
table(data_f_COVID$CLASIFICACION_FINAL)

# Remove objects, keep objects names so that code can be re-run as several DBs:
data_f <- data_f_COVID
rm(list = c('data_f_COVID'))

# Check this manually:
file_n <- 'COVID-only_COISS-only'
# infile_prefix <- NULL
infile_prefix <- sprintf('%s_%s', infile_prefix, file_n)
infile_prefix

# Do manually, rename outdir if sub-setting
# Create a folder for results:
dir()
if (!dir.exists(infile_prefix)) {
    dir.create(infile_prefix)
}
dir()
############


############
# New variables
# Time cuts for admission
# Time cuts for outcome
# Time cuts for point prevalence

# ###
# Analysis dates, based on epidemic waves:
# T0_start	30 July 2021
# T0_end	02 August 2021
# T1_start	03 September 2021
# T1_end	08 September 2021
# T2_start	19 December 2021
# T2_end	28 February 2022
# Figure 2


T0_start <- '2021-07-30'
T0_end <- '2021-08-02'
T1_start <- '2021-09-03'
T1_end <- '2021-09-08'
T2_start <- '2021-12-19'
T2_end <- '2022-02-28'

# Time cuts needed are based separately on:
# FECHA_INGRESO
# FECHA_DEF

# so that DiD can be performed on e.g. T1 accounting for T0, and T2 accounting for T0
###
############


############
###
# Time cuts for admission
# The following is based on FECHA_INGRESO:
df_recode <- data_f

length(which(as.Date(df_recode$FECHA_INGRESO) >= as.Date(T0_start)))
length(which(df_recode$FECHA_INGRESO < T0_start))

summary(df_recode$FECHA_INGRESO)
summary(df_recode$FECHA_INGRESO_char)
dim(df_recode)
# Looks like for 2021 database, once COVID-only cases are kept, the last date is before the T2 cut-off.

summary(df_recode$FECHA_DEF)
summary(df_recode$FECHA_ACTUALIZACION)
summary(df_recode$FECHA_SINTOMAS)

df_recode$d_time_cuts_INGRESO <- ifelse(df_recode$FECHA_INGRESO >= T0_start & df_recode$FECHA_INGRESO <= T0_end, 'T0',
                           ifelse(df_recode$FECHA_INGRESO >= T1_start & df_recode$FECHA_INGRESO <= T1_end, 'T1',
                                  ifelse(df_recode$FECHA_INGRESO >= T2_start & df_recode$FECHA_INGRESO <= T2_end, 'T2',
                                         ifelse(df_recode$FECHA_INGRESO < T0_start, 'pre-T0',
                                                ifelse(df_recode$FECHA_INGRESO > T0_end & df_recode$FECHA_INGRESO < T1_start, 'gap_T0_T1',
                                                       ifelse(df_recode$FECHA_INGRESO > T1_end & df_recode$FECHA_INGRESO < T2_start, 'gap_T1_T2',
                                                              ifelse(df_recode$FECHA_INGRESO > T2_end, 'post-T2',
                                                                     'check'
                                                                     )))))))

dim(df_recode)
summary(factor(df_recode$d_time_cuts_INGRESO))
df_recode$d_time_cuts_INGRESO
df_recode$d_time_cuts_INGRESO <- factor(df_recode$d_time_cuts_INGRESO,
                           levels = c('pre-T0', 'T0',
                                      'gap_T0_T1', 'T1',
                                      'gap_T1_T2', 'T2',
                                      'post-T2'),
                           # labels = levels,
                           ordered = TRUE
                           )
summary(df_recode$d_time_cuts_INGRESO)

# Looks OK:
epi_head_and_tail(df_recode[df_recode$d_time_cuts_INGRESO == 'pre-T0', ], cols = 6)
epi_head_and_tail(df_recode[df_recode$d_time_cuts_INGRESO == 'T0', ], cols = 6)
epi_head_and_tail(df_recode[df_recode$d_time_cuts_INGRESO == 'T1', ], cols = 6)

dim(df_recode)

# Plot:
i <- 'd_time_cuts_INGRESO'
file_n <- 'plots_bar'
suffix <- 'pdf'
infile_prefix
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
plot_list <- list()
plot_1 <- epi_plot_bar(df_recode, var_x = 'd_time_cuts_INGRESO')
plot_list[[i]] <- plot_1
my_plot_grid <- epi_plots_to_grid(plot_list = plot_list)
epi_plot_cow_save(file_name = outfile, plot_grid = my_plot_grid)

# Clean up a bit:
data_f <- df_recode
rm(list = c('df_recode'))
str(data_f)
###
############


# TO DO: continue here, re-run, clean up, etc.
# Maybe just keep true_NA as character
############
###
# The following is based on FECHA_DEF:
# Time cuts for outcome
# sink("output_capture.txt")
df_recode <- data_f

length(which(as.Date(df_recode$FECHA_DEF) >= as.Date(T0_start)))
length(which(df_recode$FECHA_DEF < T0_start))

summary(df_recode$FECHA_DEF)
summary(df_recode$FECHA_INGRESO)
summary(df_recode$FECHA_ACTUALIZACION)
summary(df_recode$FECHA_SINTOMAS)

# Switch to char cols for dates for handling NA's because of '9999-99-99' database coding:
summary(df_recode$FECHA_DEF_char)
table(df_recode$FECHA_DEF_char)

# Issue with introducing rows with NA's was because NA handling was the first line in the ifelse statement
# but if last line then true NA's are lost
# Used as.char for handling 9999-99-99 codes but same issue
# Tried several things but best to handle true NA's separately, in different statements

df_recode$d_time_cuts_DEF <- NULL
# Apply the recoding logic using dplyr and case_when in separate statements so that clashes don't occur with NA's
df_recode <- df_recode %>%
  mutate(
    # Handle true NA's and censored first
    d_time_cuts_DEF = case_when(
        is.na(d_death) ~ 'true_NA', #NA_character_,  # Handle true NAs
        FECHA_DEF_char == '9999-99-99' ~ 'censored',
        TRUE ~ NA_character_  # Initialize as NA for other rows
    )
  )
summary(factor(df_recode$d_time_cuts_DEF))

# Handle date conditions separately
df_recode <- df_recode %>%
  mutate(
    d_time_cuts_DEF = case_when(
      !is.na(d_time_cuts_DEF) ~ d_time_cuts_DEF,  # Retain 'censored' and true NA's
      # is.na(d_death) ~ NA_character_,  # Handle true NAs
      is.na(d_time_cuts_DEF) & FECHA_DEF < T0_start ~ 'pre-T0',
      is.na(d_time_cuts_DEF) & FECHA_DEF >= T0_start & FECHA_DEF <= T0_end ~ 'T0',
      is.na(d_time_cuts_DEF) & FECHA_DEF > T0_end & FECHA_DEF < T1_start ~ 'gap_T0_T1',
      is.na(d_time_cuts_DEF) & FECHA_DEF >= T1_start & FECHA_DEF <= T1_end ~ 'T1',
      is.na(d_time_cuts_DEF) & FECHA_DEF > T1_end & FECHA_DEF < T2_start ~ 'gap_T1_T2',
      is.na(d_time_cuts_DEF) & FECHA_DEF >= T2_start & FECHA_DEF <= T2_end ~ 'T2',
      is.na(d_time_cuts_DEF) & FECHA_DEF > T2_end ~ 'post-T2',
      TRUE ~ 'check'  # Default case if none of the above match
    )
  )

dim(df_recode)
dim(data_f)
summary(factor(df_recode$d_time_cuts_DEF))
epi_head_and_tail(df_recode[df_recode$d_time_cuts_DEF == 'pre-T0', ], cols = 6)
# View(tail(df_recode[df_recode$d_time_cuts_DEF == 'pre-T0', ], n = 100))
subset_pre_T0 <- df_recode[df_recode$d_time_cuts_DEF == 'pre-T0', ]
summary(factor(subset_pre_T0$d_time_cuts_DEF)) # should only be 'pre-T0'

# Check if there are rows where all points are NA:
subset_pre_T0[rowSums(is.na(subset_pre_T0)) == ncol(subset_pre_T0), ] # should be empty
# Fine up to here
###
############


############
###
# TO DO: recheck this
# Return 'True_NAs' placeholder back to actual NA:
# True NA's are from non-sensical dates coded in original (max follow-up) outcome var (d_death)
# This and other tries re-introduce rows of NA's. They seem to be additional rows that couldn't be handled though.
# This appears to solely be when subsetting and not in the actual dataframe.
# Check separate script trying to debug this 'subset_NA_rows_issue.R'

df_recode2 <- df_recode %>%
  mutate(
    d_time_cuts_DEF = case_when(
        d_time_cuts_DEF == 'true_NA' ~ NA_character_, # Handle true NAs
        TRUE ~ d_time_cuts_DEF  # Retain all other rows
    )
  )

nrow(df_recode2) == nrow(data_f)
summary(factor(df_recode2$d_time_cuts_DEF))

# Subsetting introduces NA's but these aren't present in full dataset:
epi_head_and_tail(df_recode2[df_recode2$d_time_cuts_DEF == 'pre-T0', ], cols = 6)
# View(tail(df_recode2[df_recode2$d_time_cuts_DEF == 'pre-T0', ], n = 100))
subset_pre_T0 <- df_recode2[df_recode2$d_time_cuts_DEF == 'pre-T0', ]
summary(factor(subset_pre_T0$d_time_cuts_DEF)) # should only be 'pre-T0'
# Check if there are rows where all points are NA:
subset_pre_T0[rowSums(is.na(subset_pre_T0)) == ncol(subset_pre_T0), ] # should be empty but isn't

# Not OK:
epi_head_and_tail(df_recode2[df_recode2$d_time_cuts_DEF == 'pre-T0', ], cols = 6)
epi_head_and_tail(df_recode2[df_recode2$d_time_cuts_DEF == 'T0', ], cols = 6)
epi_head_and_tail(df_recode2[df_recode2$d_time_cuts_DEF == 'T1', ], cols = 6)


df_recode2$d_time_cuts_DEF <- factor(df_recode2$d_time_cuts_DEF,
                                 levels = c('pre-T0', 'T0',
                                            'gap_T0_T1', 'T1',
                                            'gap_T1_T2', 'T2',
                                            'post-T2', 'censored'),
                                 ordered = TRUE
                                 )

summary(df_recode2$d_time_cuts_DEF)
summary(df_recode2$d_time_cuts_INGRESO)
###
############


############
###
# Plot:
i <- 'd_time_cuts_DEF'
file_n <- 'plots_bar'
suffix <- 'pdf'
infile_prefix
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
plot_list <- list()
plot_1 <- epi_plot_bar(df_recode, var_x = 'd_time_cuts_DEF')
plot_list[[i]] <- plot_1
my_plot_grid <- epi_plots_to_grid(plot_list = plot_list)
epi_plot_cow_save(file_name = outfile, plot_grid = my_plot_grid)

# Clean up a bit:
data_f <- df_recode2
rm(list = c('df_recode', 'df_recode2'))
str(data_f)
###
############


############
###
# Time cuts for point prevalence
# Outcome death within each time-cut, needed for DiD analysis (as transversal, not long. as with days to death)
# Should equal outcome var (e.g. max follow-up with FECHA_ACTUALIZACION, or 30 day mortality from admission) plus d_time_cuts_DEF which has mark for death at each time period
# Should be equal to point prevalence, where all events during the period over whole population admitted (ie prior admissions still there plus new admission for the period)
# So:
# FECHA_DEF for the specific period is event occurred
# FECHA_INGRESO for the specific period is population at risk

df_recode <- data_f

colnames(df_recode)
summary(df_recode$d_time_cuts_INGRESO)
summary(df_recode$d_time_cuts_DEF)

# New var for prevalence during period, either FECHA_DEF or FECHA_INGRESO
# df_recode$d_time_cuts_prev <- NULL
df_recode$d_time_cuts_prev <- ifelse(is.na(df_recode$d_death), NA,
    ifelse(df_recode$d_time_cuts_INGRESO == 'pre-T0' | df_recode$d_time_cuts_DEF == 'pre-T0', 'pre-T0',
        ifelse(df_recode$d_time_cuts_INGRESO == 'T0' | df_recode$d_time_cuts_DEF == 'T0', 'T0',
            ifelse(df_recode$d_time_cuts_INGRESO == 'gap_T0_T1' | df_recode$d_time_cuts_DEF == 'gap_T0_T1', 'gap_T0_T1',
                ifelse(df_recode$d_time_cuts_INGRESO == 'T1' | df_recode$d_time_cuts_DEF == 'T1', 'T1',
                    ifelse(df_recode$d_time_cuts_INGRESO == 'gap_T1_T2' | df_recode$d_time_cuts_DEF == 'gap_T1_T2', 'gap_T1_T2',
                        ifelse(df_recode$d_time_cuts_INGRESO == 'T2' | df_recode$d_time_cuts_DEF == 'T2', 'T2',
                            ifelse(df_recode$d_time_cuts_INGRESO == 'post-T2' | df_recode$d_time_cuts_DEF == 'post-T2', 'post-T2',
                                   'check'
                                   ))))))))

dim(df_recode)
summary(factor(df_recode$d_time_cuts_prev))

# Var as factor:
df_recode$d_time_cuts_prev <- factor(df_recode$d_time_cuts_prev,
                                   levels = c('pre-T0', 'T0',
                                            'gap_T0_T1', 'T1',
                                            'gap_T1_T2', 'T2',
                                            'post-T2'),
                                 ordered = TRUE
                                 )
summary(df_recode$d_time_cuts_prev)

# TO DO: same issue as above when subsetting only:
# Not OK:
epi_head_and_tail(df_recode[df_recode$d_time_cuts_prev == 'pre-T0', ], cols = 6)
epi_head_and_tail(df_recode[df_recode$d_time_cuts_prev == 'T0', ], cols = 6)
epi_head_and_tail(df_recode[df_recode$d_time_cuts_prev == 'T1', ], cols = 6)


# Plot:
i <- 'd_time_cuts_prev'
file_n <- 'plots_bar'
suffix <- 'pdf'
infile_prefix
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
plot_list <- list()
plot_1 <- epi_plot_bar(df_recode, var_x = 'd_time_cuts_prev')
plot_list[[i]] <- plot_1
my_plot_grid <- epi_plots_to_grid(plot_list = plot_list)
epi_plot_cow_save(file_name = outfile, plot_grid = my_plot_grid)


# Clean up a bit:
data_f <- df_recode
rm(list = c('df_recode'))
str(data_f)
###
############


############
###
# Sanity check as issues with NAs rows introduced:
df_recode <- data_f

colnames(df_recode)

cols_to_check <- c('ID_REGISTRO',
                   "d_death",
                   "d_days_to_death",
                   "d_time_cuts_INGRESO",
                   "d_time_cuts_DEF",
                   "d_time_cuts_prev"
                   )

dim(data_f)
dim(df_recode)
summary(df_recode[, cols_to_check])
epi_head_and_tail(df_recode[, cols_to_check], cols = 6)
# Looks fine

lapply(cols_to_check, function(x) head(df_recode[, x]))
lapply(cols_to_check, function(x) tail(df_recode[, x]))
# Fine up to here too


# But not when subsetting:
# Looks OK:
epi_head_and_tail(df_recode[df_recode$d_time_cuts_INGRESO == 'pre-T0', cols_to_check], cols = 6)
epi_head_and_tail(df_recode[df_recode$d_time_cuts_INGRESO == 'T0', cols_to_check], cols = 6)
epi_head_and_tail(df_recode[df_recode$d_time_cuts_INGRESO == 'T1', cols_to_check], cols = 6)

# Not OK:
epi_head_and_tail(df_recode[df_recode$d_time_cuts_DEF == 'pre-T0', cols_to_check], cols = 6)
epi_head_and_tail(df_recode[df_recode$d_time_cuts_DEF == 'T0', cols_to_check], cols = 6)
epi_head_and_tail(df_recode[df_recode$d_time_cuts_DEF == 'T1', cols_to_check], cols = 6)


# Not OK:
epi_head_and_tail(df_recode[df_recode$d_time_cuts_prev == 'pre-T0', cols_to_check], cols = 6)
epi_head_and_tail(df_recode[df_recode$d_time_cuts_prev == 'T0', cols_to_check], cols = 6)
epi_head_and_tail(df_recode[df_recode$d_time_cuts_prev == 'T1', cols_to_check], cols = 6)


# TO DO: needs checking, same issue as above:
df_recode[df_recode$d_time_cuts_DEF == 'pre-T0', ]
tail(df_recode[df_recode$d_time_cuts_DEF == 'pre-T0', ])

tail(df_recode[df_recode$d_time_cuts_DEF == 'pre-T0', ], n = 10)
# Last 7 rows are NAs
tail(df_recode[df_recode$d_time_cuts_DEF == 'T0', ], n = 10)
tail(df_recode[df_recode$d_time_cuts_DEF == 'T1', ], n = 10)
# Different IDs for those without NAs but last 7 rows in each period are NAs

# TO DO: check
# for (i in levels(df_recode$d_time_cuts_prev)) {
#     print(sprintf('Period: %s', i))
#     # 
#     print("everybody for the period:")
#     prev_cut <- nrow(df_recode[df_recode$d_time_cuts_prev == i, ])
#     print(prev_cut)
#     cat('\n')
#     # 
#     print("everybody who died in the period:")
#     outcome_Y <- nrow(df_recode[df_recode$d_time_cuts_DEF == i, ])
#     print(outcome_Y)
#     cat('\n')
#     # 
#     print("everybody who was alive in the period:")
#     outcome_N <- nrow(df_recode[df_recode$d_time_cuts_DEF == 'censored' &
#                                     df_recode$d_time_cuts_prev == i, ]
#                       )
#     print(outcome_N)
#     cat('\n')
#     # This should match:
#     print(prev_cut == outcome_Y + outcome_N)
#     
#     # 
#     print("everybody who died in the period should match with overall outcome (end of study follow-up):")
#     print(nrow(df_recode[df_recode$d_time_cuts_DEF == i & df_recode$d_death == 1, ]))
#     cat('\n')
#     # 
#     print("but should be different to those who were still alive at the particular period:")
#     print(nrow(df_recode[df_recode$d_time_cuts_DEF == 'censored' & df_recode$d_death == 0, ]))
#     cat('\n')
#     cat('\n')
# }
# 
# 
# 
# 
# summary(df_recode[df_recode$d_time_cuts_DEF == 'T0', ])
# dim(df_recode[df_recode$d_time_cuts_DEF == 'T0', ]) # equals number of people who died in this period
# dim(df_recode[df_recode$d_time_cuts_INGRESO == 'T0', ]) # equals number of people at risk (survivors and non-survivors) in this period
# 
# epi_head_and_tail(df_recode[, c('d_time_cuts_INGRESO','d_time_cuts_DEF')], cols = 2)
# summary(df_recode[, c('d_time_cuts_INGRESO','d_time_cuts_DEF')])
# table(df_recode$d_time_cuts_INGRESO, df_recode$d_time_cuts_DEF)
# # d_time_cuts_DEF includes those who were admitted before e.g. T0 start date as it's those who died during specified period.
# 
# # but need an outcome variable for each period for regression with 0's and 1's:
###
############


############
###
# Add outcome variables for each period
# This is based on FECHA_DEF not (!) FECHA_INGRESO for each period
# so if e.g. df_recode$d_time_cuts_DEF == 'pre-T0' death occurred in that period, if 'censored' then was alive at that period.
levels(df_recode$d_time_cuts_DEF)

df_recode$d_pre_T0_DEF <- ifelse(is.na(df_recode$d_death), NA, # true NAs
                         ifelse(df_recode$d_time_cuts_DEF == 'pre-T0', 1,
                                0
                                ))
df_recode$d_T0_DEF <- ifelse(is.na(df_recode$d_death), NA, # true NAs
                         ifelse(df_recode$d_time_cuts_DEF == 'T0', 1,
                                0
                                ))
df_recode$d_gap_T0_T1_DEF <- ifelse(is.na(df_recode$d_death), NA, # true NAs
                         ifelse(df_recode$d_time_cuts_DEF == 'gap_T0_T1', 1,
                                0
                                ))
df_recode$d_T1_DEF <- ifelse(is.na(df_recode$d_death), NA, # true NAs
                         ifelse(df_recode$d_time_cuts_DEF == 'T1', 1,
                                0
                                ))
df_recode$d_gap_T1_T2_DEF <- ifelse(is.na(df_recode$d_death), NA, # true NAs
                         ifelse(df_recode$d_time_cuts_DEF == 'gap_T1_T2', 1,
                                0
                                ))
df_recode$d_T2_DEF <- ifelse(is.na(df_recode$d_death), NA, # true NAs
                         ifelse(df_recode$d_time_cuts_DEF == 'T2', 1,
                                0
                                ))
df_recode$d_post_T2_DEF <- ifelse(is.na(df_recode$d_death), NA, # true NAs
                         ifelse(df_recode$d_time_cuts_DEF == 'post-T2', 1,
                                0
                                ))

colnames(df_recode)
deaths_periods_list <- c("d_pre_T0_DEF", "d_T0_DEF",
                         "d_gap_T0_T1_DEF", "d_T1_DEF",
                         "d_gap_T1_T2_DEF", "d_T2_DEF",
                         "d_post_T2_DEF")
lapply(df_recode[, deaths_periods_list], function(x) summary(as.factor(x)))
dim(df_recode)


summary(df_recode[, c('d_time_cuts_INGRESO','d_time_cuts_DEF', 'd_T0_DEF')])
epi_head_and_tail(df_recode[, c('d_time_cuts_INGRESO','d_time_cuts_DEF', 'd_T0_DEF')], cols = 3)
epi_head_and_tail(df_recode[df_recode$d_T0_DEF == 1, c('d_time_cuts_INGRESO','d_time_cuts_DEF', 'd_T0_DEF')], cols = 3)

# So:
colnames(df_recode)
df_recode$d_time_cuts_prev # for point prevalence, has FECHA_INGRESO and DEF coded
df_recode$d_time_cuts_INGRESO # admitted at particular time period
df_recode$d_time_cuts_DEF # suffered event at particular time period; coded as factor
df_recode$d_T1_DEF # event for this period coded as 0's and 1's; i.e. 1's for period, 0's everything else

# Clean up a bit:
data_f <- df_recode
rm(list = c('df_recode'))
str(data_f)
###
############


############
###
# Intervention variable
# entidad_um "Identifica la entidad donde se ubica la unidad medica que brindó la atención."
dplyr::glimpse(data_f$ENTIDAD_UM)
summary(data_f$ENTIDAD_UM)

# Labels:
interv_labels <- '../data/raw/diccionario_datos_covid19/COISS_states.csv'
interv_labels <- epi_read(interv_labels)
epi_head_and_tail(interv_labels, cols = 4)

data_interv <- data_f %>% left_join(interv_labels[, c(2, 4)],
                                    by = c('ENTIDAD_UM'),
                                    )

# Means 'intervention' variable has been added
# Add 'd_' for quick look up later if many more derived variables are created:
data_interv$d_intervention <- data_interv$intervention
data_interv$intervention <- NULL
str(data_interv)

summary(as.factor(data_interv$d_intervention))
data_interv$d_intervention <- factor(data_interv$d_intervention,
                                   levels = c('non-COISS', 'COISS', 'other'),
                                   ordered = TRUE
                                   )
summary(data_interv$d_intervention)
str(data_interv)


# Clean up:
data_f <- data_interv
rm(list = c('data_interv'))
ls()
###
############


############
###
# Plot:
i <- 'd_intervention'
file_n <- 'plots_bar'
suffix <- 'pdf'
infile_prefix
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
plot_list <- list()
plot_1 <- epi_plot_bar(data_f, var_x = 'd_intervention')
plot_list[[i]] <- plot_1
my_plot_grid <- epi_plots_to_grid(plot_list = plot_list)
epi_plot_cow_save(file_name = outfile, plot_grid = my_plot_grid)
###
############


############
# Exclude 'others' (CDMX, EdoMex)
epi_head_and_tail(data_f)
colnames(data_f)
summary(data_f$d_intervention)

data_f_COISS <- data_f[which(!as.character(data_f$d_intervention) == 'other'), ]
dim(data_f)
dim(data_f_COISS)
summary(data_f$d_intervention)
summary(data_f$d_intervention)[1] + summary(data_f$d_intervention)[2]
summary(data_f_COISS$d_intervention)

# Drop level as now 0:
data_f_COISS$d_intervention <- epi_clean_drop_zero_levels_vector(factor_var = data_f_COISS$d_intervention)
summary(data_f_COISS$d_intervention)

# Remove objects, keep objects names so that code can be re-run as several DBs:
data_f <- data_f_COISS
rm(list = c('data_f_COISS'))
############



############
# The end:
# Save objects, to eg .RData file:
folder <- '../data/processed'
script <- '2_df_subset'
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
