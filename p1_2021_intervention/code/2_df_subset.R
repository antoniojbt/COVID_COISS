############
# COISS paper 1
# L. Bonifaz
# April 2024

# This script is specific to COISS vs non-COISS 2021-2022 COVID positive analysis
# Input is output from script 1_setup.R
# Output is tsv and rdata files for descriptive stats and further analysis
############


############
# New variables
# Time cuts for admission
# Time cuts for outcome
# Time cuts for Case Fatality Rate:
# Denominator: Deaths from the disease during the period
# Numerator: Diagnosed cases treated during the period, deaths during the period and ongoing treatments from previous periods

# For CFR, period prev, etc.:
  # pop who suffers event during period = FECHA_DEF during period : covers admitted before and during who suffer event
  # pop at risk during period =   FECHA_INGRESO during period or FECHA_DEF during period: covers alive and events during period
                            #   + FECHA_INGRESO before period but suffer event after period : covers admitted before who remain alive through period and need to be censored
                            #   + FECHA_INGRESO before period who do not suffer event, remain alive during period and are discharged alive at any point should be included but CANNOT as there is no variable (or combination for this; i.e. no discharge variable other than death date)
                            #   This means numbers are overestimated as missing counts for the denominator when studying specific dates. For whole study, follow-up is long, so not an issue as highly unlikely to remain in hospital and alive for many months for COVID19.

# For CFR, prevalence/period inclusion, period/time cuts are not continuous/dependent, i.e. each window analysis is independent and will thus include admissions from previous windows if those admissions died or remained alive during current period
# so that DiD can be performed on e.g. T1 accounting for T0, and T2 accounting for T0

# derived variables:
  # "d_death" = event occurred up until maximum follow-up time, FECHA_ACTUALIZACION; integer
  # "d_days_to_death" = days from admission to event; FECHA_INGRESO to FECHA_DEF; numeric
  # "d_time_cuts_INGRESO" = time cuts based on admission date; factor ordered
  # "d_time_cuts_DEF" = time cuts based on outcome date; factor ordered
  # "d_time_cuts_prev" = time cuts based on period, including admissions and events; factor ordered
  # "d_intervention_T1" = COISS intervention at T1; integer
  # "d_intervention_T2" = COISS intervention at T2; integer

# For regressions:
  # subset based on d_time_cuts_prev, which will be pop at risk for period
  # outcome var will then be (as integers):
    # "d_pre_T0_outcome"
    # "d_T0_outcome"
    # "d_gap_T0_T1_outcome"
    # "d_T1_outcome"
    # "d_gap_T1_T2_outcome"
    # "d_T2_outcome"
    # "d_post_T2_outcome"
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
############


############
###
# Time cuts for admission
# The following is based on FECHA_INGRESO:
df_recode <- data_f

length(which(as.Date(df_recode$FECHA_INGRESO) >= as.Date(T0_start)))
length(which(df_recode$FECHA_INGRESO < T0_start))
any(is.na(df_recode$FECHA_INGRESO))
# There are no NA's for 2021 and 2022 databases

summary(df_recode$FECHA_INGRESO)
summary(df_recode$FECHA_INGRESO_char)
dim(df_recode)
# Looks like for 2021 database, once COVID-only cases are kept, the last date is before the T2 cut-off.

summary(df_recode$FECHA_DEF)
summary(df_recode$FECHA_ACTUALIZACION)
summary(df_recode$FECHA_SINTOMAS)

df_recode$d_time_cuts_INGRESO <- 
                      ifelse(df_recode$FECHA_INGRESO >= T0_start & df_recode$FECHA_INGRESO <= T0_end, 'T0',
                          ifelse(df_recode$FECHA_INGRESO >= T1_start & df_recode$FECHA_INGRESO <= T1_end, 'T1',
                              ifelse(df_recode$FECHA_INGRESO >= T2_start & df_recode$FECHA_INGRESO <= T2_end, 'T2',
                                  ifelse(df_recode$FECHA_INGRESO < T0_start, 'pre_T0',
                                      ifelse(df_recode$FECHA_INGRESO > T0_end & df_recode$FECHA_INGRESO < T1_start, 'gap_T0_T1',
                                          ifelse(df_recode$FECHA_INGRESO > T1_end & df_recode$FECHA_INGRESO < T2_start, 'gap_T1_T2',
                                              ifelse(df_recode$FECHA_INGRESO > T2_end, 'post_T2',
                                                     'check'
                                                     )))))))

dim(df_recode)
summary(factor(df_recode$d_time_cuts_INGRESO))
df_recode$d_time_cuts_INGRESO
df_recode$d_time_cuts_INGRESO <- factor(df_recode$d_time_cuts_INGRESO,
                           levels = c('pre_T0', 'T0',
                                      'gap_T0_T1', 'T1',
                                      'gap_T1_T2', 'T2',
                                      'post_T2'),
                           # labels = levels,
                           ordered = TRUE
                           )
summary(df_recode$d_time_cuts_INGRESO)

# Looks OK:
epi_head_and_tail(df_recode[df_recode$d_time_cuts_INGRESO == 'pre_T0', ], cols = 6)
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

# df_recode$d_time_cuts_DEF <- NULL
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
      is.na(d_time_cuts_DEF) & FECHA_DEF < T0_start ~ 'pre_T0',
      is.na(d_time_cuts_DEF) & FECHA_DEF >= T0_start & FECHA_DEF <= T0_end ~ 'T0',
      is.na(d_time_cuts_DEF) & FECHA_DEF > T0_end & FECHA_DEF < T1_start ~ 'gap_T0_T1',
      is.na(d_time_cuts_DEF) & FECHA_DEF >= T1_start & FECHA_DEF <= T1_end ~ 'T1',
      is.na(d_time_cuts_DEF) & FECHA_DEF > T1_end & FECHA_DEF < T2_start ~ 'gap_T1_T2',
      is.na(d_time_cuts_DEF) & FECHA_DEF >= T2_start & FECHA_DEF <= T2_end ~ 'T2',
      is.na(d_time_cuts_DEF) & FECHA_DEF > T2_end ~ 'post_T2',
      TRUE ~ 'check'  # Default case if none of the above match
    )
  )

dim(df_recode)
dim(data_f)
summary(factor(df_recode$d_time_cuts_DEF))
epi_head_and_tail(df_recode[df_recode$d_time_cuts_DEF == 'pre_T0', ], cols = 6)
# View(tail(df_recode[df_recode$d_time_cuts_DEF == 'pre_T0', ], n = 100))
subset_pre_T0 <- df_recode[df_recode$d_time_cuts_DEF == 'pre_T0', ]
summary(factor(subset_pre_T0$d_time_cuts_DEF)) # should only be 'pre_T0'

# Check if there are rows where all points are NA:
subset_pre_T0[rowSums(is.na(subset_pre_T0)) == ncol(subset_pre_T0), ] # should be empty
# Fine up to here
###


###
# Just keep true_NA as character
# Returning 'True_NAs' placeholder back to actual NA caused havoc with the recoding, introducing rows with NA's across when subsetting.
# True NA's are from non-sensical dates coded in original (max follow-up) outcome var (d_death)
# This and other tries re-introduce rows of NA's. They seem to be additional rows that couldn't be handled though.
# This appears to solely be when subsetting and not in the actual dataframe.
# Check separate script trying to debug this 'subset_NA_rows_issue.R'

# df_recode2 <- df_recode %>%
#   mutate(
#     d_time_cuts_DEF = case_when(
#         d_time_cuts_DEF == 'true_NA' ~ NA_character_, # Handle true NAs
#         TRUE ~ d_time_cuts_DEF  # Retain all other rows
#     )
#   )
# 
# nrow(df_recode2) == nrow(data_f)
# summary(factor(df_recode2$d_time_cuts_DEF))
# 
# # Subsetting introduces NA's but these aren't present in full dataset:
# epi_head_and_tail(df_recode2[df_recode2$d_time_cuts_DEF == 'pre_T0', ], cols = 6)
# # View(tail(df_recode2[df_recode2$d_time_cuts_DEF == 'pre_T0', ], n = 100))
# subset_pre_T0 <- df_recode2[df_recode2$d_time_cuts_DEF == 'pre_T0', ]
# summary(factor(subset_pre_T0$d_time_cuts_DEF)) # should only be 'pre_T0'
# # Check if there are rows where all points are NA:
# subset_pre_T0[rowSums(is.na(subset_pre_T0)) == ncol(subset_pre_T0), ] # should be empty but isn't
# 
# # Not OK if subsetting when actual NAs are used:
# epi_head_and_tail(df_recode2[df_recode2$d_time_cuts_DEF == 'pre_T0', ], cols = 6)
# epi_head_and_tail(df_recode2[df_recode2$d_time_cuts_DEF == 'T0', ], cols = 6)
# epi_head_and_tail(df_recode2[df_recode2$d_time_cuts_DEF == 'T1', ], cols = 6)

# But OK if string is used:
epi_head_and_tail(df_recode[df_recode$d_time_cuts_DEF == 'pre_T0', ], cols = 6)
epi_head_and_tail(df_recode[df_recode$d_time_cuts_DEF == 'T0', ], cols = 6)
epi_head_and_tail(df_recode[df_recode$d_time_cuts_DEF == 'T1', ], cols = 6)

names(summary(factor(df_recode$d_time_cuts_DEF)))
df_recode$d_time_cuts_DEF <- factor(df_recode$d_time_cuts_DEF,
                                 levels = c('pre_T0', 'T0',
                                            'gap_T0_T1', 'T1',
                                            'gap_T1_T2', 'T2',
                                            'post_T2', 'censored',
                                            "true_NA"),
                                 ordered = TRUE
                                 )

summary(df_recode$d_time_cuts_DEF)
summary(df_recode$d_time_cuts_INGRESO) # no NA's for 2021 and 2022 databases
###

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
data_f <- df_recode
rm(list = c('df_recode'))
str(data_f)
###
############


############
###
# Time cuts for point prevalence
# Outcome death within each time-cut, needed for DiD analysis (as transversal, not long. as with days to death)
# Should equal outcome var (e.g. max follow-up with FECHA_ACTUALIZACION, or 30 day mortality from admission) plus d_time_cuts_DEF which has mark for death at each time period
# Should be equal to point prevalence, where all events during the period over whole population admitted (ie prior admissions still there plus new admission for the period)
# See definitions above for pop at risk

df_recode <- data_f

colnames(df_recode)
summary(df_recode$d_time_cuts_INGRESO)
summary(df_recode$d_time_cuts_DEF)

# df_recode$d_time_cuts_prev <- NULL
df_recode$d_time_cuts_prev <- character(length = nrow(df_recode))
df_recode$d_time_cuts_prev <- 
  ifelse(is.na(as.character(df_recode$d_death)), 'true_NA', # if using e.g. NA_character_ NA rows get introduced when subsetting
    # FECHA_INGRESO during period or FECHA_DEF during period:
  ifelse((df_recode$d_time_cuts_INGRESO == 'pre_T0' | df_recode$d_time_cuts_DEF == 'pre_T0'),
    # There is no pre-pre_T0, so no prior admissions to this point
    'pre_T0',
    # FECHA_INGRESO during period or FECHA_DEF during period:
  ifelse((df_recode$d_time_cuts_INGRESO == 'T0' | df_recode$d_time_cuts_DEF == 'T0') |
    # FECHA_INGRESO before period but suffer event after period:
         (df_recode$FECHA_INGRESO < T0_start & df_recode$FECHA_DEF > T0_end),
    'T0',
    # FECHA_INGRESO during period or FECHA_DEF during period:
  ifelse((df_recode$d_time_cuts_INGRESO == 'gap_T0_T1' | df_recode$d_time_cuts_DEF == 'gap_T0_T1') |
    # FECHA_INGRESO before period but suffer event after period:
         (df_recode$FECHA_INGRESO < T0_end & df_recode$FECHA_DEF > T1_start),
    'gap_T0_T1',
    # FECHA_INGRESO during period or FECHA_DEF during period:
  ifelse((df_recode$d_time_cuts_INGRESO == 'T1' | df_recode$d_time_cuts_DEF == 'T1') |
    # FECHA_INGRESO before period but suffer event after period:
         (df_recode$FECHA_INGRESO < T1_start & df_recode$FECHA_DEF > T1_end),
    'T1',
    # FECHA_INGRESO during period or FECHA_DEF during period:
    ifelse((df_recode$d_time_cuts_INGRESO == 'gap_T1_T2' | df_recode$d_time_cuts_DEF == 'gap_T1_T2') |
    # FECHA_INGRESO before period but suffer event after period:
           (df_recode$FECHA_INGRESO < T1_end & df_recode$FECHA_DEF > T2_start),
           'gap_T1_T2',
    # FECHA_INGRESO during period or FECHA_DEF during period:
    ifelse((df_recode$d_time_cuts_INGRESO == 'T2' | df_recode$d_time_cuts_DEF == 'T2') |
    # FECHA_INGRESO before period but suffer event after period:
           (df_recode$FECHA_INGRESO < T2_start & df_recode$FECHA_DEF > T2_end),
           'T2',
    # FECHA_INGRESO during period or FECHA_DEF during period:
    ifelse((df_recode$d_time_cuts_INGRESO == 'post_T2' | df_recode$d_time_cuts_DEF == 'post_T2'),
    # There is no post-post_T2, so no events after this point
           'post_T2',
    # Everything else (should be none):
           'check'
           ))))))))

dim(df_recode)
dim(data_f)
summary(factor(df_recode$d_time_cuts_prev))
names(summary(factor(df_recode$d_time_cuts_prev)))

# Var as factor:
df_recode$d_time_cuts_prev <- factor(df_recode$d_time_cuts_prev,
                                   levels = c('pre_T0', 'T0',
                                            'gap_T0_T1', 'T1',
                                            'gap_T1_T2', 'T2',
                                            'post_T2', 'true_NA'),
                                 ordered = TRUE
                                 )
summary(df_recode$d_time_cuts_prev)

# Same issue as above when subsetting only with NA's, so left as character
epi_head_and_tail(df_recode[df_recode$d_time_cuts_prev == 'pre_T0', ], cols = 6)
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
# Add outcome variables for each period
# This is based on FECHA_DEF not (!) FECHA_INGRESO for each period
# so if e.g. df_recode$d_time_cuts_DEF == 'pre_T0' death occurred in that period, if 'censored' then was alive at that period.
# Had same issues as before, ifelse() really can't handle NA's or statements coming from different column types

df_recode <- data_f

colnames(df_recode)
deaths_periods_list <- c("d_pre_T0_outcome", "d_T0_outcome",
                         "d_gap_T0_T1_outcome", "d_T1_outcome",
                         "d_gap_T1_T2_outcome", "d_T2_outcome",
                         "d_post_T2_outcome")

period_subsets <- levels(df_recode$d_time_cuts_DEF)
period_subsets <- period_subsets[1:7] # remove extras
period_subsets

# Create a var for each period:
# i <- 'd_pre_T0_outcome'
count <- 0
for (i in deaths_periods_list) {
  print(i)
  count <- count + 1
  df_recode[[i]] <- character(length = nrow(df_recode))
  df_recode[[i]] <- ifelse(is.na(as.character(df_recode$d_death)), 'NA', # true NAs
                                ifelse(as.character(df_recode$d_time_cuts_DEF) == as.character(period_subsets[[count]]), '1',
                                       '0'
                                       ))
  # df_recode[[i]] <- as.integer(df_recode[[i]]) # This introduces rows with NAs when subsetting
  print(typeof(df_recode[[i]]))
  print(summary(as.integer(df_recode[[i]])))
  print(summary(as.factor(df_recode[[i]])))
  print(epi_head_and_tail(df_recode[df_recode[[i]] == 1,
                              c('ID_REGISTRO', 'd_time_cuts_INGRESO',
                                'd_time_cuts_DEF', 'd_pre_T0_outcome')],
                    cols = 4
                    ))

  }

lapply(df_recode[, deaths_periods_list], function(x) summary(as.factor(x)))
dim(df_recode)

# Not all time cuts get NAs introduced, pre_T0 did, check:
summary(df_recode[, c('ID_REGISTRO', 'd_time_cuts_INGRESO','d_time_cuts_DEF', 'd_pre_T0_outcome')])
epi_head_and_tail(df_recode[, c('ID_REGISTRO', 'd_time_cuts_INGRESO','d_time_cuts_DEF', 'd_pre_T0_outcome')], cols = 4)
epi_head_and_tail(df_recode[df_recode$d_pre_T0_outcome == 1, c('ID_REGISTRO', 'd_time_cuts_INGRESO','d_time_cuts_DEF', 'd_pre_T0_outcome')], cols = 4)

# Check:
pre_T0 <- df_recode[df_recode$d_time_cuts_prev == 'pre_T0', ]
epi_head_and_tail(pre_T0) # looks OK
summary(as.factor(pre_T0$d_pre_T0_outcome)) # very few NA's in 2021 and 2022 so may not appear in this subset
pre_T0$d_pre_T0_outcome <- as.integer(pre_T0$d_pre_T0_outcome)
summary(pre_T0$d_pre_T0_outcome)
epi_head_and_tail(df_recode[df_recode$d_pre_T0_outcome == 1, ]) # looks OK
epi_head_and_tail(df_recode[df_recode$d_pre_T0_outcome == 0, ]) # looks OK

# But:
# # df_recode$d_pre_T0_outcome <- as.integer(df_recode$d_pre_T0_outcome) #  introduces rows with NAs when subsetting
# epi_head_and_tail(df_recode[, c('ID_REGISTRO', 'd_time_cuts_INGRESO','d_time_cuts_DEF', 'd_pre_T0_outcome')], cols = 4)
# epi_head_and_tail(df_recode[as.character(df_recode$d_pre_T0_outcome) == '1', c('ID_REGISTRO', 'd_time_cuts_INGRESO','d_time_cuts_DEF', 'd_pre_T0_outcome')], cols = 4)

# So subset, recode var, run regressions

# Clean up a bit:
data_f <- df_recode
rm(list = c('df_recode'))
str(data_f)
###
############


############
###
# Intervention variables, there are two. One for T1 and one for T2 as non-COISS states changed over time.
# entidad_um "Identifica la entidad donde se ubica la unidad medica que brindó la atención."
dplyr::glimpse(data_f$ENTIDAD_UM)
summary(data_f$ENTIDAD_UM)

data_interv <- data_f

# Labels for T1:
interv_labels <- '../data/processed/var_labels_etc/COISS_states_T1.csv'
interv_labels <- epi_read(interv_labels)
epi_head_and_tail(interv_labels, cols = 4)
# print(interv_labels, n = 32)

data_interv <- data_interv %>% dplyr::left_join(interv_labels[, c(2, 4)],
                                           by = c('ENTIDAD_UM'),
                                          )

# Means 'intervention' variable has been added
# Add 'd_' for quick look up later if many more derived variables are created:
# Add that these are for T1 analysis
data_interv$d_intervention_T1 <- data_interv$intervention
data_interv$intervention <- NULL
str(data_interv)

summary(as.factor(data_interv$d_intervention_T1))
data_interv$d_intervention_T1 <- factor(data_interv$d_intervention_T1,
                                   levels = c('non-COISS', 'COISS', 'other')
                                   )
summary(data_interv$d_intervention_T1)
str(data_interv)
dim(data_interv)

# Labels for T2:
interv_labels <- '../data/processed/var_labels_etc/COISS_states_T2.csv'
interv_labels <- epi_read(interv_labels)
epi_head_and_tail(interv_labels, cols = 4)

data_interv <- data_interv %>% dplyr::left_join(interv_labels[, c(2, 4)],
                                           by = c('ENTIDAD_UM'),
                                          )

# Means 'intervention' variable has been added
# Add 'd_' for quick look up later if many more derived variables are created:
# Add that these are for T2 analysis
data_interv$d_intervention_T2 <- data_interv$intervention
data_interv$intervention <- NULL
str(data_interv)

summary(as.factor(data_interv$d_intervention_T2))
data_interv$d_intervention_T2 <- factor(data_interv$d_intervention_T2,
                                   levels = c('non-COISS', 'COISS', 'other')
                                   )
summary(data_interv$d_intervention_T2)
str(data_interv)
dim(data_interv)

# Check, COISS should be the same number:
summary(data_interv$d_intervention_T1)[2] == summary(data_interv$d_intervention_T2)[2]


# Clean up:
data_f <- data_interv
rm(list = c('data_interv'))
ls()
###


###
# Plot T1:
i <- 'd_intervention_T1'
file_n <- 'plots_bar'
suffix <- 'pdf'
infile_prefix
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
plot_list <- list()
plot_1 <- epi_plot_bar(data_f, var_x = 'd_intervention_T1')
plot_list[[i]] <- plot_1
my_plot_grid <- epi_plots_to_grid(plot_list = plot_list)
epi_plot_cow_save(file_name = outfile, plot_grid = my_plot_grid)
###

###
# Plot T2:
i <- 'd_intervention_T2'
file_n <- 'plots_bar'
suffix <- 'pdf'
infile_prefix
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
plot_list <- list()
plot_1 <- epi_plot_bar(data_f, var_x = 'd_intervention_T2')
plot_list[[i]] <- plot_1
my_plot_grid <- epi_plots_to_grid(plot_list = plot_list)
epi_plot_cow_save(file_name = outfile, plot_grid = my_plot_grid)
###
############


############
# Can't exclude 'other' rows in d_intervention_T1 
# and
# d_intervention_T2 as effectively have two datasets now and would remove all admissions from excluded states at either T1 analysis or T2.
# Exclude at each analysis separately


# # Exclude 'others' (CDMX, EdoMex), there are two intervention variables
# 
# data_f_COISS <- data_f
# 
# epi_head_and_tail(data_f_COISS)
# colnames(data_f_COISS)
# summary(data_f_COISS$d_intervention_T1)
# summary(data_f_COISS$d_intervention_T2)
# 
# 
# data_f_COISS <- data_f_COISS[which(!as.character(data_f_COISS$d_intervention_T1) == 'other'), ]
# data_f_COISS <- data_f_COISS[which(!as.character(data_f_COISS$d_intervention_T2) == 'other'), ]
# dim(data_f)
# dim(data_f_COISS)
# 
# summary(data_f_COISS$d_intervention_T1)
# summary(data_f_COISS$d_intervention_T2)
# # summary(data_f$d_intervention)[1] + summary(data_f$d_intervention)[2]
# 
# 
# # Drop level as now 0:
# # Also copying function here until I update episcout:
# epi_clean_drop_zero_levels_vector <- function(factor_var) {
#     # Ensure the input is a factor
#     if (!is.factor(factor_var)) {
#         stop("The input variable is not a factor.")
#     }
#     
#     # Get the levels that are present in the factor
#     present_levels <- levels(factor_var)[table(factor_var) > 0]
#     
#     # Drop levels that are zero
#     cleaned_factor <- factor(factor_var, levels = present_levels)
#     
#     return(cleaned_factor)
# }
# 
# data_f_COISS$d_intervention_T1 <- epi_clean_drop_zero_levels_vector(factor_var = data_f_COISS$d_intervention_T1)
# data_f_COISS$d_intervention_T2 <- epi_clean_drop_zero_levels_vector(factor_var = data_f_COISS$d_intervention_T2)
# summary(data_f_COISS$d_intervention_T1)
# summary(data_f_COISS$d_intervention_T2)
# 
# # Remove objects, keep objects names so that code can be re-run as several DBs:
# data_f <- data_f_COISS
# rm(list = c('data_f_COISS'))
############


############
dim(data_f)
str(data_f)
############


############
# Save full dataset to disk as tsv
folder <- '../data/processed'
script <- '2_df_subset'
infile_prefix
suffix <- 'tsv.gzip'
outfile <- sprintf(fmt = '%s/%s_%s.%s', folder, script, infile_prefix, suffix)
outfile
epi_write(data_f,
          outfile,
          compress = 'gzip')
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
objects_to_save <- c('data_f', 'infile_prefix', 'outfile')

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
