############
# COISS
# xxx
# Abril 2024
# Laura B. y Octavio
############

############
# TO DOs
# Analysis plan
# maybe switch to Rmd so that there is a narrative with code
# TO DO: run sensitivity analyses:
# imputed, complete cases
# row deletion if outlier in any variable
# ?
# TO DO: check exclusion criteria; maybe only EDAD
# TO DO: impute, analyse if at random, could impute 10 - 16 (ie <15% missing)
# Dig out code
############


############
project_loc <- '/Users/antoniob/Documents/work/science/projects/projects/ongoing/laura_bonifaz/estatregia_COVID_COISS/results/'
getwd()
setwd(project_loc)
############

############
# Import libraries
library(data.table)
# source functions from a different R script:
#source(file.path(Rscripts_dir, 'moveme.R')) #, chdir = TRUE)
library(episcout)
library(Hmisc)
library(ggthemes)
library(cowplot)
library(GGally)
library(broom)
library(openxlsx)
library(tidyverse)
library(lme4)
############

############
# Dates are SPSS export, prob seconds from origin date, then days from origin date when to csv.
# max minus min looks like it matches the start and end dates for recruitment/analysis of effectiveness
# September 3 to October 8,  2021 First effectiveness analysis (T1)
# December 19, 2021 to February 28,  2022 Second effectiveness  analysis (T2)
# February 28, 2022 minus September 3, 2021 is ~178 days

# Variable type definitions not working
# Dates also got mangled
# Need to ask for non-SPSS format

data_f <- '../data/datos_abiertos_DGEpi/COVID19MEXICO.csv'
# data_f <- '../data/Rubi/Poblacion_COVID19_RRR.csv'
# data_f <- '../data/Rubi/BaseCoiss_23mayo.txt'
# data_f <- read_sav(data_f)

data_f <- episcout::epi_read(data_f)

epi_head_and_tail(data_f)
# View(data_f)
colnames(data_f)
# epi_write(data_f, file_name = '../data/BaseCoiss_23mayo.txt')
############

############
# Check database codes and convert to NAs:
# Need to set labels with NA codes
table(data_f$SECTOR)
NA_labels <- c('97', '98', '99', '997', '998', '999', '36')
vec_test <- ifelse(data_f$SECTOR %in% NA_labels, NA, data_f$SECTOR)
table(vec_test)
length(which(is.na(vec_test)))
sum(is.na(vec_test))
sum(is.na(data_f$SECTOR))

# Loop all, exclude date and char cols though:
epi_clean_count_classes(data_f)
# 1 char, 4 date
dplyr::glimpse(data_f)

data_f %>%
    select_if(~ epi_clean_cond_chr_fct(.)) %>%
    colnames()

data_f %>%
    select_if(~ epi_clean_cond_numeric(.)) %>%
    colnames()

# Re-order columns to make it easier to select:
df_ord <- data_f %>%
    select('ID_REGISTRO',
           matches("^FECHA"),
           'EDAD',
           everything()
    )

colnames(df_ord)
dplyr::glimpse(df_ord)
str(df_ord)
dim(df_ord)

# Loop through to recode NAs:
# Specify columns to modify
cols_mod <- colnames(df_ord)[7:40]
str(df_ord)

# Loop through
# # fct get recoded to int with ifelse:
# df_recoded <- df_recoded %>%
#     mutate(across(cols_mod,
#                   ~ if_else(. %in% NA_labels,
#                             NA_character_,
#                             as.character(.)),
#                   .names = "{.col}_mod"
#     )
#     ) %>%
#     mutate(across(ends_with("_mod"),
#                   factor
#     )
#     )

df_ord[, cols_mod] <- lapply(df_ord[, cols_mod],
                             function(x) ifelse(x %in% NA_labels, NA, x)
)

epi_head_and_tail(df_ord)
sum(is.na(data_f$SECTOR))
sum(is.na(df_ord$SECTOR))
dplyr::glimpse(df_ord)

data_f <- df_ord
############

############
# Check classes and convert
epi_clean_count_classes(df = data_f)
epi_head_and_tail(data_f)
epi_head_and_tail(data_f, last_cols = T)
tibble::glimpse(data_f)


# Manually convert:
length(unique(data_f$FECHA_DEF))
unique(data_f$FECHA_DEF)
table(data_f$FECHA_DEF)
data_f$FECHA_DEF <- data.table::as.IDate(data_f$FECHA_DEF,
                                         format = "%Y-%m-%d"
)

data_f$PAIS_NACIONALIDAD
typeof(data_f$PAIS_NACIONALIDAD)
length(unique(data_f$PAIS_NACIONALIDAD))
unique(data_f$PAIS_NACIONALIDAD)
table(data_f$PAIS_NACIONALIDAD)
data_f$PAIS_NACIONALIDAD <- as.factor(data_f$PAIS_NACIONALIDAD)

data_f$PAIS_ORIGEN
typeof(data_f$PAIS_ORIGEN)
length(unique(data_f$PAIS_ORIGEN))
unique(data_f$PAIS_ORIGEN)
table(data_f$PAIS_ORIGEN)
data_f$PAIS_ORIGEN <- as.factor(data_f$PAIS_ORIGEN)

data_f$EDAD
typeof(data_f$EDAD)
length(unique(data_f$EDAD))
unique(data_f$EDAD)
table(data_f$EDAD)
data_f$EDAD <- as.numeric(data_f$EDAD)
length(which(data_f$EDAD > 95))
summary(data_f$EDAD)
# 1488 over 95, max is 154
dim(data_f)

epi_clean_count_classes(df = data_f)
tibble::glimpse(data_f)
str(data_f)

char_cols <- data_f %>%
    select_if(~ epi_clean_cond_chr_fct(.)) %>%
    colnames()
char_cols
epi_head_and_tail(data_f[, char_cols], cols = 3)

# Explicitly check the type of each column:
sapply(data_f, typeof)
# IDates are stored under the hood as integers

# Function to convert true integer columns to factors, excluding IDate:
convert_int_to_factor <- function(dt) { # dt as it's a data.table object
    for (col in names(dt)) {
        # Check if the column is integer and not an IDate:
        if (is.integer(dt[[col]]) && !inherits(dt[[col]], "IDate")) {
            dt[[col]] <- as.factor(dt[[col]])
        }
    }
    dt
}

# Convert int cols to factors:
data_f <- convert_int_to_factor(data_f)

# Check the changes
str(data_f)
# Looks good

summary(data_f)
dim(data_f)
############


############
# TO DO:
# Use data dictionary to add labels and levels to factor columns
# Go back to already processed set and run with dates as is. Keep it simple!
############


############
# Outlier exclusion:
sum(is.na(data_f$EDAD))
summary(data_f$EDAD)
length(which(data_f$EDAD > 90))
data_f[which(data_f$EDAD > 120), "EDAD"]
# Exclude e.g. >95 years old?

data_f$EDAD <- ifelse(data_f$EDAD > 95, NA, data_f$EDAD)
sum(is.na(data_f$EDAD))
summary(data_f$EDAD)
length(which(data_f$EDAD > 95))

length(which(data_f$EDAD < 1))
length(which(data_f$EDAD == 0))

# Lots of zeros, maybe under one year olds, exclude as ambiguous:
data_f$EDAD <- ifelse(data_f$EDAD < 1, NA, data_f$EDAD)
sum(is.na(data_f$EDAD))
summary(data_f$EDAD)
length(which(data_f$EDAD < 1))
length(which(data_f$EDAD == 0))

dplyr::glimpse(data_f)
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
# 27
nrow(na_perc[na_perc$na_perc > 5, ])
# 8
nrow(na_perc[na_perc$na_perc > 15, ])
# 8

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
table(complete.cases(data_f[,
                            which(!colnames(data_f) %in% cols_miss)
])
)
# About 10% have a missing entry once problem variables are excluded

epi_write(na_perc,
          'na_perc.txt',
          row.names = TRUE,
          col.names = TRUE
)
############


############
# Summary statistics
epi_clean_count_classes(data_f)
dplyr::glimpse(data_f)
str(data_f)

# Numeric
stats_num <- epi_stats_summary(df = data_f$EDAD, class_type = 'int_num')
stats_num

# Numeric data summary doesn't need tidying but could be formatted:
stats_num <- epi_stats_format(stats_num, digits = 2)
# View(stats_num)
# EDAD is only numeric variable

# Save as table
# pwd already in results folder:
file_n <- 'sum_stats.txt'
epi_write(file_object = stats_num, file_name = file_n)

# Factor
col_facts <- data_f[, c(7:40)]
stats_fct <- epi_stats_summary(df = col_facts, class_type = 'chr_fct')
stats_fct
# View(stats_fct)

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
file_n <- 'COISS_02052024/stats_factors.txt'
epi_write(file_object = stats_fct_tidy,
          file_name = file_n
)

str(data_f)
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
sum_dates_df <- data.frame(character(0),
                           numeric(0),
                           numeric(0),
                           numeric(0),
                           numeric(0),
                           numeric(0),
                           numeric(0)
)
colnames(sum_dates_df) <- c('variable',
                            'Min',
                            '25%',
                            'Median',
                            '75%',
                            'Max',
                            'IQR'
)
# sum_dates_df

for (i in col_dates) {
    sum_dates <- epi_stats_dates(data_f[[i]])
    # print(sum_dates)
    sum_dates <- as.data.frame(cbind(i,
                                     t(sum_dates[, "Value"])
    )
    )
    # sum_dates
    sum_dates_df <- rbind(sum_dates_df,
                          sum_dates)
}
sum_dates_df
file_n <- 'COISS_02052024/desc_dates.txt'
epi_write(sum_dates_df, file_n)
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
dates_list$FECHA_INGRESO

# Save:
for (i in names(dates_list)) {
    print(i)
    file_n <- epi_output_name()
}


epi_write()
############


############
# TO DO: continue here
# Plot dates

############


############
# # Run mixed model
# mem <- lmer(lethality ~ ESTRATEGIA + edad + SEXO + COMORBILIDADES +
#                 (1 | entidad_um),
#             data = data_f)
#
# summary(mem)
# plot(residuals(mem))

# Predict:
# new_data <- data.frame(fixedEffect = new_fixed_values, randomEffect = new_random_values)
# predictions <- predict(mem, newdata = new_data, re.form = NA)  # re.form = NA to exclude random effects
############


############
#


############


############
q()
############
