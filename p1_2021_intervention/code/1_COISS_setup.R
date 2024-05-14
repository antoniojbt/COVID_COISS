############
# COISS paper 1
# L. Bonifaz
# April 2024
############


############
# TO DOs:
# Check exclusion criteria:
  # EDAD <1, >95
  # EdoMex; CDMX
  # Clasif Final 1, 2, 3 only, exclude >=4
  # ?
# Use DGE 2021 data, which covers T0 to end of T2
# Run as independent analyst


# Q's:
  # There are gaps between dates for start and end for T0, 1 and 2

# Run as sensitivity analyses:
  # Diff in Diff
  # Mixed effects model

# Write-up

# Extra:
  # Impute if at random
  # COISS vs non-COISS vs others
  # COVID vs non-COVID
  # Other comparisons?
  # Cross with other data?
  # Join COVID19 DGE databases by year:
    # Clean up duplicates (max date of one year overlaps with min date of next)
    # Clarify how ID variable was created; duplicates?
    # Contact DGE, get methods document for database (collection, creation, maintenance, etc)
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
# Dates are SPSS export, prob seconds from origin date, then days from origin date when to csv.
# max minus min looks like it matches the start and end dates for recruitment/analysis of effectiveness
# September 3 to October 8,  2021 First effectiveness analysis (T1)
# December 19, 2021 to February 28,  2022 Second effectiveness  analysis (T2)
# February 28, 2022 minus September 3, 2021 is ~178 days

# Variable type definitions not working
# Dates also got mangled
# Need to ask for non-SPSS format

# Read data based on 0_COISS_comp_base.R
# Run 2021 database as it covers the period of interest:
COVID19MEXICO2021 <- '../data/raw/datos_abiertos_covid19/COVID19MEXICO2021.zip'
infile_prefix <- 'COVID19MEXICO2021'

# Run code for any COVID database as possible:
data_f <- episcout::epi_read(COVID19MEXICO2021)
epi_head_and_tail(data_f)
str(data_f)
summary(data_f$FECHA_INGRESO)
colnames(data_f)
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

df_ord[, cols_mod] <- lapply(df_ord[, cols_mod],
                             function(x) ifelse(x %in% NA_labels, NA, x)
                             )

epi_head_and_tail(df_ord)
sum(is.na(data_f$SECTOR))
sum(is.na(df_ord$SECTOR))
dplyr::glimpse(df_ord)

data_f <- df_ord
rm(list = c('df_ord'))
############


############
# Check classes and convert
epi_clean_count_classes(df = data_f)
epi_head_and_tail(data_f)
epi_head_and_tail(data_f, last_cols = T)
tibble::glimpse(data_f)

###
# Manually convert:
length(unique(data_f$FECHA_DEF))
unique(data_f$FECHA_DEF)
table(data_f$FECHA_DEF)
data_f$FECHA_DEF <- data.table::as.IDate(data_f$FECHA_DEF,
                                         format = "%Y-%m-%d"
                                         )
###


###
data_f$PAIS_NACIONALIDAD
typeof(data_f$PAIS_NACIONALIDAD)
length(unique(data_f$PAIS_NACIONALIDAD))
unique(data_f$PAIS_NACIONALIDAD)
table(data_f$PAIS_NACIONALIDAD)
data_f$PAIS_NACIONALIDAD <- as.factor(data_f$PAIS_NACIONALIDAD)
###


###
data_f$PAIS_ORIGEN
typeof(data_f$PAIS_ORIGEN)
length(unique(data_f$PAIS_ORIGEN))
unique(data_f$PAIS_ORIGEN)
table(data_f$PAIS_ORIGEN)
data_f$PAIS_ORIGEN <- as.factor(data_f$PAIS_ORIGEN)
###


###
data_f$EDAD
typeof(data_f$EDAD)
length(unique(data_f$EDAD))
unique(data_f$EDAD)
table(data_f$EDAD)
data_f$EDAD <- as.numeric(data_f$EDAD)
length(which(data_f$EDAD > 95))
summary(data_f$EDAD)
# many over 95, max is e.g. >154
dim(data_f)
###


###
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
###


###
# Function to convert true integer columns to factors, excluding IDate:
# TO DO: move to episcout
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
###
############


############
# Use data dictionary to add labels and levels to factor columns
# Add levels and labels
table(data_f$ORIGEN)
var_fact <- data_f$ORIGEN
labels_desc <- c("USMER", "FUERA DE USMER")
# levs <- c(1:2)
# Reassign levels and labels to the factor
var_fact <- factor(var_fact,
                   # levels = ,
                   labels = labels_desc
                   )
table(var_fact)

# Loop:
lookup_df <- '../data/201128_Catalogos_factor_labels.csv'
lookup_df <- epi_read(lookup_df)
epi_head_and_tail(lookup_df)
str(lookup_df)
head(lookup_df$variable)
colnames(lookup_df)

df_fact <- epi_clean_label(data_df = data_f,
                           lookup_df = lookup_df
                           )
str(df_fact)
data_f <- df_fact
rm(list = c('df_fact'))
# TO DO: Missing SI_NO labels for int/fact variables; 'SI' = 1, 'NO' = 2
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
# Keep data_f for laoding in next scripts
############


############
# Add outcome death variable

# Add outcome variable:
data_f$death <- ifelse(!is.na(data_f$FECHA_DEF), 1, 0)
# data_f$death <- factor(data_f$death, labels = c('yes', 'no'))
summary(as.factor(data_f$death))

# Add time to death, will use date of hospitalisation as start date, because
# is more relevant for treatment analysis (COISS vs non)
# is available, looks OK
# reflects recognition of severe disease needing medical intervention
# probably less biased than date of sympton onset
data_f$days_to_death <- as.numeric(difftime(data_f$FECHA_DEF,
                                            data_f$FECHA_INGRESO,
                                            units = "days")
                                   )
summary(data_f$days_to_death)

# TO DO: continue here
# Have negative values (!), convert to NA's:

# Replace NA time with the maximum follow-up time, for censored data:
# Arbitrary but will use FECHA_ACTUALIZACION as running 2021 to 2022 database
# If analysing all (2020 to 2024) could be today's date as collection is ongoing
max_followup <- as.numeric(difftime(max(data_f$FECHA_ACTUALIZACION),
                                    min(data_f$FECHA_INGRESO),
                                    units = "days")
                           )
max_followup
summary(data_f$days_to_death)
summary(data_f$days_to_death[is.na(data_f$days_to_death)])
data_f$days_to_death[is.na(data_f$days_to_death)] <- max_followup
summary(data_f$days_to_death)
############


############
# The end:
# Save one object, to eg .RData file:
# Check and remove objects that are not necessary to save:
ls()
object_sizes <- sapply(ls(), function(x) object.size(get(x)))
object_sizes <- as.matrix(rev(sort(object_sizes))[1:10])
object_sizes

objects_to_save <- (c('data_f', 'infile_prefix'))
file_n <- '../data/processed/1_COISS_setup.rdata.gzip'
save(list = objects_to_save,
     file = file_n,
     compress = 'gzip'
     )
# Remove all:
# rm_list <- 'xx'
rm(list = ls())

# Filename to save current R session (entire workspace) data and objects at the end:
# output_name <- 'xx'
# suffix <- 'xx'
# save_session <- sprintf('%s_%s.RData', output_name, suffix)
# print(sprintf('Saving an R session image as: %s', save_session))
# save.image(file = save_session, compress = 'gzip')

sessionInfo()
# q()

# Next: run the script for xxx
############

