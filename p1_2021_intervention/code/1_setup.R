############
# COISS paper 1
# L. Bonifaz
# April 2024
############


############
###
# Done:
# Check exclusion criteria:
  # EDAD <1, >95
  # EdoMex; CDMX
  # Clasif Final 1, 2, 3 only, exclude >=4
  # ? That's it
# Use DGE 2021 data, which covers T0 to end of T2
# Run as independent analyst

# Univariate
  # All, COVID-only
  # Re-run code above for plots and stats but COVID-only
###


###
# TO DOs:
# Q's:
  # There are gaps between dates for start and end for T0, 1 and 2

# Bivariate
  # For COVID-only_COISS-only:
    # COISS vs non-COISS
    # By outcome: survivors vs non-survivors

# Run as sensitivity analyses:
  # Diff in Diff
  # Mixed effects model

# Regressions:
  # Subset data to time cuts; check dates ie T1 vs T0, etc as each 'T' already encompasses start and end dates
  # Reproduce results from paper
  # Run chi2
  # Get standard tables and risk
  # Pretty KM plot
  # Survival model
  # Diff in diff
  # MLM
  # Discuss date cut-off (T2 is truncated)
  # Arrange call to understand COVID data collection and database methods

# Write-up

# Extra:
  # Impute if at random and actual missing data analysis
  # COISS vs non-COISS vs others
  # COVID vs non-COVID
  # Other comparisons?
  # Cross with other data?
  # Join COVID19 DGE databases by year:
    # Clean up duplicates (max date of one year overlaps with min date of next)
    # Clarify how ID variable was created; duplicates?
    # Contact DGE, get methods document for database (collection, creation, maintenance, etc)
  # Additional bivariate:
    # COVID vs non-COVID
    # Gender, age, etc



###
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
# Check classes and convert
epi_clean_count_classes(df = data_f)
epi_head_and_tail(data_f)
epi_head_and_tail(data_f, last_cols = T)
tibble::glimpse(data_f)


# Manually convert:
###
length(unique(data_f$FECHA_DEF))
unique(data_f$FECHA_DEF)
table(data_f$FECHA_DEF)
data_f$FECHA_DEF <- data.table::as.IDate(data_f$FECHA_DEF,
                                         format = "%Y-%m-%d"
                                         )
# This converts invalid dates such as 999-99-99 to NA
# There are true NAs though, e.g. death dates before admission dates (converted to NAs below)
# Converted to NAs in outcome variable
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
# Missing SI_NO labels for int/fact variables; 'SI' = 1, 'NO' = 2
# but recode to 0 and 1's first, run below
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

# Loop through to recode NAs but preserve attributes:
# Function to convert specified factor values to NA
epi_clean_fct_to_na <- function(factor_var, values_to_na) {
  # Ensure the input is a factor:
  if (!is.factor(factor_var)) {
    stop("The input variable is not a factor.")
  }
  
  # Convert specified values to NA:
  levels_to_na <- levels(factor_var) %in% values_to_na
  factor_var[as.character(factor_var) %in% values_to_na] <- NA
  
  # Preserve the factor structure
  levels(factor_var)[levels_to_na] <- NA
  
  return(factor_var)
}

# Apply the function to the factor variable in the data frame
vec_test <- epi_clean_fct_to_na(data_f$SEXO, NA_labels)
summary(vec_test)

# Specify columns to modify:
cols_mod <- colnames(df_ord)[7:40]
str(df_ord)

for (i in cols_mod) {
  df_ord[[i]] <- epi_clean_fct_to_na(df_ord[[i]], NA_labels)
  }

epi_head_and_tail(df_ord)
sum(is.na(data_f$SECTOR))
sum(is.na(df_ord$SECTOR))
dplyr::glimpse(df_ord)
str(df_ord)
summary(df_ord$SEXO)

data_f <- df_ord
rm(list = c('df_ord'))
############


############
# Drop levels with 0 as these are NAs
vec_test <- data_f$SEXO
summary(vec_test)
vec_test <- epi_clean_drop_zero_levels_vector(factor_var = vec_test)
summary(vec_test)

# Loop:
df_cleaned <- data_f

df_cleaned[] <- lapply(df_cleaned, function(column) {
  if (is.factor(column)) {
    epi_clean_drop_zero_levels_vector(column)
  } else {
    column
  }
})

str(df_cleaned)
summary(df_cleaned$SEXO)
epi_head_and_tail(df_cleaned)
summary(df_cleaned)

data_f <- df_cleaned
rm(list = c('df_cleaned'))
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

str(data_f)
# Keep data_f for loading in next scripts
############


############
# Add outcome death variable

# Add outcome variable:
data_f$death <- ifelse(!is.na(data_f$FECHA_DEF), 1, 0)
# data_f$death <- factor(data_f$death, labels = c('yes', 'no'))
# data_f$death <- as.factor(data_f$death)
summary(data_f$death)
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

# Have negative values (!), convert to NA's, done below

# Replace NA time with the maximum follow-up time, for censored data:
# Arbitrary but will use FECHA_ACTUALIZACION as running 2021 to 2022 database
# FECHA_ACTUALIZACION is about 10 days after last death date
# Try last death plus 30 days
# If analysing all (2020 to 2024) could be today's date as collection is ongoing

summary(data_f$FECHA_ACTUALIZACION)
summary(data_f$FECHA_DEF)
last_day <- max(data_f$FECHA_ACTUALIZACION)
last_day

max_followup <- as.numeric(difftime(last_day,
                                    min(data_f$FECHA_INGRESO),
                                    units = "days")
                           )
max_followup
summary(data_f$days_to_death)
data_f$days_to_death[is.na(data_f$days_to_death)] <- max_followup
summary(data_f$days_to_death)
############


############
# Remove non-sensical dates
# Death before admission:
str(data_f)

counts <- length(which(as.Date(data_f$FECHA_DEF) < as.Date(data_f$FECHA_INGRESO)))
counts
# 74 fo 2021 database
counts <- length(which(as.Date(data_f$FECHA_DEF) < as.Date(data_f$FECHA_SINTOMAS)))
counts
# 0 fo 2021 database

# Test and change to NAs in outcome/event variable (death):
df_dates <- data_f
summary(df_dates$FECHA_DEF)
summary(df_dates$death)
summary(as.factor(df_dates$death))
str(df_dates)


length(which(df_dates$FECHA_DEF < df_dates$FECHA_INGRESO))
length(which(!is.na(as.Date(df_dates$FECHA_DEF)) &
  as.Date(df_dates$FECHA_DEF) < as.Date(df_dates$FECHA_INGRESO))
  )
# These last two should match

# Switch dates to Dates as ifelse() is not working:
df_dates$FECHA_ACTUALIZACION <- as.Date(df_dates$FECHA_ACTUALIZACION)
df_dates$FECHA_INGRESO <- as.Date(df_dates$FECHA_INGRESO)
df_dates$FECHA_SINTOMAS <- as.Date(df_dates$FECHA_SINTOMAS)
df_dates$FECHA_DEF <- as.Date(df_dates$FECHA_DEF)
str(df_dates)


df_dates$death <- if_else(!is.na(as.Date(df_dates$FECHA_DEF)) &
                            as.Date(df_dates$FECHA_DEF) < as.Date(df_dates$FECHA_INGRESO),
                          NA_integer_,
                          as.integer(df_dates$death)
                          )
length(which(is.na(df_dates$death)))
summary(as.factor(df_dates$death))
# Should have e.g. 74 NAs
# Massive pain as type coercion wasn't working

# Death before symptoms:
df_dates$death <- if_else(!is.na(as.Date(df_dates$FECHA_DEF)) &
                            as.Date(df_dates$FECHA_DEF) < as.Date(df_dates$FECHA_SINTOMAS),
                          NA_integer_,
                          as.integer(df_dates$death)
                          )

summary(as.factor(df_dates$death))
df_dates$death

head(data_f$FECHA_DEF)
head(df_dates$FECHA_DEF)


data_f <- df_dates
rm(list = c('df_dates'))
str(data_f)
############


############
# Convert all factor variables coded with 1 and 2's to 0 and 1's for regression models
# Not absolutely necessary but makes it easier to interpret, particularly as 2 == 'NO'
# SI_NO labels for int/fact variables; 'SI' = 1, 'NO' = 2

df_recode <- data_f
summary(as.factor(df_recode$MIGRANTE))
vec_test <- ifelse(df_recode$MIGRANTE == 2, 0, df_recode$MIGRANTE)
summary(as.factor(vec_test))

# Check colnames manually as only want SI/NO 1/2 vars:
str(df_recode)
summary(df_recode$TIPO_PACIENTE)
cols_recode <- c('INTUBADO',
                 'NEUMONIA',
                 'EMBARAZO',
                 'HABLA_LENGUA_INDIG',
                 'INDIGENA',
                 'DIABETES',
                 'EPOC',
                 'ASMA',
                 'INMUSUPR',
                 'HIPERTENSION',
                 'OTRA_COM',
                 'CARDIOVASCULAR',
                 'OBESIDAD',
                 'RENAL_CRONICA',
                 'TABAQUISMO',
                 'OTRO_CASO',
                 'TOMA_MUESTRA_LAB',
                 'TOMA_MUESTRA_ANTIGENO',
                 'MIGRANTE',
                 'UCI'
                 )

lapply(df_recode[, cols_recode], summary)

# Recode:
for (i in cols_recode) {
  df_recode[[i]] <- ifelse(df_recode[[i]] == 2, 0, df_recode[[i]])
  }
str(df_recode)
lapply(df_recode[, cols_recode], summary)

# Missing SI_NO labels for int/fact variables; 'SI' = 1, 'NO' = 2; now 0 == 'NO', 1 == 'SI
# Convert to factors:
for (i in cols_recode) {
  df_recode[[i]] <- factor(df_recode[[i]],
                           levels = c(0, 1),
                           labels = c('No', 'Yes')
                           )
  }
str(df_recode)
lapply(df_recode[, cols_recode], summary)
summary(data_f$MIGRANTE)
summary(data_f$UCI)
summary(as.factor(df_recode$UCI))

as.character(summary(as.factor(df_recode$UCI))[2])

stopifnot("Values don't match" =
identical(as.character(summary(data_f$UCI)[2]), # because 2 == No
          as.character(summary(as.factor(df_recode$UCI))[1]) # because 0 == No
          )
)
# Should be TRUE

# Clean up a bit:
data_f <- df_recode
rm(list = c('df_recode'))
str(data_f)
############


############
# The end:
# Save objects, to eg .RData file:
folder <- '../data/processed'
script <- '1_setup'
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
