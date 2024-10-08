############
# COISS paper 1
# L. Bonifaz
# April 2024
# Should run all databases for COVID DGE
# Input is joined databases from 0_COISS_comp_base.R script
# Output is cleaned and labelled data for further analysis:
  # subsetting for this project: COVID+ only, COISS only in script 2_df_subset.R
  # and downstream (descriptive stats, plots, regression, etc.)
############


############
###
# Done:
# Check exclusion criteria:
  # EDAD <1, >95; can use 0 as these are individuals under 1 year old
  # EdoMex; CDMX
  # Clasif Final 1, 2, 3 only, exclude >=4
  # ? That's it
# Use DGE 2021 data, which covers T0 to end of T2
# Joined 2021 and 2022 as key date is FECHA_SINTOMAS, which I think is admission (talked to Ruth Gonzalez)
# Run as independent analyst

# Contacted DGE for methods document for database (collection, creation, maintenance, etc):
  # This is the Manual de Vigilancia Epidemiologica Enfermedades Respiratorias
  # Check anexes
  # SISVER system, sentinel type
  # Subsite lineamientos y manuales
  # Manual de Laboratorio
  # on DGE site
  # Ruth Gonzalez; Laura Flores

# Univariate
  # All, COVID-only
  # Re-run code above for plots and stats but COVID-only
  # Run chi2

# Bivariate
  # For COVID-only_COISS-only:
  # By outcome: survivors vs non-survivors

# Script 4a could be cleaned up but OK for now, worked as exploratory for 2021-2022 databases. If running/joining others then simplify as complication are the T0, T1 T2 windows for analysis.

# Regressions:
  # Simple survival model
  # Discuss date cut-off (T2 is truncated)
  # needs proper selection of covariates
  # check problems with missing data, exclude vars based on this
  # Create a table of counts, proportions, etc. for simple overview
  # Save tables, see tendency
  # change follow-up to 30 days, replot 4a script with this
  # re-check NAs for death at script 4
  # Use 30-day mortality
  # Subset data to time cuts; check dates ie T1 vs T0, etc as each 'T' already encompasses start and end dates
  # Get standard tables and risk  
  # Pretty KM plot  


###


###
# TO DOs:
  # Run DiD as sensitivity analysis:
    # Check assumptions: independence, expected frequencies, sample size
    # plot/check residuals, R2, etc
    # Easy to digest presentation
  # Reproduce results from paper

  # Finish pretty plots:
    # KM by date cut off and intervention
    # plot of counts and proportions by date, outcome, and intervention, see script 3a

# Q's:
  # There are gaps between dates for start and end for T0, 1 and 2
  # Check max symptoms here is after FECHA_ACTUALIZACION
  # Are re-admissions an issue? e.g. violate independence assumption

# Write-up

# Clean-up:
  # Check TO DOs within each script
  # Move any remaining functions to episcout (marked by 'TO DO', otherwise already moved)
  # Update episcout
###


###
# Extra:
  # Impute if at random and actual missing data analysis
  # COISS vs non-COISS vs others
  # COVID vs non-COVID
  # Other comparisons?
  # Cross with other data?
  # Join COVID19 DGE databases by year:
    # Clean up duplicates (max date of one year overlaps with min date of next); there are no duplicates when checking by ID, and shouldn't be as data is medical unit admissions
  
  # Additional bivariate:
    # COVID vs non-COVID
    # Gender, age, etc

  # setup HPC scripts as models take too long
  # Cox or MLM
  # Competing Risks Analysis?  
  # Plan additional questions for full dataset
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
# Old notes from when exploring processed and raw datasets:
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
# COVID19MEXICO2021 <- '../data/raw/datos_abiertos_covid19/COVID19MEXICO2021.zip'
# data_f <- '../data/processed/COVID19MEXICO2021_2022.rdata.gzip'
# infile_prefix <- 'COVID19MEXICO2021_2022'

# Run code for any COVID database as possible:
# data_f <- episcout::epi_read(data_f)
############


############
# Load a previous R session, data and objects:
# infile <- '../data/processed/1_setup_COVID19MEXICO2021.rdata.gzip'
infile <- '../data/processed/0_comp_bases_COVID19MEXICO_2021_2022.rdata.gzip'
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
############


############
# Find non-unique IDs
df_dups <- data_f
dups <- epi_clean_get_dups(df_dups, var = "ID_REGISTRO")
dups
# View(t(dups))

# Get indices:
unique_dup_IDs <- unique(dups$ID_REGISTRO)
unique_dup_IDs
inds <- which(as.character(df_dups$ID_REGISTRO) %in% as.character(unique_dup_IDs))
inds
df_dups[inds, ]

# Add a row index:
dups$inds <- inds
dups
dups[, c('ID_REGISTRO', 'inds')] 

# Keep the one with the most recent FECHA_ACTUALIZACION:
dups_keep <- dups %>%
  group_by(ID_REGISTRO) %>%
  filter(FECHA_ACTUALIZACION == max(FECHA_ACTUALIZACION))
dups_keep
dups_keep[, c('ID_REGISTRO', 'inds')] 

# Macth IDs to original data:
inds_keep <- dups_keep$inds
inds_keep
df_dups <- df_dups[-inds_keep, ]

# Check:
nrow(data_f) - nrow(df_dups) == nrow(dups_keep)
dups <- epi_clean_get_dups(df_dups, var = "ID_REGISTRO")
dups # should be zero

# Clean up:
data_f <- df_dups
rm(list = c('df_dups'))
############


############
# Check classes and convert
epi_clean_count_classes(df = data_f)
epi_head_and_tail(data_f)
epi_head_and_tail(data_f, last_cols = T)
tibble::glimpse(data_f)


# Manually convert:
###
# Keep copies of original date variables (as character) as can be a headache to manage once transformed for derived vars:

date_cols <- c('FECHA_ACTUALIZACION',
               'FECHA_INGRESO',
               'FECHA_SINTOMAS',
               'FECHA_DEF'
               )
lapply(data_f[, date_cols], summary)
lapply(data_f[, date_cols], typeof)
str(data_f[, date_cols])

data_f$FECHA_ACTUALIZACION_char <- as.character(data_f$FECHA_ACTUALIZACION)
data_f$FECHA_INGRESO_char <- as.character(data_f$FECHA_INGRESO)
data_f$FECHA_SINTOMAS_char <- as.character(data_f$FECHA_SINTOMAS)
data_f$FECHA_DEF_char <- as.character(data_f$FECHA_DEF)

date_cols_char <- c('FECHA_ACTUALIZACION_char',
                    'FECHA_INGRESO_char',
                    'FECHA_SINTOMAS_char',
                    'FECHA_DEF_char'
                    )
lapply(data_f[, date_cols_char], summary)
lapply(data_f[, date_cols_char], typeof)
str(data_f[, date_cols_char])
###


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
epi_head_and_tail(data_f[, char_cols], cols = 7)

# Explicitly check the type of each column:
sapply(data_f, typeof)
# IDates are stored under the hood as integers
###


###
# Function to convert true integer columns to factors, excluding IDate:
epi_clean_int_to_factor <- function(dt) { # dt as it's a data.table object
    for (col in names(dt)) {
        # Check if the column is integer and not an IDate:
        if (is.integer(dt[[col]]) && !inherits(dt[[col]], "IDate")) {
            dt[[col]] <- as.factor(dt[[col]])
        }
    }
    dt
}

# Convert int cols to factors:
data_f <- epi_clean_int_to_factor(data_f)

# Check the changes
str(data_f)
# Looks good

summary(data_f)
dim(data_f)
###
############


############
# Use data dictionary to add labels and levels to factor columns

###
# Add levels and labels
table(data_f$ORIGEN)
var_fact <- data_f$ORIGEN
labels_desc <- c("USMER", "FUERA DE USMER")
# Unidades Monitoras de Enfermedad Respiratoria Viral (USMER) 
# levs <- c(1:2)
# Reassign levels and labels to the factor
var_fact <- factor(var_fact,
                   # levels = ,
                   labels = labels_desc
                   )
table(var_fact)

# Loop:
lookup_df <- '../data/processed/var_labels_etc/201128_Catalogos_factor_labels.csv'
lookup_df <- epi_read(lookup_df)
epi_head_and_tail(lookup_df)
str(lookup_df)
head(lookup_df$variable)
colnames(lookup_df)
###

###
# Function here as haven't updated episcout:
epi_clean_label <- function(data_df, lookup_df) {
    # Ensure the lookup dataframe is in the correct format
    lookup_df <- lookup_df %>%
        mutate(level = as.character(level))  # Convert keys to character if not already
    
    # Iterate over each variable in the lookup table
    for (v in unique(lookup_df$variable)) {
        if (v %in% names(data_df)) {
            # Get levels and labels for this variable
            levels_and_labels <- lookup_df %>%
                filter(variable == eval(v)) %>% # This errors, as does !!sym(v)
                select(level, label) %>%
                arrange(as.numeric(level))  # Ensure levels are in the correct order
            
            # Convert the relevant column in data_df to a factor
            data_df[[v]] <- factor(data_df[[v]],
                                   levels = levels_and_labels$level,
                                   labels = levels_and_labels$label)
        }
    }
    
    return(data_df)
}
###


###
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
dim(df_ord)
cols_mod <- which(sapply(df_ord, is.factor))
cols_mod <- names(cols_mod)
cols_mod
str(df_ord[, cols_mod])


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

###
# Also copying function here until I update episcout:
epi_clean_drop_zero_levels_vector <- function(factor_var) {
    # Ensure the input is a factor
    if (!is.factor(factor_var)) {
        stop("The input variable is not a factor.")
    }
    
    # Get the levels that are present in the factor
    present_levels <- levels(factor_var)[table(factor_var) > 0]
    
    # Drop levels that are zero
    cleaned_factor <- factor(factor_var, levels = present_levels)
    
    return(cleaned_factor)
}
###


###
vec_test <- data_f$SEXO
summary(vec_test)
vec_test <- epi_clean_drop_zero_levels_vector(factor_var = vec_test)
summary(vec_test)

# To loop:
df_cleaned <- data_f
dim(df_cleaned)
###

###
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
dim(df_cleaned)

data_f <- df_cleaned
rm(list = c('df_cleaned'))
###
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

# Lots of zeros, maybe under one year olds, keep as these are explicitly coded as <1 year olds:
# data_f$EDAD <- ifelse(data_f$EDAD < 1, NA, data_f$EDAD)
sum(is.na(data_f$EDAD))
summary(data_f$EDAD)
length(which(data_f$EDAD < 1))
length(which(data_f$EDAD == 0))

str(data_f)
# Keep data_f for loading in next scripts
############


############
###
# Remove non-sensical dates and add new variables
# Death before admission
# Test and change to NAs in outcome/event variable (death):
df_dates <- data_f
summary(df_dates$FECHA_DEF)
str(df_dates)

counts <- length(which(as.Date(df_dates$FECHA_DEF) < as.Date(df_dates$FECHA_INGRESO)))
counts
# 74 fo 2021 database; 84 for 2021-2022
counts <- length(which(as.Date(df_dates$FECHA_DEF) < as.Date(df_dates$FECHA_SINTOMAS)))
counts
# 0 fo 2021 database; 1 for 2021-2022


length(which(df_dates$FECHA_DEF < df_dates$FECHA_INGRESO))
length(which(!is.na(as.Date(df_dates$FECHA_DEF)) &
  as.Date(df_dates$FECHA_DEF) < as.Date(df_dates$FECHA_INGRESO))
  )
# These last two should match

# Switch dates to Dates as ifelse() coercion won't work below:
df_dates$FECHA_ACTUALIZACION <- as.Date(df_dates$FECHA_ACTUALIZACION)
df_dates$FECHA_INGRESO <- as.Date(df_dates$FECHA_INGRESO)
df_dates$FECHA_SINTOMAS <- as.Date(df_dates$FECHA_SINTOMAS)
df_dates$FECHA_DEF <- as.Date(df_dates$FECHA_DEF)
str(df_dates)
dim(df_dates)

# Dates with NAs (9999-99-99) stay as NAs but introduce NAs in outcome variable:
# Add outcome death variable
df_dates$d_death <- ifelse(!is.na(df_dates$FECHA_DEF), 1, 0)
df_dates$d_death <- as.integer(df_dates$d_death)
str(df_dates$d_death)
str(df_dates)

summary(df_dates$d_death)
summary(as.factor(df_dates$d_death))

# Introduce NAs for non-sensical dates:
df_dates$d_death <- if_else(!is.na(as.Date(df_dates$FECHA_DEF)) &
                            as.Date(df_dates$FECHA_DEF) < as.Date(df_dates$FECHA_INGRESO),
                          NA_integer_,
                          as.integer(df_dates$d_death)
                          )
length(which(is.na(df_dates$d_death)))
summary(as.factor(df_dates$d_death))
# Should have e.g. 74 NAs; 84 for 2021 with 2022
# Note no NAs for EDAD and very few for non-sensical dates
# Massive pain as type coercion wasn't working

# Death before symptoms:
df_dates$d_death <- if_else(!is.na(as.Date(df_dates$FECHA_DEF)) &
                            as.Date(df_dates$FECHA_DEF) < as.Date(df_dates$FECHA_SINTOMAS),
                          NA_integer_,
                          as.integer(df_dates$d_death)
                          )

summary(as.factor(df_dates$d_death))
head(data_f$FECHA_DEF)
head(df_dates$FECHA_DEF)
dim(df_dates)

# Clean up a bit:
str(df_dates)

data_f <- df_dates
rm(list = c('df_dates'))
str(data_f)
###


###
# Add time to d_death, will use date of hospitalisation as start date, because
# is more relevant for treatment analysis (COISS vs non)
# is available, looks OK
# reflects recognition of severe disease needing medical intervention
# probably less biased than date of symptom onset
# Note: check max symptoms is after FECHA_ACTUALIZACION
# Use 'd_' as suffix for new/derived variables

data_f <- df_dates

summary(data_f$FECHA_SINTOMAS)
summary(data_f$FECHA_ACTUALIZACION)


df_dates$d_days_to_death <- as.numeric(difftime(time1 = df_dates$FECHA_DEF,
                                              time2 = df_dates$FECHA_INGRESO,
                                              units = "days")
                                   )
summary(df_dates$d_days_to_death)
length(which(df_dates$d_days_to_death < 0))
# Has negative values (!), convert to NA's
df_dates$d_days_to_death <- ifelse(df_dates$d_days_to_death < 0, NA, df_dates$d_days_to_death)
summary(df_dates$d_days_to_death)
summary(df_dates$FECHA_DEF)
length(which(df_dates$d_days_to_death < 0))


# Replace NA time with the maximum follow-up time, for censored data:
# Arbitrary but will use FECHA_ACTUALIZACION as running 2021 to 2022 database
# FECHA_ACTUALIZACION is about 10 days after last death date
# Try last death plus 30 days
# If analysing all (2020 to 2024) could be today's date as collection is ongoing
# Keep as is but explore further in later scripts using eg 30 day mortality; would need censoring data after that period though.

summary(df_dates$FECHA_ACTUALIZACION)
summary(df_dates$FECHA_DEF)
last_day <- max(df_dates$FECHA_ACTUALIZACION)
last_day

max_followup <- as.numeric(difftime(last_day,
                                    min(df_dates$FECHA_INGRESO),
                                    units = "days")
                           )
max_followup
summary(df_dates$d_days_to_death)
df_dates$d_days_to_death[is.na(df_dates$d_days_to_death)] <- max_followup
summary(df_dates$d_days_to_death)

# Check:
summary(df_dates$d_days_to_death)
summary(df_dates$FECHA_DEF)
summary(as.factor(df_dates$d_death))

# Clean up:
data_f <- df_dates
rm(list = c('df_dates'))
str(data_f)
###


###
# Re-censor and add outcome variables for different periods as follow-up time is too long if based on FECHA_INGRESO

df_dates <- data_f

# Check code above and clean up for df_dates object
# Define new censoring time
censoring_time <- 30 # for new cut-off for follow-up time, e.g. 30 days

# Re-introduce new censoring:
df_dates$d_death_30 <- ifelse(df_dates$d_days_to_death > censoring_time, 0, df_dates$d_death)
df_dates$d_days_to_death_30 <- pmin(df_dates$d_days_to_death, censoring_time)

df_dates$d_death_30 <- as.integer(df_dates$d_death_30)

summary(df_dates$d_death_30)
summary(df_dates$d_days_to_death_30)
table(df_dates$d_days_to_death_30)

# Clean up:
data_f <- df_dates
rm(list = c('df_dates'))
str(data_f)
###
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
# Re-censor with new variable:
# Done in script 4_surv_outcome.R
# Because corresponds more to survival analysis setup than here (i.e. outcome definition depends on follow-up definition: end of study, 30-day mortality)
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
