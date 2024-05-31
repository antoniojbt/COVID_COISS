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
library(broom)
library(lme4)
library(survival)
library(survminer)
library(caret)
library(pROC)
############


############
# Load a previous R session, data and objects:
# infile <- '../data/processed/4a_surv_outcome_bivariate_COVID19MEXICO2021_COVID-only_COISS-only.rdata.gzip'
infile <- '../data/processed/4a_surv_outcome_bivariate_COVID19MEXICO2021_2022_COVID-only_COISS-only.rdata.gzip'
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
# Run sub_sampling script as taking too long to get regressions
# Test code first with sub-sample:
perc_needed <- 0.01
source('/Users/antoniob/Documents/work/science/devel/github/antoniojbt/episcout/R/sub_sampling.R')
ls()
data_f_sub <- data_f_sub # to remove RStudio warnings


# Vars to test:
# outcome_var <- 'd_death'
outcome_var <- 'd_death_30'
# outcome_var <- '' # these are counts as factor at each time-cut

sum(data_f_sub[[outcome_var]])
head(data_f_sub[[outcome_var]])

# Should now have data_f_sub:
epi_head_and_tail(data_f_sub)
table(data_f[[outcome_var]])
table(data_f_sub[[outcome_var]])
prop.table(table(data_f[[outcome_var]]))
prop.table(table(data_f_sub[[outcome_var]]))
############


############
# DiD setup

###
# By d_intervention:
table(data_f$d_death, data_f$d_intervention)
table(data_f$d_death_30, data_f$d_intervention)
# These will be different as e.g. using follow-up to 30 days will change counts of events occurred by given date

# By d_intervention and date:
summary(data_f$d_time_cuts)
###



############


############
# Simple regression setup

###
# Set-up formula:
mod_spec <- sprintf("%s ~ d_intervention", outcome_var)
mod_spec
model_formula <- as.formula(mod_spec)
model_formula

# Fit the model
# Use sub-sample:
# df <- data_f
df <- data_f_sub[data_f_sub$d_time_cuts_prev == 'T1', ]
df <- data_f[data_f$d_time_cuts_prev == 'T1', ]
df <- data_f[data_f$d_time_cuts_prev == 'T0', ]
summary(df$d_time_cuts_prev) # only T0 and T1 should have >0


model_1 <- glm(model_formula,
               data = df, # or e.g. pre_T0
               family = binomial
               )

# Summarize the model
res1 <- summary(model_1)
str(res1)
res1
###


###
# DiD:
# Set-up formula:
mod_spec <- sprintf("%s ~ d_intervention + d_intervention * d_time_cuts_prev", outcome_var)
mod_spec
model_formula <- as.formula(mod_spec)
model_formula


# Data to test:
colnames(data_f)
summary(data_f$d_time_cuts_prev)
df_T0 <- data_f[data_f$d_time_cuts_prev == 'T0', ]
df_T1 <- data_f[data_f$d_time_cuts_prev == 'T1', ]

df_T0 # TO DO: check as whole rows with NA's
df_T1

# df <- rbind(df_T0, df_T1)
df <- df_T0
df[, 'd_time_cuts_prev']
summary(df$d_time_cuts_prev) # only T0 and T1 should have >0


# Simple DiD:
model_2 <- glm(model_formula,
               data = df, # or e.g. pre_T0
               family = binomial
               )
# Summarize the model
str(model_2)
res <- summary(model_2)
res
###
############


############
# Setup covariates
# Run simple (above) then adjusted for various models

colnames(data_f)
summary(data_f$SECTOR)
summary(data_f$ORIGEN)
summary(data_f)

colnames(data_f)

# Excluded, admin vars:
# "ID_REGISTRO"
# "FECHA_ACTUALIZACION"

# Would correlate:
# "FECHA_DEF"
# "ENTIDAD_UM" # because it determines d_intervention
# "MUNICIPIO_RES" # would correlate with ENTIDAD_UM but need to check
# "ENTIDAD_NAC" # would correlate with ENTIDAD_UM but need to check
# "ENTIDAD_RES" # would correlate with ENTIDAD_UM but need to check
# "d_days_to_death"


# Unsure:
extra_check <- c("FECHA_INGRESO", # surv analysis will account for it
                 "FECHA_SINTOMAS" # need to check db manual
                 )

# Covariates:
covars <- c("d_intervention",
            # "d_time_cuts", # exclude for now as running models for each cut
            "EDAD",
            "ORIGEN",
            "SECTOR",
            "SEXO",
            # "TIPO_PACIENTE", # errors, don't know why
            # "INTUBADO", # high NA's
            "NEUMONIA",
            "NACIONALIDAD",
            # "EMBARAZO", # errors, don't know why; high NA's
            # "HABLA_LENGUA_INDIG", # errors, don't know why
            "INDIGENA",
            "DIABETES",
            "EPOC",
            "ASMA",
            "INMUSUPR",
            "HIPERTENSION",
            "OTRA_COM",
            "CARDIOVASCULAR",
            "OBESIDAD",
            "RENAL_CRONICA",
            "TABAQUISMO",
            "OTRO_CASO"
            # "TOMA_MUESTRA_LAB", # admin var, may correlate
            # "RESULTADO_LAB", # admin var, may correlate
            # "TOMA_MUESTRA_ANTIGENO", # admin var, may correlate
            # "RESULTADO_ANTIGENO", # admin var, may correlate
            # "CLASIFICACION_FINAL", # admin var, may correlate
            # "MIGRANTE", # errors, don't know why; high NA's
            # "PAIS_NACIONALIDAD", # errors, don't know why
            # "PAIS_ORIGEN", # errors, don't know why; high NA's
            # "UCI" # high NA's
            )

# Use sub-sample:
# df <- data_f
df <- data_f_sub

lapply(df[, covars], summary)

# Setup formula:
time_var <- 'd_days_to_death_30'

# For GLMs:
mod_spec_lm <- sprintf("%s ~ %s", outcome_var, paste(covars, collapse = " + "))
mod_spec_lm
mod_form_lm <- as.formula(mod_spec_lm)
mod_form_lm
############


############
# Basic DiD
# Fit linear model

DiD_formula <- as.formula(sprintf("%s ~ %s + d_intervention * d_time_cuts", # Needs interaction term
                                  outcome_var, paste(covars, collapse = " + ")
                                 )
                          )
DiD_formula

model_DiD <- lm(DiD_formula, data = data_f)

# Summarize model
res_model_DiD <- summary(model_DiD)
res_model_DiD

# Tidy the model output
tidy(res_model_DiD)
############


############
# The end:
# Save objects, to eg .RData file:
folder <- '../data/processed'
script <- '5a_DiD'
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
