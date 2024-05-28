############
# COISS paper 1
# L. Bonifaz
# May 2024
# Multiple variable regression setup
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
perc_needed <- 0.10
source('/Users/antoniob/Documents/work/science/devel/github/antoniojbt/episcout/R/sub_sampling.R')
ls()
data_f_sub <- data_f_sub # to remove RStudio warnings

sum(data_f_sub$d_death_30)

# Should now have data_f_sub:
epi_head_and_tail(data_f_sub)
table(data_f[[outcome_var]])
table(data_f_sub[[outcome_var]])
prop.table(table(data_f[[outcome_var]]))
prop.table(table(data_f_sub[[outcome_var]]))
############


############
# Set up and loop

###
# df <- data_f_sub
df <- data_f_sub
df

# Total pop at time point:
summary(df$d_time_cuts_prev)

# Pop admitted at time-point:
summary(df$d_time_cuts_INGRESO)

# Pop suffered event at time-point, factor:
summary(df$d_time_cuts_DEF)

# Pop suffered event at time-point, integer:
deaths_periods_list <- c("d_pre_T0_DEF", "d_T0_DEF",
                         "d_gap_T0_T1_DEF", "d_T1_DEF",
                         "d_gap_T1_T2_DEF", "d_T2_DEF",
                         "d_post_T2_DEF")
lapply(df[, deaths_periods_list], function(x) summary(as.factor(x)))
# 0's look balanced as coded for all if no event at time-period for individual
dim(df)
df[["d_intervention"]]
###


###
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

lapply(df[, covars], summary)
###


###
# Initialize storage for results, already have proportions, tables and chi-sq:
# tables_list <- list()
# props_list <- list()
# chi_tests_list <- list()
glm_results_list <- list()

# Keep track of indices:
count <- 0

# Simple regression:
# covars <- 'd_intervention'

# GLM, no interaction terms:
for (i in levels(df$d_time_cuts_prev)) {
    print(i)
    count <- count + 1
    # by_time_cuts[[i]] <- data_f[data_f$d_time_cuts == i, ]
    sub_df <- df[df$d_time_cuts_prev == i, ]
    outcome_var <- deaths_periods_list[count]
    
    # # Run and save:
    # tabs <- table(sub_df[[outcome_var]], sub_df[['d_intervention']])
    # tables_list[[i]] <- tabs
    # 
    # # Run and save:
    # props <- round(prop.table(tabs), digits = 3)
    # props_list[[i]] <- tabs
    # 
    # # Run and save:
    # chi_res <- chisq.test(sub_df[[outcome_var]], sub_df[['d_intervention']])
    # chi_tests_list[[i]] <- chi_test
    
    # GLM spec:
    mod_spec <- sprintf("%s ~ %s", outcome_var, paste(covars, collapse = " + "))
    mod_spec
    mod_form <- as.formula(mod_spec)
    print(mod_form)
    
    mod <- glm(mod_form,
                   data = sub_df,
                   family = binomial
                   )
    
    # Store:
    glm_summary <- tidy(mod)
    glm_summary$time_cut <- i
    glm_results_list[[i]] <- glm_summary
    # print(summary(mod))
    
}

# Combine GLM results into a single data frame
glm_results_df <- do.call(rbind, glm_results_list)

# Save:
file_n <- 'glm_results_time_points'
suffix <- 'txt'
i <- ''
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
epi_write(file_object = chi_df,
          file_name = outfile
          )
###
############


############
# The end:
# Save objects, to eg .RData file:
folder <- '../data/processed'
script <- '5a_multivars_regs'
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
