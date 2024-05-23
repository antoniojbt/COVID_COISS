############
# COISS paper 1
# L. Bonifaz
# May 2024
############


############
# TO DO:
  # Run DiD as primary analysis; followed by eg Cox or MLM
  # Check assumptions: independence, expected frequencies, sample size
  # plot/check residuals, R2, etc
  # Easy to digest presentation
  # Competing Risks Analysis?

# Done:
  # needs proper selection of covariates
  # check problems with missing data, exclude vars based on this
  # Create a table of counts, proportions, etc. for simple overview
  # Save tables, see tendency
  # change follow-up to 30 days, replot 4a script with this
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
source('/Users/antoniob/Documents/work/science/devel/github/antoniojbt/episcout/R/sub_sampling.R')

# Should now have data_f_sub:
epi_head_and_tail(data_f_sub)
table(data_f[[outcome_var]])
table(data_f_sub[[outcome_var]])
prop.table(table(data_f[[outcome_var]]))
prop.table(table(data_f_sub[[outcome_var]]))
############


############
# Univariate tests
# Chi-squared test

# By d_intervention:
table(data_f$d_death, data_f$d_intervention)
table(data_f$d_death_30, data_f$d_intervention)
# These will be different as e.g. using follow-up to 30 days will change counts of events occurred by given date

chi_t <- chisq.test(data_f$d_death, data_f$d_intervention)
chi_t$observed
str(chi_t)
chi_t$observed


# By d_intervention and date:
summary(data_f$d_time_cuts)

pre_T0 <- data_f[data_f$d_time_cuts == 'pre_T0', ]
T0 <- data_f[data_f$d_time_cuts == 'T0', ]
gap_T1_T0 <- data_f[data_f$d_time_cuts == 'gap_T1_T0', ]
T1 <- data_f[data_f$d_time_cuts == 'T1', ]
gap_T1_T2 <- data_f[data_f$d_time_cuts == 'gap_T1_T2', ]
T2 <- data_f[data_f$d_time_cuts == 'T2', ]
post_T2 <- data_f[data_f$d_time_cuts == 'post_T2', ]

summary(data_f$d_intervention)
summary(factor(data_f$d_death))


# Vars to test:
# outcome_var <- 'd_death'
outcome_var <- 'd_death_30'

# Initialize list and df for results by time cuts and chi-sq test:
by_time_cuts <- list()
chi_df <- data.frame()

# Loop through each level of d_time_cuts
for (i in levels(data_f$d_time_cuts)) {
    # print(i)
    # Subset time cut:
    t <- data_f[data_f$d_time_cuts == i, ]
    
    # Tables:
    contingency_table <- table(t[[outcome_var]], t$d_intervention)
    prop_table <- round(prop.table(contingency_table), digits = 3)
    
    # Test:
    chi_test <- chisq.test(t[[outcome_var]], t$d_intervention)
    # print(chi_test)
    
    # Save results:
    by_time_cuts[[i]] <- list(
        contingency_table = contingency_table,
        proportions = prop_table,
        chi_test = chi_test
        )
    
    # Append chi-squared:
    chi_df <- rbind(chi_df, data.frame(
        time_cut = i,
        chi_squared = chi_test$statistic,
        p_value = chi_test$p.value,
        df = chi_test$parameter
    ))
    }

# Save:
file_n <- 'chi_squared'
suffix <- 'txt'
i <- outcome_var
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
epi_write(file_object = chi_df,
          file_name = outfile
          )

# Save proportions and contingency tables:
for (i in names(by_time_cuts)) {
    file_n <- 'contingency_table'
    suffix <- 'txt'
    outfile <- sprintf(fmt = '%s/%s_%s_%s.%s', infile_prefix, file_n, i, outcome_var, suffix)
    # outfile
    epi_write(file_object = by_time_cuts[[i]]$contingency_table,
              file_name = outfile
              )
    
    file_n <- 'proportions'
    suffix <- 'txt'
    outfile <- sprintf(fmt = '%s/%s_%s_%s.%s', infile_prefix, file_n, i, outcome_var, suffix)
    # outfile
    epi_write(file_object = by_time_cuts[[i]]$proportions,
              file_name = outfile
    )
    }

print(chi_df)
############


############
# Simple regression setup

# Setup formula:
outcome_var <- 'd_death_30'
mod_spec <- sprintf("%s ~ d_intervention", outcome_var)
mod_spec
model_formula <- as.formula(mod_spec)

# Fit the model
# Use sub-sample:
# df <- data_f
df <- data_f_sub


model_1 <- glm(model_formula,
               data = df, # or e.g. pre-T0
               family = binomial
               )

# Summarize the model
res <- summary(model_1)
res

# # Get predicted probabilities
# predicted_probs <- predict(model_1, type = "response")
# predicted_probs
# 
# # Get predicted classes (0 or 1) using a threshold of 0.5
# predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)
# 
# # View the first few predicted probabilities and classes
# head(predicted_probs)
# head(predicted_classes)
# 
# # Create a confusion matrix
# caret::confusionMatrix(as.factor(predicted_classes), as.factor(data_f$d_death))
# 
# # Plot the ROC curve
# roc_curve <- roc(data_f$d_death, predicted_probs)
# plot(roc_curve)
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
outcome_var <- 'd_death_30'
# outcome_var <- 'd_death'

time_var <- 'd_days_to_death_30'

# For GLMs:
mod_spec_lm <- sprintf("%s ~ %s", outcome_var, paste(covars, collapse = " + "))
mod_spec_lm
mod_form_lm <- as.formula(mod_spec_lm)
mod_form_lm


# For survival analysis:
mod_spec_surv <- sprintf("Surv(%s, %s) ~ %s", time_var, outcome_var, paste(covars, collapse = " + "))
mod_form_surv <- as.formula(mod_spec_surv)
mod_form_surv
############


############
# Multi-variable regressions:

###
# Survival
str(df)
fit <- surv_fit

surv_fit <- survfit(mod_form_surv,
               data = df
               )

surv_fit_plot <- ggsurvplot(fit, data = df, conf.int = TRUE,
                            xlab = "Time (days)", ylab = "Survival Probability",
                            title = "Kaplan-Meier Survival Curves by Time Period"
                            )
# TO DO: as for outcome_var; i.e. re-run 4a but showing adjusted values
###


###
# Log-rank test to compare survival across periods:
surv_diff <- survdiff(mod_form, data = data_f)
surv_diff
str(surv_diff)
###


###
# Cox proportional hazards model:
# TO DO
res <- summary(cox_model)
str(res)
res
###



###
# GLM, no interaction terms:
for (i in levels(data_f$d_time_cuts)) {
    print(i)
    # by_time_cuts[[i]] <- data_f[data_f$d_time_cuts == i, ]
    t <- data_f[data_f$d_time_cuts == i, ]
    print(table(t$d_death, t$d_intervention))
    print(round(prop.table(table(t$d_death, t$d_intervention)), digits = 3))
    print(chisq.test(t$d_death, t$d_intervention))
    model_2 <- glm(mod_form,
                   data = t,
                   family = binomial
    )
    print(summary(model_2))
}
###
############



############
# Basic DiD
# Fit linear model

DiD_formula <- as.formula("d_days_to_death ~ d_intervention +
                                   EDAD +
                                   SEXO +
                                   NACIONALIDAD +
                                   DIABETES +
                                   HIPERTENSION + 
                                   d_intervention * d_time_cuts" # Needs interaction term
                            )

model_DiD <- lm(DiD_formula, data = data_f)

# Summarize model
res_model_DiD <- summary(model_DiD)
res_model_DiD

# Tidy the model output
tidy(res_model_DiD)
############



############
# TO DO:
# Run mixed model
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
# The end:
# Save objects, to eg .RData file:
folder <- '../data/processed'
script <- '6_regressions'
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
