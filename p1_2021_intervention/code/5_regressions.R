############
# COISS paper 1
# L. Bonifaz
# May 2024
# Simple regression setup
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
summary(data_f$d_time_cuts_prev)

# Could just cycle thorugh this but can't remember if code was misbehaving
# Survival models were when in a loop, but moved those out
# Keeping for now
pre_T0 <- data_f[data_f$d_time_cuts_prev == 'pre_T0', ]
T0 <- data_f[data_f$d_time_cuts_prev == 'T0', ]
gap_T1_T0 <- data_f[data_f$d_time_cuts_prev == 'gap_T1_T0', ]
T1 <- data_f[data_f$d_time_cuts_prev == 'T1', ]
gap_T1_T2 <- data_f[data_f$d_time_cuts_prev == 'gap_T1_T2', ]
T2 <- data_f[data_f$d_time_cuts_prev == 'T2', ]
post_T2 <- data_f[data_f$d_time_cuts_prev == 'post_T2', ]

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
               data = df, # or e.g. pre_T0
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
# Initialize storage for results, already have proportions, tables and chi-sq:
# tables_list <- list()
# props_list <- list()
# chi_tests_list <- list()
glm_results_list <- list()


# Keep track of indices:
count <- 0

# Simple regression:
covars <- 'd_intervention'

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
script <- '5_regressions'
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

