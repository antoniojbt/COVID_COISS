############
# COISS paper 1
# L. Bonifaz
# May 2024
# Simple regression set-up

# Input is output from script 2_df_subset.R
# Output are various tables from simple regression analysis for the full dataset and for time cuts/windows, no rdata or full dataset.
############


############
project_loc <- '/Users/antoniob/Documents/work/science/projects/projects/ongoing/laura_bonifaz/COVID_COISS/p1_2021_intervention/results/'
getwd()
setwd(project_loc)
############


############
# Save screen outputs:
sink("COVID19MEXICO_2021_2022_COVID-only_COISS-only/sink_output_5_regressions.R.txt")
getwd()
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
library(epitools)
############


############
# Load a previous R session, data and objects:
infile <- '../data/processed/2_df_subset_COVID19MEXICO_2021_2022_COVID-only_COISS-only.rdata.gzip'
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

###
# By d_intervention:
table(data_f$d_death, data_f$d_intervention)
table(data_f$d_death_30, data_f$d_intervention)
# These will be different as e.g. using follow-up to 30 days will change counts of events occurred by given date

contingency_table <- table(data_f$d_death, data_f$d_intervention)
contingency_table
prop_table <- round(prop.table(contingency_table), digits = 3)
prop_table


chi_t <- chisq.test(data_f$d_death, data_f$d_intervention)
str(chi_t)
chi_t
chi_t$observed
chi_t$expected
chi_t$observed
# More deaths in COISS than expected, and significant
# Formally:
  # The Chi-squared test result indicates a statistically significant association between the outcome variable and the intervention variable, death through-out the study period and whether the medical unit carried out COISS measures. The very low p-value (< 0.05) suggests that the differences in the distribution of outcomes between the COISS and non-COISS groups are unlikely to have occurred by chance alone at time point T0. The proportions and contingency table show that the difference is substantial and statistically significant.

# Magnitude and direction of association:
# Odds ratio
odds_ratio <- epitools::oddsratio(contingency_table)
odds_ratio
odds_ratio$measure
# Formally:
  # There is a significant difference outcomes between the COISS and non-COISS states
  # COVID19+ individuals attended in COISS units (states) had a 26.3% higher odds of dying compared to non-COISS states.
  # The result is highly statistically significant with a narrow 95% CI, indicating high confidence in the precision of the estimate
  # Values are un-adjusted for other factors and cover admissions for 2021 and 2022
###
############


############
###
# By d_intervention and date:
summary(data_f$d_time_cuts_prev)

# Subset manually
# Survival models were when in a loop, but moved those out
# Keeping for now
# TO DO: not actually using these in chi-sq, check others too

# Get windows for analysis / time periods, but exclude NAs ('true_NA' is a placeholder):
time_cuts <- levels(data_f$d_time_cuts_prev)
time_cuts <- time_cuts[time_cuts != 'true_NA']
time_cuts

pre_T0 <- data_f[data_f$d_time_cuts_prev == 'pre_T0', ]
T0 <- data_f[data_f$d_time_cuts_prev == 'T0', ]
gap_T0_T1 <- data_f[data_f$d_time_cuts_prev == 'gap_T0_T1', ]
T1 <- data_f[data_f$d_time_cuts_prev == 'T1', ]
gap_T1_T2 <- data_f[data_f$d_time_cuts_prev == 'gap_T1_T2', ]
T2 <- data_f[data_f$d_time_cuts_prev == 'T2', ]
post_T2 <- data_f[data_f$d_time_cuts_prev == 'post_T2', ]

df_time_cuts <- list(pre_T0, T0, gap_T0_T1, T1, gap_T1_T2, T2, post_T2)
names(df_time_cuts) <- time_cuts
names(df_time_cuts)
df_time_cuts

lapply(df_time_cuts, dim)

summary(data_f$d_intervention)
summary(factor(data_f$d_death))
epi_head_and_tail(pre_T0)
###
############


############
# Chi-squared tests

###

# Vars to test:
# outcome_var <- 'd_death'
outcome_var <- 'd_death_30'
summary(factor(data_f[[outcome_var]]))

# Initialize list and df for results by time cuts and chi-sq test:
by_time_cuts <- list()
chi_df <- data.frame()

# Loop through each level of d_time_cuts_prev
for (i in levels(data_f$d_time_cuts_prev)) {
    # print(i)
    # Subset time cut:
    t <- data_f[data_f$d_time_cuts_prev == i, ]
    
    # Tables:
    contingency_table <- table(t[[outcome_var]], t$d_intervention)
    prop_table <- round(prop.table(contingency_table), digits = 3)
    
    # Test, if two levels (eg subset may have no events):
    if(nrow(contingency_table) > 1) {
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
                } else {
                    print(i)
                   print("At least two levels needed for chi-squared test. Missing events possibly.")
            }
    }
###


###
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
###


###
# Explore results by time cut:
chi_df
names(by_time_cuts)
by_time_cuts[['pre_T0']]
# The association between the outcome and the intervention variables is highly significant

by_time_cuts[['T0']]
# The association between the outcome and the intervention variables is highly significant

by_time_cuts[['gap_T0_T1']]
# The association between the outcome and the intervention variables is highly significant

by_time_cuts[["T1"]]
# The association between the outcome and the intervention variables is significant at 0.002

by_time_cuts[["gap_T1_T2"]]
# The association between the outcome and the intervention variables is highly significant

by_time_cuts[["T2"]]
# The association between the outcome and the intervention variables is highly significant

by_time_cuts[["post_T2"]]
# The association between the outcome and the intervention variables is highly significant
# Needs direction, magnitude, CI.
###
############


############
# Odds ratio by time cut for each time cut

###
# Vars to test:
outcome_var <- 'd_death'
# outcome_var <- 'd_death_30'
summary(factor(data_f[[outcome_var]]))

intervention_var <- 'd_intervention'
summary(factor(data_f[[intervention_var]]))


# Whole set:
contingency_table <- table(data_f[[outcome_var]], data_f$d_intervention)
contingency_table

odds_ratio <- epitools::oddsratio(contingency_table)
odds_ratio
str(odds_ratio)
odds_ratio$data
odds_ratio$measure
odds_ratio$p.value
###


###
# Save all results from OR estimates
# Function to calculate and extract odds ratio components
# TO DO: move to episcout, hardcoded vars though
epi_stats_odds_ratio <- function(df, outcome_var, intervention_var) {
  # Construct the contingency table:
  contingency_table <- table(df[[outcome_var]], df[[intervention_var]])
  # Calculate odds ratio:
  odds_ratio <- epitools::oddsratio(contingency_table)
  
  # Extract the required components:
  results <- list(
      data = odds_ratio$data,
      measure = odds_ratio$measure,
      p_value = odds_ratio$p.value
      )
  return(results)
}

# List of dfs by time-cut:
names(df_time_cuts)
subset_df <- df_time_cuts[['pre_T0']]
or_test <- epi_stats_odds_ratio(subset_df,
                                outcome_var,
                                intervention_var
                                )

str(or_test)
or_test
###


###
# List for results:
results_list <- list()

# Subsets
time_cuts

# Loop through subsets and calculate odds ratios:
for (i in time_cuts) {
    subset_df <- df_time_cuts[[i]]
    or_test <- epi_stats_odds_ratio(subset_df,
                                    outcome_var,
                                    intervention_var
                                    )

    # or_test
    # Store results
    # ID subset:
    or_test$subset_id <- i
    results_list[[i]] <- or_test
    }

results_list
results_list$pre_T0
results_list$pre_T0$subset_id
results_list$pre_T0$data
results_list$pre_T0$measure
results_list$pre_T0$p_value

results_list$pre_T0$data[1, "non-COISS"]
results_list[1][[1]]

# Convert the list of results to a data frame with all values from odds_ratio$xx objects:
results_df <- do.call(rbind, lapply(results_list, function(x) {
  data.frame(
    subset_id = x$subset_id,
    non_COISS_0 = x$data[1, "non-COISS"], # not saving totals
    non_COISS_1 = x$data[2, "non-COISS"],
    COISS_0 = x$data[1, "COISS"],
    COISS_1 = x$data[2, "COISS"],
    OR_estimate = x$measure[2, "estimate"], # not saving reference group
    OR_lower = x$measure[2, "lower"],
    OR_upper = x$measure[2, "upper"],
    p_value = x$p_value[2, "chi.square"] # not saving Fisher's p-value
  )
}))

results_df

# Save to file:
file_n <- 'odds_ratios'
suffix <- 'txt'
i <- outcome_var
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
epi_write(file_object = results_df,
          file_name = outfile
          )

# All ORs but T2 are positive and significant with tight CIs, i.e. intervention increases risk of death except at T2. Unadjusted ORs.
###
############


############
# Simple regression setup

summary(factor(data_f[[outcome_var]]))
summary(data_f[[intervention_var]])
head(data_f[[intervention_var]])


str(data_f[[outcome_var]])
str(data_f[[intervention_var]])

# TO DO: check if need to recode intervention to 0's and 1's for regression
# Currently ordered factor with Levels: non-COISS < COISS

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
deaths_periods_list <- c("d_pre_T0_outcome", "d_T0_outcome",
                         "d_gap_T0_T1_outcome", "d_T1_outcome",
                         "d_gap_T1_T2_outcome", "d_T2_outcome",
                         "d_post_T2_outcome")
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

# Get windows for analysis / time periods, but exclude NAs ('true_NA' is a placeholder):
time_cuts <- levels(df$d_time_cuts_prev)
time_cuts <- time_cuts[time_cuts != 'true_NA']
time_cuts

# GLM, no interaction terms:
for (i in time_cuts) {
    print(i)
    count <- count + 1
    # by_time_cuts[[i]] <- data_f[data_f$d_time_cuts_prev == i, ]
    sub_df <- df[df$d_time_cuts_prev == i, ]
    outcome_var <- deaths_periods_list[count]
    summary(as.factor(sub_df[[outcome_var]]))

    # Outcomes for time cut subsets were saved as char as ifelse() was problematic, convert to integer:
    sub_df[[outcome_var]] <- as.integer(sub_df[[outcome_var]])
    summary(as.factor(sub_df[[outcome_var]]))
    # Check rows with NAs don't appear, issue from script 1:
    print(epi_head_and_tail(sub_df))
    
    # # Run and save, chi sq but already done above, keep code for now:
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
epi_write(file_object = glm_results_df,
          file_name = outfile
          )
###
############


############
# No changes to the rdata file loaded, no need to save again.
# # The end:
# # Save objects, to eg .RData file:
# folder <- '../data/processed'
# script <- '5_regressions'
# infile_prefix
# suffix <- 'rdata.gzip'
# outfile <- sprintf(fmt = '%s/%s_%s.%s', folder, script, infile_prefix, suffix)
# outfile
# 
# # Check and remove objects that are not necessary to save:
# object_sizes <- sapply(ls(), function(x) object.size(get(x)))
# object_sizes <- as.matrix(rev(sort(object_sizes))[1:10])
# object_sizes
# objects_to_save <- (c('data_f', 'infile_prefix', 'outfile'))
# 
# # Save:
# save(list = objects_to_save,
#      file = outfile,
#      compress = 'gzip'
# )
# 
# # Remove/clean up session:
# all_objects <- ls()
# all_objects
# rm_list <- which(!all_objects %in% objects_to_save)
# all_objects[rm_list]
# rm(list = all_objects[rm_list])
# ls() # Anything defined after all_objects and objects_to_save will still be here
# 
# sessionInfo()

# Saving screen outputs:
sink()

# # q()
# 
# # Next: run the script for xxx
############

