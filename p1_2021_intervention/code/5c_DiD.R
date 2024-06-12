############
# COISS paper 1
# L. Bonifaz
# June 2024

# Difference in difference regression model

# Input is output from script 5_regression_setup.R
# Output are various tables from DiD regression analysis for each of the intervention groups (T1 group and T2 group). An rdata is saved to pass on for assumption checking.
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
library(survival)
library(survminer)
library(car)
############


############
# Load a previous R session, data and objects:
infile <- "../data/processed/5_regression_setup_COVID19MEXICO_2021_2022_COVID-only_COISS-only.rdata.gzip"
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
# Should be in environment, remove warnings:
data_f_T1 <- data_f_T1 # to remove RStudio warnings
data_f_T2 <- data_f_T2 # to remove RStudio warnings
############


############
# Set-up analysis variables
# TO DO: Set manually

# Outcome variable:
# outcome_var <- 'd_death_30'
outcome_var <- 'd_death'

# Time to event variable:
# time_var <- 'd_days_to_death_30'
time_var <- 'd_days_to_death'

# Intervention group variable:
intervention_var <- 'd_intervention_T1'
# intervention_var <- 'd_intervention_T2'

# Data frame in use:
df_name <- 'data_f_T1' # or 'data_f_T2' ; 'data_f' ; 'data_f_sub'
# df_name <- 'data_f_T2'

# Name of df:
df <- data_f_T1 # or data_f_T2 ; data_f ; data_f_sub
# df <- data_f_T2

# Analysis window:
analysis_cut <- 'T1'
# analysis_cut <- 'T2'

# Events for specific window variable:
events_analysis_cut <- 'd_T1_outcome' # or 'd_T2_outcome' ; 'd_T0_outcome' ; etc
# events_analysis_cut <- 'd_T2_outcome'

# Baseline window variable:
baseline_cut <- 'T0' # could be pre-T0 but not in design
events_baseline <- 'd_T0_outcome' # or 'd_T1_outcome' ; 'd_T0_outcome' ; 'd_T1_outcome' ; 'd_T2_outcome'
############


############
# Save screen outputs

# All the way down here as need variable names defined above:
script_n <- '5c_DiD_R'
time_stamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
df_name
sink_fn <- sprintf("%s/sink_%s_%s_%s.txt", infile_prefix, script_n, df_name, time_stamp)
sink_fn
sink(sink_fn)
getwd()

timestamp()

print('File loaded:')
print(infile)
print('Loaded data and objects:')
ls()
############


############
# Vars in use

print('Summary of variables in use:')

analysis_vars <- c(outcome_var,
                   time_var,
                   intervention_var,
                   df_name,
                   analysis_cut,
                   events_analysis_cut,
                   baseline_cut,
                   events_baseline
                   )

analysis_var_names <- c('outcome_var',
                        'time_var',
                        'intervention_var',
                        'df_name',
                        'analysis_cut',
                        'events_analysis_cut',
                        'baseline_cut',
                        'events_baseline'
                        )
# Create a data frame for the variables:
analysis_var_df <- data.frame(analysis_var_names, analysis_vars)
analysis_var_df

# Variables in use:
print(sprintf('Outcome variable is: %s', outcome_var))
print(sprintf('Time variable is: %s', time_var))
print(sprintf('Intervention variable is: %s', intervention_var))
print(sprintf('Data frame in use is: %s', df_name))
print(sprintf('Analysis window is: %s', analysis_cut))
print(sprintf('Outcome variable at analysis window is: %s', events_analysis_cut))
print(sprintf('Baseline window is: %s', baseline_cut))
print(sprintf('Outcome variable at baseline is: %s', events_baseline))

dim(df)
print(df)
colnames(df)

# Already loaded, contains the periods for each analysis window:
time_cuts <- time_cuts
############


############
# Data to test
# Subset baseline and analysis window data

###
# TO DO: Set manually
summary(data_f$d_time_cuts_prev)
summary(df$d_time_cuts_prev) # this will be subset with either T1 states or T2 states as intervention groups changed; all time-cuts should be > 1

# For both but must come from either data_f_T1 or data_f_T2 as numbers will change because test groups/states change:
df_baseline <- df[df$d_time_cuts_prev == baseline_cut, ]

# For window:
df_analysis_cut <- df[df$d_time_cuts_prev == analysis_cut, ]

df_baseline
df_analysis_cut

df <- rbind(df_baseline, df_analysis_cut)

df[, 'd_time_cuts_prev']

summary(df$d_time_cuts_prev) # only baseline e.g. T0 and analysis_cut e.g. T1 should have >0
stopifnot(sum(df$d_time_cuts_prev == analysis_cut) > 1)


summary(df[[intervention_var]]) # There shouldn't be an 'other' category
stopifnot(length(df[[intervention_var]]) > 1)
###


###
# TO DO: Set manually
# a variable indicating whether the period is before or after the intervention
# Because the data frame df in this script is subset to baseline and analysis windows, e.g. T0 with T1, there is effectively a pre/post difference for the regression. Convert to integer:
summary(df$d_time_cuts_prev)
epi_head_and_tail(df[df$d_time_cuts_prev == baseline_cut, ])
epi_head_and_tail(df[df$d_time_cuts_prev == analysis_cut, ])
# So pre = baseline_cut and post = analysis_cut
# There are no NA's
df$d_pre_post <- ifelse(df$d_time_cuts_prev == analysis_cut, 1, 0)
df$d_pre_post <- as.integer(df$d_pre_post)
typeof(df$d_pre_post)
summary(factor(df$d_pre_post))
summary(df$d_time_cuts_prev) # should match the time cuts

interv_period <- 'd_pre_post'
###
############


############
# Set-up covariates
# Already in scope, remove warnings:
covars <- covars
print(covars)
# from 5_regression_setup.R
############


############
# Simple DiD

# Shouldn't need a loop, will be e.g. joined T0 and either T1 or T2, one regression for each
# Consider using pre-T0 instead of T0, unsure why they picked T0 only as baseline
# Run a univariate version with a simple (T1_COISS - T0_COISS) - (T1_non-COISS - T0_non-COISS) model
# Then add covariates

###
# Univariate DiD
colnames(df)

# No deaths at e.g. pre-T0 if subset of data used e.g. T0 and T2; only 0's will appear
summary(factor(data_f[[events_baseline]])) # outcome/deaths at baseline e.g. T0
# All events in full dataset

summary(factor(df[[events_baseline]])) # outcome/deaths at baseline e.g. T0
# Only events recorded if subset to e.g. T0 and T2

summary(factor(df[[events_analysis_cut]]))

summary(factor(df[[outcome_var]])) # e.g. death within 30 days
summary(factor(df$d_death)) # death at any point in study

summary(df$d_time_cuts_prev) # admissions, deaths and prior admissions without outcome during period

print(intervention_var) # groups (states) as defined at analysis window T1, or T2
summary(df[[intervention_var]])
###


###
# Set variables for analysis fir simple DiD

# Population at risk and with outcome at window/time-cut e.g. T1

# Analysis window e.g. T1, already defined above:
analysis_cut

# Outcome for analysis window already defined above:
events_analysis_cut

# Outcome for baseline already defined above:
events_baseline

# Round digits:
round_dig <- 4
###


###
# Example for one time point for calculation of proportion of deaths

# Pop with outcome at e.g. T2:
summary(factor(df[[events_analysis_cut]]))
pop_w_outcome <- length(which(df[[events_analysis_cut]] == 1))
pop_w_outcome

# Pop at risk:
summary(df$d_time_cuts_prev)
pop_at_risk <- length(which(df$d_time_cuts_prev == analysis_cut))
pop_at_risk

# Proportion of deaths:
prop_death <- round((pop_w_outcome / pop_at_risk), round_dig)

print(sprintf('Proportion of deaths at %s: %s', analysis_cut, prop_death))
###

###
# Converted to a function for re-use

# Already moved to episcout but need to source it here as haven't updated:
epi_stats_prop_outcome <- function(df,
                                   outcome_var_window,
                                   pop_at_risk_var,
                                   analysis_window,
                                   round_dig = 4) {
  # Population with outcome:
  pop_w_outcome <- length(which(df[[outcome_var_window]] == 1))

  # Population at risk:
  pop_at_risk <- length(which(df[[pop_at_risk_var]] == analysis_window))

  # Proportion of deaths
  prop_death <- round((pop_w_outcome / pop_at_risk), round_dig)

  # Print result
  # result <- sprintf('Proportion of deaths at %s: %s', analysis_window, prop_death)
  # print(result)

  # Return the proportion for further use
  return(prop_death)
}

# Test:
prop_death_test <- epi_stats_prop_outcome(df = df,
                                    outcome_var_window = events_analysis_cut,
                                    pop_at_risk_var = 'd_time_cuts_prev',
                                    analysis_window = analysis_cut,
                                    round_dig = round_dig
                                    )
prop_death_test
###


###
# Estimate for each baseline and each analysis window
# Proportions of deaths at baseline and window for e.g. T0 and T1 for COISS and non-COISS states

prop_death_overall <- round((sum(df[[outcome_var]] == 1) / nrow(df)), round_dig)
prop_death_overall

# For each analysis window (time-cut):
pop_at_risk_var <- 'd_time_cuts_prev' # same for all

# Function to calculate proportions
calc_prop <- function(df, intervention_status, outcome_var_window, analysis_window) {
      # Data frame needs sub-setting for each intervention group:
      df_sub <- df[df[[intervention_var]] == intervention_status, ]
      prop_death <- epi_stats_prop_outcome(df = df_sub,
                             outcome_var_window = outcome_var_window,
                             pop_at_risk_var = pop_at_risk_var,
                             analysis_window = analysis_window,
                             round_dig = round_dig)
      print(sprintf('Proportion of deaths at %s for %s: %s',
                    analysis_window,
                    intervention_status,
                    prop_death
                    )
            )
      return(prop_death)
      }

# Calculate proportions for each group and period:
prop_death_window_interv <- calc_prop(df, 'COISS', events_analysis_cut, analysis_cut)
prop_death_baseline_interv <- calc_prop(df, 'COISS', events_baseline, baseline_cut)
prop_death_window_control <- calc_prop(df, 'non-COISS', events_analysis_cut, analysis_cut)
prop_death_baseline_control <- calc_prop(df, 'non-COISS', events_baseline, baseline_cut)

# Store:
props <- c(prop_death_window_interv,
           prop_death_baseline_interv,
           prop_death_window_control,
           prop_death_baseline_control)

# Simple difference-in-differences:
simple_did <- (prop_death_window_interv - prop_death_baseline_interv) -
              (prop_death_window_control - prop_death_baseline_control)

# Convert to percentage
simple_did_perc <- simple_did * 100

# Print results
print(paste("Overall proportion of deaths:", prop_death_overall))
cat(paste(props, collapse = "\n")) #, "\n", sep = "")
print(sprintf("Difference-in-Differences for %s minus %s for %s vs %s: %.2f%%",
              analysis_cut,
              baseline_cut,
              'COISS',
              'non-COISS',
              simple_did_perc
              )
      )

print('Interpretation for univariate DiD:')
cat('\n
# Can be interpreted as excess outcomes at analysis window compared to baseline
# Then compared to control group
# Negative values indicate that the intervention group had fewer outcomes than the control group, proportionally, after adjusting for baseline differences

# For T1:
# 0.0074 - 0.0079
# -0.0005
# -0.05%
# Negative value for T1 minus T0, difference is negligible at 0.05%
# Reflects COISS states, which were worse off, caught up with non-COISS states, after adjusting for baseline differences but without considering other variables such as age, BMI, etc.

# For T2:
# 0.0019 - 0.0056
# -0.37%
# Negative value for T2 minus T0, difference feels substantial at 0.37%

# Does not account for individual-level variability
# Does not provide standard errors or confidence intervals
# Cannot/limited control for additional covariates
')
###
############


############
# The univariate DiD should match a simple DiD regression model
# Interaction term between the group and the intervention period
# Use of interaction terms in Cox: test whether the effect of one predictor variable on the hazard rate depends on the level of another predictor variable. Useful to test whether effects are not only additive. i.e.:
# Main effects for intervention variable on the hazard rate
# Main effects for before and after status variable on the hazard rate
# Interaction effects: captures the combined effect of being in the intervention group and the d_pre_post status on the hazard rate.
# DiD: estimates the causal effect of the intervention by comparing the changes in outcomes over time between a group that receives the intervention and a group that does not. The interaction term is the DiD estimate where the combined effect of being in the intervention group and the post-intervention period is captured. In other words, it estimates the additional effect of the intervention beyond any changes that would occur over time regardless of the intervention. Specifically, it models the difference in the hazard rate between the intervention and control groups before and after the intervention.

# So, for main effects:
# d_intervention_T2COISS: baseline difference in the hazard rate between the intervention and control groups, before the intervention
# d_pre_post: change in the hazard rate over time for the control group (from pre to post intervention)
# For interaction term:
# The additional change in the hazard rate for the intervention group, over and above the changes observed in the control group accounting for the passage of time.


###
# Variables needed:
analysis_var_df
colnames(df)

# outcome definition is problematic because each time period has deaths that occurred at that point
# d_death is the outcome across the study. If individuals died early, e.g. at T0 they will be marked as event occurred for all other time periods.
summary(factor(df$d_death)) 

# Same for d_death_30 although less of an issue as these are deaths within 30 days of admission
# So log reg with single binary outcome assessing death at each time period does seem not appropriate
# d_death_30 is also likely not appropriate either as periods are not uniform; e.g. T0 is 3 days, T1 is ~35 days, etc.
# best is regression to predict days to event, despite survival being >90% at all time points

# Regardless, code here for log reg with binary outcome for simple DiD.
# Univariate DiD is OK as proportion of deaths (CFR) is estimated for each time-point
# binary outcome variable indicating death:
outcome_var

# time variable indicating the time period:
time_cuts

# group variable indicating whether an observation is in the control or intervention group:
intervention_var

# a variable indicating whether the period is before or after the intervention
# defined above
###


###
# Logistic regression

# Formula:
mod_spec <- sprintf('%s ~ %s * %s', outcome_var, intervention_var, interv_period)
mod_spec
mod_form <- as.formula(mod_spec)
mod_form

print('Formula for logistic regression:')
print(mod_form)

did_simple_mod <- glm(mod_form, data = df, family = binomial())

# Summary of the model
summary(did_simple_mod)

# Tidy summary for easier interpretation
df_did_simple_mod <- tidy(did_simple_mod)
df_did_simple_mod$odds_ratio <- exp(df_did_simple_mod$estimate)
df_did_simple_mod
# View(df_did_simple_mod)

# Save:
file_n <- 'did_simple_mod'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s_%s_%s.%s', infile_prefix, file_n, outcome_var, df_name, suffix)
outfile
epi_write(file_object = df_did_simple_mod,
          file_name = outfile
          )

print('Interpretation for T1:')
cat('\n
# See T2 interpretation for overall explanation. Results are very similar:
The odds of death at baseline for non-COISS are very low.
Intervention groups have a higher odds of death at baseline compared to the control group.
The change in log-odds from T0 to T1 for is positive however for the pre_post variable specification, indicating that there is a higher chance of death in the post-intervention period for individuals admitted in COISS states compared to the baseline period.
The estimate for the interaction term for the the DiD analysis is negative, indicating that the odds of death in COISS states decreased more than in non-COISS states from T0 to T1. The OR here is 0.86, suggesting a 14% lower odds of death in COISS states compared to non-COISS states during T1 once T0 is accounted for.
No other covariates were included.
')

print('Interpretation for T2:')
cat('\n
# Formally:\n
# Intercept: log-odds of d_death_30 (death within 30 days) for non_COISS "control" group during the baseline period (pre-intervention, here T0 only).
The OR shows a very low probability of death at baseline. No other covariates accounted for.

# COISS intervention states: change in log-odds of death within 30 days for the intervention group compared to the control group during the baseline period (pre-intervention).
Value is positive and indicates a higher odds of death for the intervention group at baseline compared to non-COISS states.
OR is ~1.3, i.e. odds of death are ~31% higher in COISS at baseline. The intervention group has higher baseline odds of death compared to the control group.

# Pre_post: change in log-odds of death within 30 days for COISS from T0 to T2 (intervention/analysis window period).
Value is negative, i.e. there is a lower odds of death in the post-intervention period compared to the baseline period.
OR is 0.33, ~67% lower odds of death in the post-intervention period for the control group.
non-COISS states during T2 (post-intervention period) have odds of death decreased significantly. This suggests a general reduction in the odds of death for the control group in the post-intervention period. The effect also occurs for the intervention group and, independent to this, the intervention group has an additional reduction in the odds of death beyond the reduction observed in the control group, as indicated by the interaction term (difference-in-differences).

# Interaction term: represents the difference-in-differences estimate which shows the additional change in log-odds of death within 30 days for COISS from T0 to T2, relative to changes non_COISS.
Value is negative, thus COISS states had a greater reduction in the odds of death compared to non-COISS states from T0 to T2.
The OR is 0.69, suggesting that COISS states had a 31% lower additional odds of death compared to non-COISS states during T2 despite a higher odds during the baseline period.
The effect of the intervention during T2 in COISS states shows an additional reduction in the odds of death compared to the control group in the post-intervention period.

This model only tests for the interaction term, there is no adjustment for additional covariates.
Further, the outcome is binary, which is not appropriate as the periods for analysis are not uniform and thus events within periods cannot be attributed properly.

It should closely match the univariate DiD however.
')

###
############


############
# Cox Proportional Hazards with DiD (interaction term) and additional covariates
# time to event (instead of binary outcome)

###

analysis_var_df
covars

# Formula:
mod_spec <- sprintf('Surv(%s, %s) ~ %s + %s * %s',
                    time_var,
                    outcome_var,
                    paste(covars, collapse = ' + '),
                    intervention_var,
                    interv_period
                    )
mod_spec
mod_form <- as.formula(mod_spec)
mod_form

print('Formula for logistic regression:')
print(mod_form)

cox_did_covar_mod <- coxph(mod_form,
                           data = df
                           )

# Summary of the model
summary(cox_did_covar_mod)

# Tidy summary for easier interpretation
df_cox_did_covar_mod <- tidy(cox_did_covar_mod)
df_cox_did_covar_mod$odds_ratio <- exp(df_cox_did_covar_mod$estimate)
df_cox_did_covar_mod
# View(df_cox_did_covar_mod)

# Save:
file_n <- 'cox_did_covar_mod'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s_%s_%s_%s.%s',
                   infile_prefix,
                   file_n,
                   time_var,
                   outcome_var,
                   df_name,
                   suffix
                   )
outfile
epi_write(file_object = df_cox_did_covar_mod,
          file_name = outfile
          )
###


###
# Assumptions check for adjusted DiD model

# Test the proportional hazards assumption:
ph_test <- cox.zph(cox_did_covar_mod)

print(ph_test)

# Schoenfeld residuals:
plot(ph_test)
# scaled Schoenfeld residuals against time. If the proportional hazards assumption holds, the residuals should be randomly scattered around zero with no apparent pattern. A systematic trend may indicate a violation of the assumption.

# For T1:
# Global violation
# EDAD, NEUMONIA, d_pre_post also significant

# For T2:
# Global violation
# EDAD, NEUMONIA, d_pre_post also significant


# Martingale residuals plot for continuous vars (EDAD):
# Extract the subset of data used in the model, complete cases only:
covars
model_data <- df[complete.cases(df[, c(covars, intervention_var, "d_pre_post")]), ]
model_data

# Calculate residuals:
residuals_martingale <- residuals(cox_did_covar_mod, type = "martingale")

# Ensure lengths match:
length(model_data$EDAD) == length(residuals_martingale)

# Plot martingale residuals against age:
scatter.smooth(model_data$EDAD,
               residuals_martingale,
               xlab = "Age (EDAD)",
               ylab = "Martingale Residuals",
               main = "Martingale Residuals vs. Age"
               )

# For T2:
# Cox PH assumption not met (!)
# Moved to 5c_DiD_assumptions.R script to explore other formulas and problem variables.
###


###
# Overall interpretation
print('Interpretation for T1:')
cat('\n
# The hazard of death for the intervention group (COISS) compared to the control group at baseline is slightly higher (~3%), but not statistically significant (p = 0.538).

# The post-intervention period (T1) decreases the hazard of death by ~5%, but the effect is not statistically significant (p = 0.279).

# The interaction term, difference-in-difference estimate, indicates that the intervention group in the post-intervention period has a slightly decreased hazard of death (0.97%), but the effect is not statistically significant (p = 0.875).

# The model shows significant associations for many of the covariates with the hazard of death within 30 days. The interaction term (d_intervention_T1COISS:d_pre_post) is not significant, suggesting that the intervention did not significantly alter the hazard in the post-intervention period compared to the control group. Highly significant predictors include increasing age, male gender, presence of comorbities such as pneumonia, immunosuppression, arterial hypertension, obesity, and chronic kidney disease.

# Model fit:
- High predictive ability with a C-statistic (Concordance) of 0.932 and a precise estimate (SE = 0.002). Values range from 0.5 (no better than random chance) to 1 (perfect prediction).
- The model with predictors is significantly better than the null model (p < 2e-16 for LRT).
- Predictors collectively have significant effects (p < 2e-16 Wald test).
- Compared to the null model, the adjusted model significantly improves the fit (p < 2e-16, logrank test).

# However, the PH assumption is not met (!).
')


print('Interpretation for T2:')
cat('\n
# Very similar to T1 once corrected for T0 baseline and covariates in the model.

# The hazard for death for COISS states at baseline is lower compared to non-COISS states, but the effect is not significant.

# The post-intervention period at T2 decreases the hazard of death by ~55%, and the effect is highly statistically significant.

# The difference-in-difference estimate again indicates that the intervention group in the post-intervention period has a decreased hazard of death (0.97%), but is non-significant (p = 0.58).

# Other covariates are largely as for T1, with large and significant effects for age, male gender, and comorbities, particularly pneumonia with a hazard odds of 18.

# The model fit is highly predictive and improved over the null. 

# However, the PH assumption is not met (!).
')
###


###
# # Visualise Cox PH model with interaction term
# # Create survival curves from the fitted model
# 
# ###
# # After fitting Cox PH
# # 
# 
# summary(df$d_intervention_T1)
# levels(df$d_intervention_T1)
# 
# summary(factor(df$d_pre_post))
# 
# # New data frame for predictions:
# new_data <- expand.grid(
#   EDAD = median(df$EDAD, na.rm = TRUE),
#   SEXO = unique(df$SEXO),
#   NEUMONIA = unique(df$NEUMONIA),
#   NACIONALIDAD = unique(df$NACIONALIDAD),
#   INDIGENA = unique(df$INDIGENA),
#   EPOC = unique(df$EPOC),
#   ASMA = unique(df$ASMA),
#   INMUSUPR = unique(df$INMUSUPR),
#   HIPERTENSION = unique(df$HIPERTENSION),
#   OTRA_COM = unique(df$OTRA_COM),
#   CARDIOVASCULAR = unique(df$CARDIOVASCULAR),
#   OBESIDAD = unique(df$OBESIDAD),
#   RENAL_CRONICA = unique(df$RENAL_CRONICA),
#   TABAQUISMO = unique(df$TABAQUISMO),
#   d_intervention_T1 = c( "non-COISS", "COISS"),
#   d_pre_post = c(0, 1) # Assuming binary
# )
# 
# epi_head_and_tail(new_data)
# new_data$d_intervention_T1 <- as.factor(new_data$d_intervention_T1)
# 
# # Predict survival probabilities
# surv_fit <- survfit(cox_did_covar_mod, newdata = new_data)
# 
# # Plot survival curves
# plot(surv_fit, col = 1:4, lty = 1:4, xlab = "Time (days)", ylab = "Survival Probability")
# legend("topright", legend = c("Control Pre", "Control Post", "Intervention Pre", "Intervention Post"), col = 1:4, lty = 1:4)
# ###
# 
# 
# ###
# # Simpler data frame for predictions:
# simp_new_data <- expand.grid(
#   EDAD = median(df$EDAD, na.rm = TRUE),
#   SEXO = unique(df$SEXO),
#   NEUMONIA = unique(df$NEUMONIA),
#   d_intervention_T1 = c( "non-COISS", "COISS"),
#   d_pre_post = c(0, 1) # Assuming binary
# )
# 
# epi_head_and_tail(simp_new_data)
# simp_new_data$d_intervention_T1 <- as.factor(simp_new_data$d_intervention_T1)
# 
# # Predict survival probabilities
# surv_fit <- survfit(cox_did_covar_mod, newdata = simp_new_data)
# 
# # Plot survival curves
# plot(surv_fit, col = 1:4, lty = 1:4, xlab = "Time (days)", ylab = "Survival Probability")
# legend("topright", legend = c("Control Pre", "Control Post", "Intervention Pre", "Intervention Post"), col = 1:4, lty = 1:4)
###
############



############
# The end:
# Save objects, to eg .RData file:
folder <- '../data/processed'
script <- '5c_DiD_R'
infile_prefix
suffix <- 'rdata.gzip'
outfile <- sprintf(fmt = '%s/%s_%s.%s', folder, script, infile_prefix, suffix)
outfile

# Check and remove objects that are not necessary to save:
ls()
object_sizes <- sapply(ls(), function(x) object.size(get(x)))
object_sizes <- as.matrix(rev(sort(object_sizes))[1:10])
object_sizes
# From loadd objects from script 5_regression_setup.R:
objects_to_save <- c('data_f', 'data_f_T1', 'data_f_T2', 'infile_prefix', 'outfile',
                      'time_cuts', 'deaths_periods_list', 'df_time_cuts', 'covars'
                      )

# From this script:
dfs_to_save <- c('df', 'df_analysis_cut', 'df_baseline')
objects_to_save <- c(objects_to_save, analysis_var_names, dfs_to_save,
                     'analysis_var_df'
                     )

length(objects_to_save)
length(objects_to_save) == length(objects_to_save %in% ls())

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

# Saving screen outputs:
sink()

# # q()
#
# # Next: run the script for xxx
############
