############
# COISS paper 1
# L. Bonifaz
# April 2024
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


# TO DO: continue here
############
# Load a previous R session, data and objects:
file_n <- '../data/processed/3_COISS_intervention_setup_COVID19MEXICO2021_COVID-only_COISS_only.rdata.gzip'
ls()
load(file_n, verbose = TRUE)
ls()
data_f <- data_f # just to get rid of RStudio warnings
str(data_f)
dim(data_f)

# For saving/naming outputs, should already come with the RData file though:
infile_prefix <- infile_prefix
infile_prefix
# Otherwise manually:
# infile_prefix <- 'COVID19MEXICO2021_COVID-only'

# Create a folder for results:
if (!dir.exists(infile_prefix)) {
    dir.create(infile_prefix)
}

# Save files as e.g.:
# infile_prefix
# file_n <- 'sum_stats'
# suffix <- 'txt'
# outfile <- sprintf(fmt = '%s/%s.%s', infile_prefix, file_n, suffix)
# outfile
############


############
###
# Plot K-M
# Create a Surv object
surv_object <- survival::Surv(time = data_f$days_to_death,
                              event = data_f$death
                              )
str(surv_object)


# Fit a Kaplan-Meier survival curve
surv_fit <- survival::survfit(surv_object ~ 1,
                              data = data_f
                              )
str(surv_fit)
surv_fit

# Plot the Kaplan-Meier survival curve
ggsurvplot(surv_fit,
           data = data_f,
           conf.int = TRUE,
           xlab = "Time (days)",
           ylab = "Survival Probability",
           title = "Kaplan-Meier Survival Curve for COVID-19 Deaths",
           linetype = "strata",
           surv.median.line = "hv"
           )
###


###
# Plot COISS vs non-COISS this is over the whole study period
# COISS states were worse off
summary(data_f$intervention)
# group <- data_f$intervention
surv_fit_group <- survival::survfit(surv_object ~ intervention,
                                    data = data_f
                                    )
ggsurvplot(surv_fit_group,
           data = data_f
           )
###


###
# Use only intervention periods:
summary(data_f$time_cuts)
# intervention_points <- data_f$time_cuts
# data_f$time_cuts <- factor(data_f$time_cuts)
str(data_f$time_cuts)
levels(data_f$time_cuts)

# Levels with values of 0 will error:
data_f$time_cuts <- droplevels(data_f$time_cuts)
levels(data_f$time_cuts)
summary(data_f$time_cuts)

# Also:
data_f$intervention <- droplevels(data_f$intervention)
levels(data_f$intervention)
summary(data_f$intervention)

table(data_f$intervention, data_f$time_cuts)

# There shouldn't be NAs, result should be FALSE:
any(is.na(data_f$time_cuts)) # is at least one value TRUE?
any(is.na(data_f$intervention))


# Re-run the surv object:
surv_fit_point <- survival::survfit(Surv(days_to_death, death) ~ intervention + time_cuts,
                                    data = data_f
                                    )
str(surv_fit_point)
surv_fit_point$strata
summary(surv_fit_point)

ggsurvplot(surv_fit_point,
           data = data_f,
           xlab = "Time (days)",
           ylab = "Survival Probability",
           title = "Kaplan-Meier Survival Curves by Hospital and Study Point",
           # facet.by = 'intervention', #"time_cuts",
           risk.table = TRUE,  # Add risk table
           pval = TRUE
           )  # Add p-value
# Still errors with faceting
###

###
# TO DO: save each plot:
# Plot instead separately for each time cut-off:
for (level in levels(data_f$time_cuts)) {
    surv_fit_level <- survfit(Surv(days_to_death, death) ~ intervention,
                              data = subset(data_f, time_cuts == level)
                              )
    print(ggsurvplot(
        surv_fit_level,
        data = subset(data_f, time_cuts == level),
        xlab = "Time (days)",
        ylab = "Survival Probability",
        title = paste("Kaplan-Meier Survival Curves -", level),
        risk.table = TRUE,
        pval = TRUE
    ))
}
###


###
# Try with ggplot2:
# Function to convert survfit object to data frame:
# TO DO: double check this function and summary(surv_fit_point) give the same output
surv_summary <- function(survfit_obj, time_var) {
    data.frame(
        time = survfit_obj$time,
        n.risk = survfit_obj$n.risk,
        n.event = survfit_obj$n.event,
        n.censor = survfit_obj$n.censor,
        surv = survfit_obj$surv,
        strata = rep(names(survfit_obj$strata), times = survfit_obj$strata)
    )
}


# Convert survfit object to data frame:
# Re-run the surv object and fit:
surv_fit_point <- survival::survfit(Surv(days_to_death, death) ~ intervention + time_cuts,
                                    data = data_f
                                    )
str(surv_fit_point)
surv_fit_point$n
surv_fit_point$time


surv_data <- surv_summary(surv_fit_point, "time")
epi_head_and_tail(surv_data, cols = 6)
summary(surv_data)

# Extract hospital and time_cuts from strata:
surv_data <- surv_data %>%
    mutate(
        intervention = sapply(strsplit(as.character(strata), ","), function(x) trimws(sub("intervention=", "", x[1]))),
        time_cuts = sapply(strsplit(as.character(strata), ","), function(x) trimws(sub("time_cuts=", "", x[2])))
    )
epi_head_and_tail(surv_data, cols = 8)
surv_data$time_cuts
surv_data$intervention
# Trimmed of whitespaces now

# Set order for time-cuts:
surv_data$time_cuts <- factor(surv_data$time_cuts,
                              levels = c('pre-T0', 'T0', 'gap_T0_T1', 'T1',
                                         'gap_T1_T2', 'T2', 'post-T2'),
                              ordered = TRUE)
summary(surv_data$time_cuts)

# Plot with ggplot2 and faceting:
# TO DO: save plot
ggplot(surv_data, aes(x = time, y = surv, color = intervention)) +
    geom_step() +
    facet_wrap(~time_cuts, scales = "free_y") +
    labs(
        title = "Kaplan-Meier Survival Curves by Intervention and Study Point",
        x = "Time (days)",
        y = "Survival Probability"
    ) +
    theme_minimal() +
    theme(legend.title = element_blank())

surv_data
###
############


############
# TO DO: create and save table
# Extract risk table data:
surv_summary <- summary(surv_fit_point)
head(surv_summary)
str(surv_summary)

# Extract risk table data
risk_table <- data.frame(
    strata = surv_summary$strata,
    time = surv_summary$time,
    n.risk = surv_summary$n.risk,
    n.event = surv_summary$n.event
    )
epi_head_and_tail(risk_table, cols = 4)
epi_head_and_tail(surv_data, cols = 8)

# Extract hospital and time_cuts from strata
risk_table <- risk_table %>%
    mutate(
        hospital = sapply(strsplit(as.character(strata), ","), function(x) trimws(sub("intervention=", "", x[1]))),
        time_cuts = sapply(strsplit(as.character(strata), ","), function(x) trimws(sub("time_cuts=", "", x[2])))
    ) %>%
    select(-strata)

# Summarize risk table data by time_cuts
risk_summary <- risk_table %>%
    group_by(time_cuts) %>%
    summarize(
        total_risk = sum(n.risk, na.rm = TRUE),
        total_events = sum(n.event, na.rm = TRUE)
    )

# Print the risk summary table
knitr::kable(risk_summary, caption = "Risk Table by Time Cuts and Events (Death)")



############


############
# Simple regression setup

###
# Fit the logistic regression model
model_1 <- glm(death ~ intervention,
               data = data_f,
               family = binomial
               )

# Summarize the model
summary(model_1)

# Get predicted probabilities
predicted_probs <- predict(model_1, type = "response")
predicted_probs

# Get predicted classes (0 or 1) using a threshold of 0.5
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)

# View the first few predicted probabilities and classes
head(predicted_probs)
head(predicted_classes)

# Create a confusion matrix
caret::confusionMatrix(as.factor(predicted_classes), as.factor(data_f$death))

# Plot the ROC curve
roc_curve <- roc(data_f$death, predicted_probs)
plot(roc_curve)
###


###
colnames(data_f)
model_2 <- glm(death ~ intervention + EDAD + SEXO,
               data = data_f,
               family = binomial
               )

# Summarize the model
summary(model_2)
summary(model_1)
###
############


############
# Basic DiD
# Fit linear model
model_DiD <- lm(death ~ intervention * days_to_death, data = data_f)

# Summarize model
summary(model_D)

# Tidy the model output
tidy(model_DiD)
############



############
# Diff in Diff
# Survival?
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
# The end:
# Save one object, to eg .RData file:
# Check and remove objects that are not necessary to save:
ls()
object_sizes <- sapply(ls(), function(x) object.size(get(x)))
object_sizes <- as.matrix(rev(sort(object_sizes))[1:10])
object_sizes
# rm_list <- 'xx'
# Remove all:
# rm(list = ls())

objects_to_save <- (c('data_f_COVID'))
file_n <- '../data/processed/2_COISS_COVID-only.rdata.gzip'
save(list = objects_to_save,
     file = file_n,
     compress = 'gzip'
)

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

