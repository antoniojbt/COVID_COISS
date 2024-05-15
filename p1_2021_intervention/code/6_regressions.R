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
infile <- '../data/processed/xx.rdata.gzip'
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
# TO DO
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
#

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
