################
# Covariate selection and assumption checks for logisit regression
# Specific to COISS scripts
# June 2024

# Note: This script is intended to be run after the 5_regression_setup.R script, 5a and 5b scripts. 

# Input is output from script 5_regression_setup.R plus covariate selection from 5b
# Output is just to screen, no rdata or full dataset, or other files. Leaving some notes below only as out of time.
################


################
# TO DOs
  # move some of this to episcout
  # Clean up and organize
  # 
################



################
library(glmnet)
library(ggplot2)
library(vcd)
library(car)
library(parallel)
library(doParallel)
library(bigmemory)
################


################
# Initial Variable Selection:
  # Domain knowledge
  
# From 5b_multivars_regs.R:
covars <- covars
outcome_var <- outcome_var
data_f <- data_f

# Set as df for code below:
df <- data_f


###
# univariate analysis

univ_results <- lapply(covars, function(var) {
  formula <- as.formula(paste("d_death_30 ~", var))
  model <- glm(formula, data = df, family = binomial)
  summary(model)$coefficients
})

str(univ_results)
# All currently selected covariates are significant
###


###
# Correlation matrix for numeric variables:
numeric_vars <- sapply(df, is.numeric)
cor_matrix <- cor(df[, numeric_vars])
print(cor_matrix)
# Only actual numeric is EDAD
###


###
# Chi-square test for independence between pairs of factor variables
# High associations indicate potential multicollinearity
# Cramér's V: 
  # Measure of association between two nominal variables; strength of association between pairs of factor variables
  # Provides a standardized value between 0 and 1:
    # 0 to 0.1: Weak association
    # 0.1 to 0.3: Moderate association
    # 0.3 to 0.5: Strong association
    # Above 0.5: Very strong association
  # For practical significance, you might consider the following cut-offs:
    # Cramér's V > 0.1: Indicates a moderate to strong practical significance.
    # Cramér's V > 0.2: A stricter threshold indicating stronger practical significance.

factor_vars <- setdiff(covars, "EDAD") # elements of covars that are not EDAD
lapply(df[, factor_vars], str)

# Chi-sq with Cramér's V for significant chi-square results:
chi_sq_w_cramers <- combn(factor_vars, 2, function(pair) {
  table <- table(df[[pair[1]]], df[[pair[2]]])
  test_result <- chisq.test(table)
  cramer_v <- sqrt(test_result$statistic / (sum(table) * (min(dim(table)) - 1)))
  list(pair = pair, p_value = test_result$p.value, statistic = test_result$statistic, cramer_v = cramer_v)
}, simplify = FALSE)

# Convert results to a data frame
df_chi_sq_w_cramers <- do.call(rbind, lapply(chi_sq_w_cramers, function(result) {
  data.frame(
    Var1 = result$pair[1],
    Var2 = result$pair[2],
    p_value = result$p_value,
    statistic = result$statistic,
    cramer_v = result$cramer_v
  )
}))

# Print the tidied results
epi_head_and_tail(df_chi_sq_w_cramers)

# Below alpha:
alpha <- 0.00000000000000000001
dim(df_chi_sq_w_cramers[which(df_chi_sq_w_cramers$p_value < alpha), ])
# many; very large dataset, small differences will be highly significant with chi-sq

# Above:
dim(df_chi_sq_w_cramers[which(df_chi_sq_w_cramers$p_value > alpha), ])
# Very few

# Cramér's V values > 0.2:
dim(df_chi_sq_w_cramers[which(df_chi_sq_w_cramers$cramer_v > 0.2), ])
df_chi_sq_w_cramers[which(df_chi_sq_w_cramers$cramer_v > 0.2), ]

# Plot heatmap of p-values
# Convert p_value to a negative log scale for better visualization
df_chi_sq_w_cramers$log_p_value <- -log10(df_chi_sq_w_cramers$p_value)

ggplot(df_chi_sq_w_cramers, aes(x = Var1, y = Var2, fill = log_p_value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Chi-Square Test p-values",
       fill = "-log10(p-value)") +
  theme_minimal()


# Consider removing SECTOR
# Consider removing DIABETES OR HIPERTENSION
###


###
# VIF calculation
mod_spec <- sprintf("%s ~ %s", outcome_var, paste(covars, collapse = " + "))
mod_spec
mod_form <- as.formula(mod_spec)
mod_form

full_model <- glm(mod_form,
                  data = df,
                  family = binomial
                  )

car::vif(full_model)
# SECTOR shows high multicollinearity with other variables (VIF >10)
###



############
# LASSO maxes out memory

# Create sub-samples if needed and save them in rdata file
# Run sub_sampling script as taking too long to get regressions
# Test code first with sub-sample:
perc_needed <- 0.01
source('/Users/antoniob/Documents/work/science/devel/github/antoniojbt/episcout/R/sub_sampling.R')
ls()
data_f_sub <- data_f_sub # to remove RStudio warnings

sum(data_f_sub[[outcome_var]])

# Should now have data_f_sub:
epi_head_and_tail(data_f_sub)
table(data_f[[outcome_var]])
table(data_f_sub[[outcome_var]])
prop.table(table(data_f[[outcome_var]]))
prop.table(table(data_f_sub[[outcome_var]]))
###


###
# LASSO for Variable Selection

df <- data_f_sub

# Haven't imputed so issues with NA's
# Remove rows with missing values
dim(df)
cols_NAs <- c(outcome_var, covars)
cols_NAs

df_na <- na.omit(df[, cols_NAs])
dim(df_na)
# Loss seems OK for this, <10%


# Run in parallel:
num_cores <- parallel::detectCores() - 3  # Use one less than the number of available cores
cl <- parallel::makeCluster(num_cores)
doParallel::registerDoParallel(cl)

print(paste("Using", num_cores, "cores for parallel processing"))

# Formula:
mod_form

# Matrix for glmnet, minus intercept column, will recode factors to dummies: 
x <- model.matrix(mod_form, data = df_na)[, -1]

# Exclude the outcome_var column and convert to big.matrix for biglasso:
big_df_na <- x[, !colnames(x) %in% outcome_var]
x <- as.big.matrix(big_df_na, type = "double")

# Get the response variable:
y <- df_na[[outcome_var]]

# LASSO model with cross-validation, log reg, with glmnet:
# alpha = 1 is LASSO penalty
# lasso_model <- cv.glmnet(x, y, family = "binomial", alpha = 1) 

# Check dimensions:
print(dim(x))  # Should not include the response/outcome column
colnames(x)
print(length(y))  # Should match the number of rows in x

# Fit LASSO with biglasso:
# cross-validation with cv.biglasso()
# simple regularization without cross-validation biglasso()
big_lasso_model <- biglasso::cv.biglasso(x, y,
                                         family = "binomial",
                                         ncores = num_cores
                                         )

str(big_lasso_model)

# # Run with parallel backend and measure time with glmnet:
# system.time({
#   lasso_model <- cv.glmnet(x,
#                            y,
#                            family = "binomial",
#                            alpha = 1,
#                            parallel = TRUE,
#                            nfolds = 5  # Reduce cross-validation folds for faster processing
#   )
# })

# Stop the parallel backend
stopCluster(cl)
gc() # garbage clean

# Extract coefficients at optimal lambda (lambda.min). These are the variables selected:
coeffs <- coef(big_lasso_model, s = "lambda.min")

# Print the selected coefficients
print(coeffs)
print(big_lasso_model)
str(big_lasso_model)

###
################



############
# # The end:
# # If running full dataset with lasso save as will be long running
# 
# # Save objects, to eg .RData file:
# 
# folder <- '../data/processed'
# script <- 'covar_selection'
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
#      )
# 
# # Remove/clean up session:
# # all_objects <- ls()
# # all_objects
# # rm_list <- which(!all_objects %in% objects_to_save)
# # all_objects[rm_list]
# # rm(list = all_objects[rm_list])
# # ls() # Anything defined after all_objects and objects_to_save will still be here
# 
# sessionInfo()
# 
# # Saving screen outputs:
# # sink()
# 
# # q()
# 
# # Next: run the script for xxx
############
