################
# Boiler plate code for running R with parallel backends

################


################
# TO DO: move to episcout

################


################
# 
################


################
# Prepare data for biglasso
X <- as.big.matrix(df_sample[, -c("outcome")], type = "double")
y <- df_sample$outcome

# Fit LASSO model
lasso_model <- biglasso(X, y, family = "binomial", alpha = 1)
selected_coefficients <- coef(lasso_model, s = "lambda.min")

# Display selected coefficients
print(selected_coefficients)
################



################
library(parallel)
library(doParallel)

mem.maxVSize()

num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
clusterExport(cl, c("df_sample", "chisq.test", "combn"))

chi_square_results <- parLapply(cl, combn(factor_vars, 2, simplify = FALSE), function(pair) {
  table <- table(df_sample[[pair[1]]], df_sample[[pair[2]]])
  test_result <- chisq.test(table)
  list(pair = pair, p_value = test_result$p.value, statistic = test_result$statistic)
})
stopCluster(cl)
################


################
# Future it

library(future)
# Define the future plan to run tasks in the background
plan(multisession)

# Define a future to run biglasso in the background
future_lasso <- future({
  library(biglasso)
  library(bigmemory)

  # Sample data frame (replace with your actual data)
  set.seed(123)
  df_na <- data.frame(
    outcome = rbinom(1000000, 1, 0.5),
    age = rnorm(1000000, 50, 10),
    sex = factor(sample(c("Male", "Female"), 1000000, replace = TRUE)),
    smoking = factor(sample(c("Yes", "No"), 1000000, replace = TRUE)),
    bmi = rnorm(1000000, 25, 5),
    income = rnorm(1000000, 50000, 10000)
  )

  # String representing the outcome variable
  outcome_var <- "outcome"

  # Remove rows with missing values
  df_na <- na.omit(df_na)

  # Convert the data frame to a numeric matrix with dummy variables for factors
  model_matrix <- model.matrix(~ . - 1, data = df_na)

  # Exclude the outcome_var column
  big_df_na <- model_matrix[, !colnames(model_matrix) %in% outcome_var]

  # Convert to big.matrix
  x <- as.big.matrix(big_df_na, type = "double")

  # Extract the response variable as a vector
  y <- df_na[[outcome_var]]

  # Fit the LASSO model with biglasso
  big_lasso_model <- cv.biglasso(x, y, family = "binomial", ncores = parallel::detectCores() - 1)

  # Save the model to a file
  saveRDS(big_lasso_model, file = "big_lasso_model.rds")

  # Extract the coefficients at the optimal lambda
  selected_coefficients <- coef(big_lasso_model, s = "lambda.min")

  # Return the selected coefficients
  selected_coefficients
})

# Retrieve the result (this will block until the future completes)
result <- value(future_lasso)
print(result)
################


################
# nohup it
# save as script:
# biglasso_script.R
library(biglasso)
library(bigmemory)

# Sample data frame (replace with your actual data)
set.seed(123)
df_na <- data.frame(
  outcome = rbinom(1000000, 1, 0.5),
  age = rnorm(1000000, 50, 10),
  sex = factor(sample(c("Male", "Female"), 1000000, replace = TRUE)),
  smoking = factor(sample(c("Yes", "No"), 1000000, replace = TRUE)),
  bmi = rnorm(1000000, 25, 5),
  income = rnorm(1000000, 50000, 10000)
)

# String representing the outcome variable
outcome_var <- "outcome"

# Remove rows with missing values
df_na <- na.omit(df_na)

# Convert the data frame to a numeric matrix with dummy variables for factors
model_matrix <- model.matrix(~ . - 1, data = df_na)

# Exclude the outcome_var column
big_df_na <- model_matrix[, !colnames(model_matrix) %in% outcome_var]

# Convert to big.matrix
x <- as.big.matrix(big_df_na, type = "double")

# Extract the response variable as a vector
y <- df_na[[outcome_var]]

# Fit the LASSO model with biglasso
big_lasso_model <- cv.biglasso(x, y, family = "binomial", ncores = parallel::detectCores() - 1)

# Save the model to a file
saveRDS(big_lasso_model, file = "big_lasso_model.rds")

# Extract the coefficients at the optimal lambda
selected_coefficients <- coef(big_lasso_model, s = "lambda.min")

# Print the selected coefficients
print(selected_coefficients)

# From the CLI:
# nohup Rscript biglasso_script.R > biglasso_output.log 2>&1 &
# nohup: runs in background and prevents it from being terminated when the user logs out
# &: backgrounds process
################
