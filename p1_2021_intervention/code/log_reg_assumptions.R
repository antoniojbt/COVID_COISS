
#########
# Covariate selection

###
# Univariate Analysis:
# 
# Perform univariate logistic regression for each covariate to assess its individual relationship with the outcome.
# Retain covariates with significant p-values (typically < 0.05).
covariates <- c("age", "sex", "bmi", "smoking")
univ_results <- lapply(covariates, function(var) {
  formula <- as.formula(paste("outcome ~", var))
  glm(formula, data = data_f, family = binomial)
})
###


###
# Correlation Analysis:
# 
# Examine the correlation matrix for numeric covariates to identify highly correlated variables (e.g., correlation > 0.7).

# Correlation matrix
numeric_vars <- sapply(data_f, is.numeric)
cor_matrix <- cor(data_f[, numeric_vars])
print(cor_matrix)
###


###
# LASSO Regression:
# 
# Use LASSO (Least Absolute Shrinkage and Selection Operator) for variable selection, which can handle multicollinearity and high-dimensional data.

install.packages("glmnet")
library(glmnet)
# Prepare data for glmnet
x <- model.matrix(outcome ~ ., data = data_f)[, -1]
y <- data_f$outcome
lasso_model <- cv.glmnet(x, y, family = "binomial", alpha = 1)
coef(lasso_model, s = "lambda.min")
###


###
# Correlation Matrix:
# 
# Examine the correlation matrix to identify pairs of covariates with high correlation (e.g., correlation > 0.7).
# Correlation matrix for numeric variables
numeric_vars <- sapply(data_f, is.numeric)
cor_matrix <- cor(data_f[, numeric_vars])
print(cor_matrix)

###
#########


#########
# Log reg test of assumptions

###
# Box-Tidwell Test for Linearity of Logit
# Checking the assumptions for logistic regression is crucial to ensure the validity and reliability of the model. Here are the key assumptions and methods to check them:
# 
# 1. Linearity of the Logits
# The relationship between the predictors and the log-odds of the outcome should be linear. This can be checked using:
# 
# Box-Tidwell Test: For continuous variables.
# Interaction Terms: Including interaction terms between continuous predictors and their logarithms.

# Simulated data
set.seed(123)
data_f <- data.frame(
  outcome = rbinom(1000, 1, 0.5),
  predictor1 = rnorm(1000),
  predictor2 = rnorm(1000)
)

# Fit logistic regression model
model <- glm(outcome ~ predictor1 + predictor2, data = data_f, family = binomial)

# Create interaction terms for Box-Tidwell Test
data_f$log_predictor1 <- log(data_f$predictor1 + 1)
data_f$log_predictor2 <- log(data_f$predictor2 + 1)

# Fit the model with interaction terms
model_box_tidwell <- glm(outcome ~ predictor1 + predictor2 + I(predictor1 * log_predictor1) + I(predictor2 * log_predictor2), data = data_f, family = binomial)
summary(model_box_tidwell)
###

###
# 
# 3. Absence of Multicollinearity
# Predictors should not be highly correlated. Multicollinearity can be checked using:
# 
# Variance Inflation Factor (VIF): VIF values above 10 indicate high multicollinearity.

# Check for multicollinearity using VIF
install.packages("car")
library(car)
vif(model)

# Fit the full model
full_model <- glm(outcome ~ ., data = data_f, family = binomial)
# Calculate VIF
vif(full_model)
###


###
# 4. No Perfect Separation
# The outcome should not be perfectly separated by the predictors. Perfect separation can be identified by:
# 
# Model Warnings: Convergence issues or infinite coefficients.
# Data Inspection: Cross-tabulations of the predictors against the outcome.
###

###
# 5. Adequate Sample Size
# Ensure an adequate number of events per predictor variable. A common rule of thumb is at least 10 events per predictor.
###

###
# 6. Goodness of Fit
# Assess the model's goodness of fit using:
# 
# Hosmer-Lemeshow Test: Evaluates how well the model fits the data.
# Pseudo R-squared Values: Provide a measure of model fit.
# Hosmer-Lemeshow test
install.packages("ResourceSelection")
library(ResourceSelection)
hoslem.test(data_f$outcome, fitted(model))

# Pseudo R-squared
install.packages("pscl")
library(pscl)
pR2(model)
###


###
# 7. Influential Observations
# Identify influential observations using:
# 
# Cook's Distance: Values above 1 indicate influential points.
# Leverage Values: High leverage points may unduly influence the model.

# Cook's Distance
cooksd <- cooks.distance(model)
plot(cooksd, type="h", main="Cook's Distance", ylab="Cook's Distance")
abline(h = 4/length(cooksd), col="red") # Cutoff for identifying influential points

# Leverage values
hatvalues <- hatvalues(model)
plot(hatvalues, type="h", main="Leverage Values", ylab="Leverage")
abline(h = 2 * (length(model$coefficients) + 1) / length(hatvalues), col="red") # Cutoff for high leverage
###

###


###

#########
