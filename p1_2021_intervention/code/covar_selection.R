
# Initial Variable Selection:
# Using domain knowledge and univariate analysis
covariates <- c("age", "sex", "bmi", "smoking")
univ_results <- lapply(covariates, function(var) {
  formula <- as.formula(paste("outcome ~", var))
  model <- glm(formula, data = data_f, family = binomial)
  summary(model)$coefficients
})

# Univariate analysis for factor variables
factor_vars <- c("factor1", "factor2", "factor3")
univ_results <- lapply(factor_vars, function(var) {
  formula <- as.formula(paste("outcome ~", var))
  model <- glm(formula, data = data_f, family = binomial)
  summary(model)$coefficients
})


# Check Correlation and Multicollinearity:
# Correlation matrix
numeric_vars <- sapply(data_f, is.numeric)
cor_matrix <- cor(data_f[, numeric_vars])
print(cor_matrix)

# VIF calculation
full_model <- glm(outcome ~ age + sex + bmi + smoking, data = data_f, family = binomial)
vif(full_model)


# LASSO for Variable Selection:
install.packages("glmnet")
library(glmnet)
# Prepare data for glmnet
x <- model.matrix(outcome ~ ., data = data_f)[, -1]
y <- data_f$outcome
lasso_model <- cv.glmnet(x, y, family = "binomial", alpha = 1)
coef(lasso_model, s = "lambda.min")


# Chi-square test for independence between pairs of factor variables
# For pairs of factor variables, use chi-square tests to check for associations. High associations indicate potential multicollinearity.
chi_square_results <- combn(factor_vars, 2, function(pair) {
  table <- table(data_f[[pair[1]]], data_f[[pair[2]]])
  chisq.test(table)
}, simplify = FALSE)



# Use Cramér's V to measure the strength of association between pairs of factor variables. Values close to 1 indicate strong association.
install.packages("vcd")
library(vcd)
# Calculate Cramér's V for pairs of factor variables
cramers_v_results <- combn(factor_vars, 2, function(pair) {
  table <- table(data_f[[pair[1]]], data_f[[pair[2]]])
  assocstats(table)$cramer
}, simplify = TRUE)



# e.g.
