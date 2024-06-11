library(glmnet)

# Check for non-positive event times, add a constant as zero days can't be handled:
summary(df_clean$d_days_to_death)
table(df_clean$d_days_to_death)
df[df$d_days_to_death == 0, c('FECHA_DEF', 'FECHA_INGRESO')]
date('2021-08-01') - date('2021-08-01')

df_clean$d_days_to_death[df_clean$d_days_to_death == 0] <- 0.1

# Prepare data for glmnet:
x <- model.matrix(mod_form, data = df_clean)[, -1]
y <- Surv(df_clean$d_days_to_death, df_clean$d_death)

# Fit the penalized Cox model using glmnet:
cox_model_penalized <- glmnet(x, y, family = "cox", alpha = 0.5) # alpha = 0.5 for elastic net (combination of lasso and ridge regression)
str(cox_model_penalized)

# Cross-validate to find the best lambda:
cv_model <- cv.glmnet(x, y, family = "cox", alpha = 0.5)
best_lambda <- cv_model$lambda.min

# Fit the final model with the best lambda:
cox_model_penalized_final <- glmnet(x, y, family = "cox", alpha = 0, lambda = best_lambda)

# Coefficients of the penalized model:
coef(cox_model_penalized_final)
