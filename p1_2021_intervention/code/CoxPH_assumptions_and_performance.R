############
###
# Check multi-colinearity on training data:
summary(df$d_pre_post)
summary(factor(df$d_pre_post))
table(df$d_pre_post, useNA = "ifany")

library(car)
vif(coxph(formula = mod_form, data = df, robust = TRUE))
###


###
# Test assumptions on training data
# Proportional hazards assumption:
cox.zph(cox_model)
###


###
# Residuals on training data:
plot(cox.zph(cox_model))
plot(survfit(cox_model), mark.time=FALSE)
###
############


############
# Evaluate Predictive Performance on test data
library(survival)
library(survminer)

###
# Predict on test data:
test_data$predicted_risk <- predict(cox_model, newdata = test_data, type = "risk")
###


###
# Concordance Index:
cindex <- concordance.index(predict(cox_model, newdata = test_data), test_data$time, test_data$status)
print(cindex)
###


###
# Calibration plot:
cal_plot <- calibrate(cox_model, cmethod = "KM", method = "boot", B = 1000)
plot(cal_plot)
###


###
# Time-dependent ROC:
library(timeROC)
time_roc <- timeROC(T = test_data$time, delta = test_data$status, marker = test_data$predicted_risk, cause = 1, times = seq(0, max(test_data$time), by = 1), iid = TRUE)
plot(time_roc)
###
############
