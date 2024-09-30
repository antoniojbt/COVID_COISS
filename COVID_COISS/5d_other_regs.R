

###
# Log-rank test to compare survival across periods:
surv_diff <- survdiff(mod_form, data = data_f)
surv_diff
str(surv_diff)
###


###
# Cox proportional hazards model:
# TO DO
res <- summary(cox_model)
str(res)
res
###



############
# TO DO:
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
