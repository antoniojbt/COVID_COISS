

###


# For survival analysis:
mod_spec_surv <- sprintf("Surv(%s, %s) ~ %s", time_var, outcome_var, paste(covars, collapse = " + "))
mod_form_surv <- as.formula(mod_spec_surv)
mod_form_surv

# Survival
str(df)
fit <- surv_fit

surv_fit <- survfit(mod_form_surv,
               data = df
               )

surv_fit_plot <- ggsurvplot(fit, data = df, conf.int = TRUE,
                            xlab = "Time (days)", ylab = "Survival Probability",
                            title = "Kaplan-Meier Survival Curves by Time Period"
                            )
# TO DO: as for outcome_var; i.e. re-run 4a but showing adjusted values
###


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
