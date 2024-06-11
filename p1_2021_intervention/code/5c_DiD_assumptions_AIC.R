###
# Create a subset of data with complete cases for each model:
complete_cases_full <- df[complete.cases(df[, c("d_days_to_death", "d_death", "EDAD_piecewise", 
                                           "NEUMONIA", "RENAL_CRONICA", "d_pre_post", 
                                           "CARDIOVASCULAR", "OTRA_COM", "SEXO", "NACIONALIDAD", 
                                           "INDIGENA", "EPOC", "ASMA", "INMUSUPR", 
                                           "HIPERTENSION", "OBESIDAD", "TABAQUISMO", 
                                           "d_intervention_T2")]), ]

complete_cases_red <- df[complete.cases(df[, c("d_days_to_death", "d_death", "EDAD_piecewise", 
                                           "NEUMONIA", "RENAL_CRONICA", "d_pre_post", 
                                           "CARDIOVASCULAR", "OTRA_COM", "SEXO",
                                           "ASMA", "INMUSUPR", 
                                           "HIPERTENSION", "OBESIDAD",
                                           "d_intervention_T2")]), ]
###


###
# Formula:
mod_form_full <- as.formula(Surv(d_days_to_death, d_death) ~
                           EDAD_piecewise +
                           strata(NEUMONIA) +
                           strata(RENAL_CRONICA) +
                           strata(d_pre_post) +
                           strata(CARDIOVASCULAR) +
                           OTRA_COM +
                           SEXO +
                           NACIONALIDAD + # removing as non-significant, parsimony
                           INDIGENA + # removing as non-significant, parsimony
                           EPOC + # removing as non-significant, parsimony
                           ASMA + INMUSUPR +
                           HIPERTENSION +
                           OBESIDAD +
                           TABAQUISMO + # removing as non-significant, parsimony
                           d_intervention_T2 * d_pre_post
                       )

print(mod_form)

# Fit:
various_mods_full <- epi_stats_run_coxph(complete_cases_full, mod_form_full)
various_mods_full$cox_did_covar
various_mods_full$ph_test
###


###
# Formula:
mod_form_red <- as.formula(Surv(d_days_to_death, d_death) ~
                           EDAD_piecewise +
                           strata(NEUMONIA) +
                           strata(RENAL_CRONICA) +
                           strata(d_pre_post) +
                           strata(CARDIOVASCULAR) +
                           OTRA_COM +
                           SEXO +
                           # NACIONALIDAD + # removing as non-significant, parsimony
                           # INDIGENA + # removing as non-significant, parsimony
                           # EPOC + # removing as non-significant, parsimony
                           ASMA + INMUSUPR +
                           HIPERTENSION +
                           OBESIDAD +
                           # TABAQUISMO + # removing as non-significant, parsimony
                           d_intervention_T2 * d_pre_post
                       )

print(mod_form_red)

# Fit:
various_mods_red <- epi_stats_run_coxph(complete_cases_red, mod_form_red)
###


###
# Compare AIC of both models
AIC(various_mods_full$cox_did_covar, various_mods_red$cox_did_covar)

# Likelihood Ratio Test to compare models
anova(various_mods_full$cox_did_covar,
      various_mods_red$cox_did_covar,
      test = "LRT"
      )
###
