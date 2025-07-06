# stoch_param <- data.frame(
#   disease = dis,
#   country = cntry,
#   vacc_scenario = vacc_scenario,
#   year = year,
#   run_id = 1:nruns,
#   incidence_rate_ag0 = NA,
#   case_fatality_ratio_ag0 = NA,
#   odds_ratio = NA,
#   disability_weight_1 = NA,
#   disability_weight_2 = NA,
#   disability_weight_3 = NA,
#   disability_weight_4 = NA,
#   illness_duration_1 = NA,
#   illness_duration_2 = NA,
#   illness_duration_3 = NA,
#   illness_duration_4 = NA,
#   prob_severity_1 = NA,
#   prob_severity_2 = NA,
#   prob_severity_3 = NA,
#   prob_severity_4 = NA,
#   duration_vacc_protection = NA,
#   vacc_efficacy =  NA)
# stoch_param$run_id[i] = params[[3]]$run_id[i]
# stoch_param$incidence_rate_ag0[i] = params[[3]]$incidence_rate_ag0[i]
# stoch_param$case_fatality_ratio_ag0[i] = params[[3]]$case_fatality_ratio_ag0[i]
# stoch_param$odds_ratio[i] = params[[3]]$odds_ratio[i]
# stoch_param$disability_weight_1[i] = params[[3]]$disability_weight_1[i]
# stoch_param$disability_weight_2[i] = params[[3]]$disability_weight_2[i]
# stoch_param$disability_weight_3[i] = params[[3]]$disability_weight_3[i]
# stoch_param$disability_weight_4[i] = params[[3]]$disability_weight_4[i]
# stoch_param$illness_duration_1[i] = params[[3]]$illness_duration_1[i]
# stoch_param$illness_duration_2[i] = params[[3]]$illness_duration_2[i]
# stoch_param$illness_duration_3[i] = params[[3]]$illness_duration_3[i]
# stoch_param$illness_duration_4[i] = params[[3]]$illness_duration_4[i]
# stoch_param$prob_severity_1[i] = params[[3]]$prob_severity_1[i]
# stoch_param$prob_severity_2i] = params[[3]]$prob_severity_2[i]
# stoch_param$prob_severity_3[i] = params[[3]]$prob_severity_3[i]
# stoch_param$prob_severity_4[i] = params[[3]]$prob_severity_4[i]
# stoch_param$duration_vacc_protection[i] =
#   params[[3]]$duration_vacc_protection[i]
# stoch_param$vacc_efficacy[i] =
#   params[[3]]$vacc_efficacy[i]

# pop <- setup_cohorts(country = cntry,
#                             year = eval(parse(text = year)),
#                             population_data = population_data)
# vacc_cov <-
#   create_vaccine_coverage(disease = dis,
#                           country = cntry,
#                           population = pop,
#                           vaccine_coverage_data = vaccine_coverage_data,
#                           year = eval(parse(text = year)))


# for (i in seq_along(vacc_cov_one_two)) {
#   vacc_cov <- vacc_cov_one_two[[i]]
#   vacc_eff_by_year <- unlist(parameter_sample[paste0("vacc_eff_year_", 0:6)])
#   # convert VE such that it works for the single-dose efficacy
#   if (i == 1) {
#     ve2 <- parameter_data_cholera[definition ==
#                                   "vaccine efficacy",]$value
#     ve1 <- parameter_data_cholera[definition ==
#                                   "vaccine efficacy - single dose",]$value
#     vacc_eff_by_year = vacc_eff_by_year*ve1/ve2
#   }
#   vacc_protected <-
#     calculate_vaccine_protected(disease = dis,
#                                 country = cntry,
#                                 population = pop,
#                                 vaccine_coverage = vacc_cov,
#                                 vaccine_efficacy = NULL,
#                                 vaccine_immunity_duration = NULL,
#                                 exponential_decay = FALSE,
#                                 vaccine_efficacy_by_year = vacc_eff_by_year)
#   # vaccine recipients can be calculated by setting the vaccine efficacy to
#   # be 1 and vaccine_efficacy_by_year to be NULL, and duration of the vaccine
#   #-derived immunity lasts long (e.g., 1000 years)
#   vacc_recipients <-
#     calculate_vaccine_protected(disease = dis,
#                                 country = cntry,
#                                 population = pop,
#                                 vaccine_coverage = vacc_cov,
#                                 vaccine_efficacy = 1.0,
#                                 vaccine_immunity_duration = 1000,
#                                 exponential_decay = TRUE,
#                                 vaccine_efficacy_by_year = NULL)
#
#   # factor protection for vaccine recipients
#
#   pop_unprotected <- pop_unprotected - vacc_protected
#   if (sum(pop_unprotected < 0) > 0) {
#     message(i, " dose: direct protected > pop, set to zero")
#     pop_unprotected[pop_unprotected < 0] = 0
#   }
#   # vaccine coverage used to switch on/off indirect effect
#   pop_unvacc <- pop_unvacc - vacc_recipients
#   # vacc_cov_year <-
#   #   calc_vacc_cov_year(pop = pop, vacc_recipients = vacc_recipients)
#   eff_vacc_cov_year <-
#     calc_vacc_cov_year(pop = pop, vacc_recipients = vacc_protected)
#   if (i == 1 )  {
#     eff_vacc_cov_year = eff_vacc_cov_year*ve1/ve2
#   }
#   indirect_vacc_protected <-
#     calc_indirect_vacc_protected_cholera(population = pop_unvacc,
#                                          eff_vacc_cov = eff_vacc_cov_year)
#   # factor protection for unvaccinated population via indirect protection
#   pop_unprotected <- pop_unprotected - indirect_vacc_protected
#   if (sum(pop_unprotected < 0) > 0) {
#     message(i, " dose: indirect protected > pop, set to zero")
#     pop_unprotected[pop_unprotected < 0] = 0
#   }
# }
