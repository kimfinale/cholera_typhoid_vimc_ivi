#' Run models to compute cohort size, cases, deaths, dalys
#'
#' The \code{simulate_tf()}
#' @param disease Target disease (i.e., cholera or typhoid)
#' @param country name of the country
#' @param year Year in which vaccination is implemented
#' @export
#' @examples
#'
simulate_tf <- function(disease = "Typhoid",
                        year = "2000:2100",
                        country = NULL,
                        params_list = NULL,
                        vacc_scenario = NULL,
                        vacc_cov_data = NULL,
                        cohort = NULL,
                        run_ids = 1:30,
                        life_expectancy_data = NULL,
                        save_by_country = FALSE) {

  dis <- disease
  rm(disease)

  stoch_output <- list()
  stoch_parameters <- list()
  central_output <- list()

  ## vacc_scenarios have numbers that are not used in other functions
  vacc_scenario_save <- vacc_scenario
  vacc_scenario <- gsub('_[1-9]', '', vacc_scenario)

  ncountry <- length(country)
  # k <- 1
  for (k in seq_along(country)) {
    cntry <- country[k]
    # outputs required by the VIMC
    cohort_size <- list()
    cases <- list()
    dalys <- list()
    deaths <- list()
    yll <- list() # added for the 2023 runs

    life_exp <-
      get_life_expectancy(life_expectancy_data = life_expectancy_data,
                          country = cntry,
                          year = eval(parse(text = year)))

    message(paste0("vaccine scenario = ", vacc_scenario_save, ", country = ",
                   cntry, ", ", k, " of ", ncountry))
    ## country-specific parameters
    # params <- params_list[[k]] # country by index
    # selecting parameters by index can cause problems when only a subset of
    # target countries are used!!
    params <- params_list[[cntry]] # country by index
    # discount parameters temporarily set at 0.03
    # to check the following functions
    # params$params$discount_rate <- 0.03
    # for (i in run_ids) {
    # i <- 1
    for (i in seq_along(run_ids)) {
      parm_id <- run_ids[i]

      param_sample <- list()
      param_sample$vacc_efficacy <- params$params$vacc_efficacy[parm_id]
      param_sample$duration_vacc_protection <-
        params$params$duration_vacc_protection[parm_id]
      param_sample$indirect_vacc_efficacy <-
        params$params$indirect_vacc_efficacy[parm_id]
      param_sample$eff_vacc_cov_limit <-
        params$params$eff_vacc_cov_limit[parm_id]

      param_sample$illness_duration <-
        c(params$params$illness_duration_1[parm_id],
          params$params$illness_duration_2[parm_id],
          params$params$illness_duration_3[parm_id],
          params$params$illness_duration_4[parm_id])

      param_sample$prob_severity <-
        c(params$params$prob_severity_1[parm_id],
          params$params$prob_severity_2[parm_id],
          params$params$prob_severity_3[parm_id],
          params$params$prob_severity_4[parm_id])

      param_sample$disability_weight <-
        c(params$params$disability_weight_1[parm_id],
          params$params$disability_weight_2[parm_id],
          params$params$disability_weight_3[parm_id],
          params$params$disability_weight_4[parm_id])

      param_sample$incidence_rate <- params$params$incidence_rate[parm_id]

      ## ------------------------------------------------------------ ##
      ## edited on 9 May 2022
      param_sample$case_fatality_ratio <- # 101 age groups
        rep(c(params$params$case_fatality_ratio_1[parm_id],
              params$params$case_fatality_ratio_2[parm_id]), c(15, 86)) # <15 and 15+
      ##---------------------------------------------------------------
      # param_sample$case_fatality_ratio <- params$case_fatality_ratio[,i]

      ## edited on 14 March 2023 -----------------------------------------------
      param_sample$discount_rate <- params$params$discount_rate[parm_id]
      param_sample$reference_year <- params$params$reference_year[parm_id]
      ##------------------------------------------------------------------------

      # disease = dis
      # country = cntry
      # population_data = population_data
      # cohort = cohort[[cntry]]
      # vaccine_coverage_data = vacc_cov_data
      # vacc_scenario = vacc_scenario
      # year = eval(parse(text = year))
      # life_expectancy = life_exp
      # parameter_sample = param_sample

      res <- vaccine_impact_tf(disease = dis,
                               country = cntry,
                               population_data = population_data,
                               cohort = cohort[[cntry]],
                               vaccine_coverage_data = vacc_cov_data,
                               vacc_scenario = vacc_scenario,
                               year = eval(parse(text = year)),
                               life_expectancy = life_exp,
                               parameter_sample = param_sample)

      cohort_size[[i]] <- res$cohort_size
      cases[[i]] <- res$cases
      dalys[[i]] <- res$dalys
      deaths[[i]] <- res$deaths
      yll[[i]] <- res$yll

    }

    stoch <- list()
    stoch$var <- c("cohort_size", "cases", "dalys", "deaths", "yll")
    stoch$val[[1]] <- cohort_size[[1]] # cohort size remains constant across nruns
    stoch$val[[2]] <- cases
    stoch$val[[3]] <- dalys
    stoch$val[[4]] <- deaths
    stoch$val[[5]] <- yll

    stoch_res <-
      vimc_stoch_report(disease = dis, country = cntry, sim_output = stoch,
                        run_ids = run_ids)

    names(stoch$val) <- stoch$var
    central_res <- vimc_central_report(disease = dis, country = cntry,
                                               sim_output = stoch$val)

    if (save_by_country) {
      data.table::fwrite(stoch_res,
             paste0("outputs/stoch_", dis, "_", vacc_scenario, "_", cntry,
                    "_", tstamp(), ".csv"))
      data.table::fwrite(central_res,
                         paste0("outputs/central_", dis, "_", vacc_scenario, "_", cntry,
                                "_", tstamp(), ".csv"))
    }
    else {
      stoch_output[[k]] <- stoch_res

      names(stoch$val) <- stoch$var
      central_output[[k]] <- central_res
    }
  }
  return(output = list(stoch = stoch_output, central = central_output))
}
