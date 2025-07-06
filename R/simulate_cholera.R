#' Run models to compute cohort size, cases, deaths, dalys
#'
#' The \code{simulate_tf()}
#' @param disease Target disease (i.e., cholera or typhoid)
#' @param country name of the country
#' @param year Year in which vaccination is implemented
#' @export
#' @examples
#'
simulate_cholera <- function(disease = "Cholera",
                        year = "2000:2100",
                        country = NULL,
                        params_list = NULL,
                        vacc_scenario = NULL,
                        vacc_cov_data = NULL,
                        cohort = NULL,
                        wash_prop = NULL,
                        wash_risk_ratio = NULL,
                        life_expectancy_data = NULL,
                        vacc_indirect_effect = NULL,
                        run_ids = NULL) {

  stoch_output <- list()
  stoch_parameters <- list()
  central_output <- list()

  vacc_scenario_save <- vacc_scenario

  ncountry <- length(country)
  vacc_ind_eff <- vacc_indirect_effect

  # k <- 1
  for (k in seq_along(country)) {
    cntry <- country[k]

    cohort_size <- list()
    cases <- list()
    deaths <- list()
    dalys <- list()
    yll <- list() # added for the 2023 runs

    life_exp <-
      get_life_expectancy(life_expectancy_data = life_expectancy_data,
                          country = cntry,
                          year = eval(parse(text = year)))

    message(paste0("vaccine scenario = ", vacc_scenario_save,
                   ", country = ", cntry, ", ", k, "/", ncountry))
    ## country-specific parameters
    params <- params_list[[cntry]] #

    # i <- 1
    for (i in seq_along(run_ids)) {
      # cat("i = ", i, "\n")
      # disease = dis
      # country = cntry
      # population_data = population_data
      # cohort = cohort[[cntry]]
      # wash_prop = wash_prop
      # wash_risk_ratio = wash_risk_ratio
      # vaccine_coverage_data = vacc_cov_data
      # vacc_scenario = vacc_scenario
      # life_expectancy = life_exp
      # year = eval(parse(text = year))
      # parameter_sample = params[i,]
      # vacc_indirect_effect = vacc_ind_eff

      tic <- Sys.time()
      res <- vaccine_impact_cholera(disease = dis,
                               country = cntry,
                               population_data = population_data,
                               cohort = cohort[[cntry]],
                               wash_prop = wash_prop,
                               wash_risk_ratio = wash_risk_ratio,
                               vaccine_coverage_data = vacc_cov_data,
                               vacc_scenario = vacc_scenario,
                               life_expectancy = life_exp,
                               year = eval(parse(text = year)),
                               parameter_sample = params[i,],
                               vacc_indirect_effect = vacc_ind_eff)

      # message(paste0(Sys.time() - tic, " elapsed for vaccine_impact_cholera"))

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

    # disease = dis
    # country = cntry
    # sim_output = stoch

    stoch_output[[k]] <-
      vimc_stoch_report(disease = dis, country = cntry, sim_output = stoch,
                        run_ids = run_ids)

    names(stoch$val) <- stoch$var
    central_output[[k]] <- vimc_central_report(disease = dis, country = cntry,
                                               sim_output = stoch$val)
  }
  return(output = list(stoch = stoch_output, central = central_output))
}
