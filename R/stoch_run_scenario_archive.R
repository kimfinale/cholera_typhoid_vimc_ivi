#' Vaccinate population for a set of condition

#' @param vacc_input vaccine coverage information in the long format
#' @param country name of the country
#' @param year Year in which vaccination is implemented
#' @export
#' @examples
#' pop <- setup_population(); compute_cases(pop)
stoch_run_scenario <- function(disease = NULL,
                               country = NULL,
                               vacc_scenario = NULL,
                               runs = 30,
                               year = 2000:2100){

  if (is.null(vacc_scenario)) {
    stop("vacc_scenrio must be provided")
  }

  if (!tolower(vacc_scenario) %in% c("novacc", "routine", "campaign")){
    stop("vacc_scenrio must be one of the following: novacc, routine, or campaign")
  }
  if (is.null(disease)) {
    stop("Disease name must be provided: cholera or typhoid")
  }
  cat( "disease =", disease, ", vacc scenario =", vacc_scenario, "\n")
  dis <- disease
  rm(disease)
  cntry <- country
  rm(country)
  stoch_param <- data.frame(
    run_id = rep(1:runs, length(cntry)),
    country = unlist(lapply(cntry, function(x) rep(x, runs))),
    ir = NA)
  index <- 1
  for (tg in cntry) {
    cat( "country =", tg, "\n")
    mean_ir <- incidence_rate[tolower(disease) == tolower(dis) & country == tg, incidence_rate_100Kpyo]
    irs <- rpois(runs, lambda = mean_ir)
    pop <- setup_cohorts(country = tg)
    if (tolower(vacc_scenario) == "routine"){
      vc <- create_vaccine_coverage(country = tg, disease = dis, routine = T)
      vp <- calculate_vaccine_protected(disease = dis, country = tg, population = pop, vacc_coverage = vc)
      pop <- pop - vp
    }
    else if (tolower(vacc_scenario) == "campaign") {
      vc <- create_vaccine_coverage(country = tg, disease = dis, routine = F)
      vp <- calculate_vaccine_protected(disease = dis, country = tg, population = pop, vacc_coverage = vc)
      pop <- pop - vp
    }

    cohort_size <- setup_cohorts(country = tg, rel_risk_low = 1.0) # cohort size is assumed to be the total population

    cases_list <- list()
    dalys_list <- list()
    deaths_list <- list()
    cfr <- parameters[tolower(disease) == tolower(dis) & definition == "case fatality ratio", value] #case fatality rate
    for (i in seq_len(runs)) {
      # cat("mean ir =", mean_ir, ", ir =", irs[i], "\n")
      stoch_param$ir[index] <- irs[i] # record keeping for incidence rates
      index <- index + 1
      cases <- calculate_cases(disease = dis, population = pop, country = tg, ir = irs[i])
      cases_list[[i]] <- cases
      deaths_list[[i]] <- cases * cfr
      dalys_list[[i]] <- calculate_DALY(disease = dis, cases = cases, country = tg)

    }
    stoch_output <- list()

    stoch_output$var <- c("cohort_size", "cases", "deaths", "dalys")
    stoch_output$val[[1]] <- cohort_size
    stoch_output$val[[2]] <- cases_list
    stoch_output$val[[3]] <- deaths_list
    stoch_output$val[[4]] <- dalys_list

    saveRDS(stoch_output, paste0("data/stoch_output_", dis, "_", tg, "_", vacc_scenario, ".rds"))

    cohort_size <- vimc_report(disease = dis, country = tg, sim_output = cohort_size, value_name = "cohort_size")
    cases <- vimc_report(disease = dis, country = tg, sim_output = cases_list, value_name = "cases")
    deaths <- vimc_report(disease = dis, country = tg, sim_output = deaths_list,value_name = "deaths")
    dalys <- vimc_report(disease = dis, country = tg, sim_output = dalys_list, value_name = "dalys")

    report <- cbind(cohort_size, cases = cases[,ncol(cases)], deaths = deaths[,ncol(deaths)], dalys = dalys[, ncol(dalys)])
    column_order <- c("disease", "year", "age", "country", "country_name", "cohort_size", "cases", "deaths", "dalys")
    report <- dplyr::relocate(report, any_of(column_order))

    saveRDS(report, paste0("data/report_", dis, "_",  tg, "_", vacc_scenario,  ".rds"))
  }
  saveRDS(stoch_param, paste0("outputs/stoch_param_", dis, "_", vacc_scenario, ".rds"))

}
