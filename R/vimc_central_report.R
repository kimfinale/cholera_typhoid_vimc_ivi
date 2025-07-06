#' VIMC report
#'
#' The \code{vimc_report()} takes the mean across stochastic runs and reshapes simulation results (e.g., data frame in the wide format or a list of wide-format data frames into the long format
#' @param template The VIMC template used
#' @param sim_output The population data of countries which includes the file
#' @export
#' @import tidyr
#' @examples
#' vimc_central_report()
#'
vimc_central_report <- function (disease = NULL,
                         country = NULL,
                         sim_output,
                         year = 2000:2100,
                         age = 0:100,
                         value_name) {

  cohort_size <- vimc_report(disease = disease, country = country,
                             sim_output = sim_output[["cohort_size"]],
                             value_name = "cohort_size")
  cases <- vimc_report(disease = disease, country = country,
                       sim_output = sim_output[["cases"]],
                       value_name = "cases")
  dalys <- vimc_report(disease = disease, country = country,
                       sim_output = sim_output[["dalys"]],
                       value_name = "dalys")

  deaths <- vimc_report(disease = disease, country = country,
                        sim_output = sim_output[["deaths"]],
                        value_name = "deaths")
  # for the 2023 runs
  yll <- vimc_report(disease = disease, country = country,
                       sim_output = sim_output[["yll"]],
                       value_name = "yll")

  report <- cbind(cohort_size,
                  cases = cases[, ncol(cases)],
                  dalys = dalys[, ncol(dalys)],
                  deaths = deaths[, ncol(deaths)],
                  yll = yll[, ncol(yll)])

  column_order <- c("disease", "year", "age", "country", "country_name",
                    "cohort_size", "cases", "dalys", "deaths", "yll")
  report <- dplyr::relocate(report, any_of(column_order))

  return(report)
}

