#' Calculate years of life lost (YLL)
#'
#' The \code{calculate_YLL()} calculatess YLL
#' @param deaths The number of deaths due to the disease of interest
#' @param life_expectancy Life expectancy at the time of death in years
#' @export
#' @examples
#' calculate_YLL(cases = cases)
calculate_YLL <- function (disease = NULL,
                           cases = NULL,
                           country = NULL,
                           year = NULL,
                           life_expectancy = NULL,
                           case_fatality = NULL) {

  # if(!exists("life_expectancy")) {
  #   life_expectancy <- data.table::fread(life_expectancy_data)
  # }
  dis <- disease
  rm(disease)
  if (is.null(country)) {
    stop("Country name must be provided")
  }
  if (is.null(cases)) {
    stop("Cases must be provided")
  }
  if (is.null(life_expectancy)) {
    life_expectancy_data <-
      fread("inst/extdata/201910gavi-5_dds-201910_2_life_ex_both.csv")
    yr <- year
    rm(year)
    life_expectancy <-
      get_life_expectancy(life_expectancy_data = life_expectancy_data,
                          country = cntry, year = yr)
  }

  YLL <-
    as.data.frame(lapply(1:ncol(cases),
                         function(x) cases[, x] * case_fatality * life_expectancy[, x]))
  names(YLL) <- names(cases)

  return (YLL)
}

