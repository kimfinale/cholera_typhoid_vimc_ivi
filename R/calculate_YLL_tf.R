#' Calculate years of life lost (YLL)
#'
#' The \code{calculate_YLL_tf()} calculates YLL for typhoid fever
#' @param deaths The number of deaths due to the disease of interest
#' @param life_expectancy Life expectancy at the time of death in years
#' @export
#' @examples
#' calculate_YLL(cases = cases)
calculate_YLL_tf <- function (cases = NULL,
                              country = NULL,
                              year = 2000:2100,
                              life_expectancy = NULL,
                              case_fatality = NULL,
                              discount_rate = NULL) {

  # if(!exists("life_expectancy")) {
  #   life_expectancy <- data.table::fread(life_expectancy_data)
  # }
  if (is.null(country)) {
    stop("Country name must be provided")
  }
  if (is.null(cases)) {
    stop("Cases must be provided")
  }
  if (is.null(life_expectancy)) {
    life_expectancy_data <-
      data.table::fread("inst/extdata/202310gavi-4_dds-202208_life_ex_both.csv")
    life_expectancy_data[, country := clean_country_names(country)]
    yr <- year
    rm(year)
    life_expectancy <-
      get_life_expectancy(life_expectancy_data = life_expectancy_data,
                          country = cntry, year = yr)
  }

  # YLL <- as.data.frame(
  #   lapply(1:ncol(cases),
  #          function(x) cases[, x] * case_fatality * life_expectancy[, x]))

  # The above equation didn't account for the discounting
  # The equation below accounts for discounting solution is in the continuous version
  dr <- discount_rate
  YLL <- as.data.frame(
    lapply(1:ncol(cases),
           function(x) cases[, x] * case_fatality * (1/dr) *
             (1 - exp(-dr * life_expectancy[,x]))))

  names(YLL) <- names(cases)

  return (YLL)
}

