#' Calculate years of life lost (YLL)
#'
#' The \code{calculate_YLL()} calculatess YLL
#' @param deaths The number of deaths due to the disease of interest
#' @param life_expectancy Life expectancy at the time of death in years
#' @export
#' @examples
#' calculate_YLL(cases = cases)
calculate_YLL <- function (cases = NULL,
                           disease = c("Typhoid", "Cholera"),
                           country = NULL,
                           year = 2000:2100,
                           case_fatality = NULL,
                           life_expectancy_data = "data/201910gavi-5_dds-201910_2_life_ex_both.csv") {

  if(!exists("life_expectancy")) {
    life_expectancy <- data.table::fread(life_expectancy_data)
  }
  dis <- disease
  rm(disease)
  if (is.null(country)) {
    stop("Country name must be provided")
  }
  if (is.null(cases)) {
    stop("Cases must be provided")
  }
  if (is.null(case_fatality)){
    case_fatality <- parameters[tolower(disease) == tolower(dis) & definition == "case fatality ratio", value]
    if (length(case_fatality) != 1){
      stop("Length is not 1. case fatality ratio must be uniquely determined")
    }
  }

  country_clean <- clean_country_names(country)
  rm(country)
  yr <- year
  rm(year)
  life_exp1 <- life_expectancy %>%
    dplyr::filter(country == country_clean, year %in% !!yr) %>%
    dplyr::select(age_from, year, value) %>%
    tidyr::pivot_wider(id_cols = c(age_from, year), names_from = year)

  life_exp2 <- life_exp1[c(1, rep(2,4), rep(3:21, each=5), 22), ] ## add rows
  life_exp2 <- life_exp2[,-1] # remove age column
  life_exp <- life_exp2[, c(rep(seq_len(ncol(life_exp2)-1), each=5), rep(ncol(life_exp2), 6)) ] # add columns
  names(life_exp) <- as.character(yr)

  if (sum(dim(cases) == dim(life_exp)) != 2) {
    stop("Dimensions do not match: cases and life expectancy")
  }
  if (length(case_fatality) == 1) {
    case_fatality <- rep(case_fatality, nrow(cases))
  }
  YLL <- as.data.frame(lapply(1:ncol(cases), function(x) cases[, x] * case_fatality * life_exp[, x]))
  names(YLL) <- names(cases)

  return (YLL)
}

