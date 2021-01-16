
#' Get ISO3 codes for country names
#'
#' The \code{get_iso3()} is used to retrieve ISO3 codes for the country name of interest
#' population for both sexes and incidence rate
#' @param country The input country names
#' @export
#' @examples
#' iso3 <- get_iso3(country = "DRC")
#' # COd
get_iso3 <- function(country, lookup_table) {
  if(missing(lookup_table)){
    lookup_table <- country_name_table
  }

  target_row <- lookup_table$country == clean_country_names(country)
  lookup_table[target_row, ]$iso3
}
