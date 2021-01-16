#' Population size by age (0-120) and year (1885-2100).
#'
#' A dataset containing the population size (UN projections) by one-year age group up to 100 and 100-120 yo and country.
#'
#' @format A data.table and data.frame:	19695 obs. of  222 variables:
#' \describe{
#'   \item{country }{country name}
#'   \item{age_from}{starting age}
#'   \item{age_to}{ending age}
#'   \item{gender}{gender}
#'   ...
#' }
#' @source \url{https://population.un.org/wpp/}
"gavi201910_int_pop_both"




#' Proportion of population who have access to basic sanitation services
#' Estimates based on 2000-2017 data from WHO/UNICEF JMP.
#' Estimatrers were fit to a saturating exponential function: f(x; k, tau) = 1 - exp (-k(x-tau))
#'
#' @format 'data.frame':	405 obs. of  3 variables:
#' \describe{
#'   \item{country }{country name}
#'   \item{year}{starting age}
#'   \item{pred}{estimates for the proportion of population who have access to basic sanitation services}
#'   ...
#' }
#' @source \url{https://washdata.org/data/downloads#WLD}
"prop_basic_san"
