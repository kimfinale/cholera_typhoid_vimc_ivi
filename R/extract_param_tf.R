#' Calculates the incidence rate by risk group for a given WASH exposure such
#' as access to improved water or sanitation
#'
#' The \code{extract_param_tf()} extract parameter value (mean, lower, or upper bounds).
#' @param params_data Country
#' @param str  Variable name string
#' @param var_name One of the "value", "upper_bound", "lower_bound"
#' @export
#' @examples
#' extract_param_tf(params_data, str, var_name = "value")
#'

extract_param_tf <- function(params_data, str, var_name = "value"){
  var <- match.arg(var_name, c("value", "upper_bound", "lower_bound"))
  params <- rep(NA, length(str))
  # params_data %>% filter(tolower(disease) == tolower(dis)) -> dat
  params_data -> dat

  for (i in 1:length(params)) {
    dat %>%
      filter(definition == str[i]) %>%
      select(eval(var)) %>%
      unlist() -> params[i]
  }
  return(params)
}

