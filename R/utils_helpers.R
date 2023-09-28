#' helpers 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
calculate_increase <- function(initial_value, increase, interest) {
  result <- initial_value + increase + ((initial_value + increase) * (interest / 100))
  return(result)
}
