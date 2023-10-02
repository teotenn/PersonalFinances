#' @title warn if not numeric
#' @author Manuel Teodoro
#' @description Triggers \code{shinyFeedback::showFeedbackWarning} if a text value is not numeric equivalent
#'
#' @param reactive_input The input defined in the UI
#' @param default_value The lavue to return if the input is not numeric equivelant
#' @param msg The message to print in the UI
#' @param color The color of the message
#'
#' @return The value as numeric, or whatever it is specified in \code{default_value}
#'
#' @details The function takes the input from the UI, therefore it requires to be
#' evaluated in a reactive environment, and needs
#' \code{shinyFeedback::useShinyFeedback(),} to be called in the UI.
#'
#' @noRd
warn_if_not_numeric <- function(reactive_input,
                                input_id,
                                default_value = NULL,
                                msg = "The value should be a number!",
                                color = "red") {
  exit_value <- as.numeric(reactive_input)
  if (is.na(exit_value)) {
    shinyFeedback::showFeedbackWarning(input_id, "Provide a Number!", color = "red")
    return(default_value)
  } else {
    shinyFeedback::hideFeedback(input_id)
    return(exit_value)
  }
}
