#' savings
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
tabulate_growth <- function(df, goal, monthly_income, interest, frequency) {
  time <- df$Month[nrow(df)]
  money <- df$Amount[nrow(df)]

  if (money >= goal) {
    return(df)
  } else if(time %% frequency == 0) {
    df <- rbind(df, c(time + 1, calculate_increase(money, monthly_income, interest)))
    tabulate_growth(df, goal, monthly_income, interest, frequency)
  } else {
    df <- rbind(df, c(time + 1, money + monthly_income))
    tabulate_growth(df, goal, monthly_income, interest, frequency)
  }
}


estimate_time <- function(initial_amount, goal, monthly_income, interest, frequency) {
  data <- data.frame(Month = 1, Amount = initial_amount)
  data <- tabulate_growth(data, goal, monthly_income, interest, frequency)
  tYears <- floor(data$Month[nrow(data)] / 12)
  tMonths <- data$Month[nrow(data)] - (tYears * 12)
  estimated_time <- paste(tYears, "years and", tMonths, "months", sep = " ")
  explanatory_text <- glue::glue("You will reach your goal in {estimated_time}.")
  return(list(data = data, text = explanatory_text))
}


calculate_monthly_needed <- function(initial_amount, goal, t_years, t_months, interest, frequency) {
  t_in_months <- t_months + (t_years * 12)
  int_ratio <- (interest / 100) / frequency
  results <- ((goal - initial_amount) * int_ratio) /  (((1 + int_ratio)^t_in_months) - 1)
  return(results)
}


estimate_monthly <- function(initial_amount, goal, t_years, t_months, interest, frequency) {
  monthly_needed <- calculate_monthly_needed(initial_amount, goal, t_years, t_months, interest, frequency)
  data <- data.frame(Month = 1, Amount = initial_amount)
  data <- tabulate_growth(data, goal, monthly_needed, interest, frequency)
  explanatory_text <- glue::glue("You need ${round(monthly_needed, 2)} each month to reach your goal.")
  return(list(data = data, text = explanatory_text))
}
