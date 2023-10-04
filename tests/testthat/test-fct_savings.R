test_that("datbulate_growth works", {
  initial_df <- data.frame(Month = 1, Amount = 0)
  tabulated_results <- tabulate_growth(initial_df, 10000, 1000, 1, 1)
  expect_s3_class(tabulated_results, "data.frame")
  expect_equal(max(tabulated_results$Month), 11)
  expect_equal(round(max(tabulated_results$Amount), 0), 10567)
})


test_that("dabulate_growth errors", {
  initial_df <- data.frame(month = 1, amount = 0)
  expect_error(tabulate_growth(initial_df, 10000, 1000, 1, 1))
  expect_error(tabulate_growth(data.frame(), 10000, 1000, 1, 1))
  expect_error(tabulate_growth(data.frame(Month = 1, Amount = 0), "A", 10, 1, 1))
  expect_error(tabulate_growth(data.frame(Month = 1, Amount = 0), -100, 10, 1, 1))
  expect_error(tabulate_growth(data.frame(Month = 1, Amount = 0), 100, -10, 1, 1))
  expect_error(tabulate_growth(data.frame(Month = 1, Amount = 0), 100, 10, -1, 1))
  expect_error(tabulate_growth(data.frame(Month = 1, Amount = 0), 100, 10, 1, -1))
})


test_that("calculate_monthly_needed works", {
  expect_equal(round(calculate_monthly_needed(100, 200, 0, 10, 1, 12), 2), 9.96)
})


test_that("calculate_monthly_needed error", {
  expect_error(calculate_monthly_needed("A", 100, 0, 10, 1, 12))
  expect_error(calculate_monthly_needed(100, "A", 0, 10, 1, 12))
  expect_error(calculate_monthly_needed(100, 100, "A", 10, 1, 12))
  expect_error(calculate_monthly_needed(100, 100, 0, "A", 1, 12))
  expect_error(calculate_monthly_needed(100, 100, 0, 10, "A", 12))
  expect_error(calculate_monthly_needed(100, 100, 0, 10, 1, "A"))
})


test_that("estimate_time works", {
  results <- estimate_time(0, 10000, 1000, 1, 1)
  expect_s3_class(results$data, "data.frame")
  expect_equal(max(results$data$Month), 11)
  expect_equal(round(max(results$data$Amount), 0), 10567)
  expect_identical(results$text, "0 years and 11 months")
})


test_that("estimate_monthly works", {
  results <- estimate_monthly(1000, 5000, 2, 0, 1, 1)
  expect_s3_class(results$data, "data.frame")
  expect_s3_class(results$text, "character")
  expect_equal(max(results$data$Month), 24)
  expect_equal(round(max(results$data$Amount), 0), 5109)
  expect_identical(results$text, "$148.29 each month")
})
