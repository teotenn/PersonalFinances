test_that("calculate_increase works", {
  expect_equal(calculate_increase(1000, 1000, 10), 2200)
})

test_that("calculate_increase error", {
  expect_error(calculate_increase(1000, 1000, "10%"))
})
