test_that("mod_savings_server works", {
  testServer(mod_savings_server, {
    ns <- session$ns
    expect_true(
      inherits(ns, "function")
    )
    expect_true(
      grepl(id, ns(""))
    )
    expect_true(
      grepl("test", ns("test"))
    )

    ## scenario as in test-fct_savings.R - "estimate_time works"
    session$setInputs(
      initial_amount = "1000",
      goal = "5000",
      reach_years = "2",
      reach_months = "0",
      int_rate = "1",
      int_return = 4
    )

    expect_equal(init_amount(), 1000)
    expect_equal(goal(), 5000)
    expect_equal(t_years(), 2)
    expect_equal(int_rate(), 1)   
    session$setInputs(calc_amount = 1)
    expect_equal(output$results, "You need $148.29 each month to reach your goal.")

    ## scenario as in test-fct_savings.R - "estimate_monthly works"
    ## And testing reactivity notc hanging pre-set values
    session$setInputs(
      initial_amount = "0",
      goal = "10000",
      add_monthly = "1000"
    )

    session$setInputs(calc_time = 1)
    expect_equal(output$results, "You will reach your goal in 0 years and 11 months.")
  })
})


test_that("module ui works", {
  ui <- mod_savings_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_savings_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})

