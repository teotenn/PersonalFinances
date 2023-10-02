#' savings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_savings_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyFeedback::useShinyFeedback(),
    
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          ## column(6 ,
          h3("Input parameters", align = "center"),
          br(),
          fluidRow(
            align = "center",
            column(
              12,
              textInput(NS(id, "initial_amount"), "Initial Amount",
                        value = "0", placeholder = "$", width = "50%")
            )
          ),
          fluidRow(
            align = "center",
            column(
              12,
              textInput(NS(id, "int_rate"), "Interest Rate",
                        value = "3", placeholder = "%", width = "50%"))
          ),
          fluidRow(
            align = "center",
            column(
              12,
              selectInput(NS(id, "int_return"),
                          "Interest Return",
                          choices = list("Annual" = 1, "6 Months" = 2,
                                         "3 Months" = 3, "Monthly" = 4),
                          selected = 1, width = "50%")
            )
          ),
          fluidRow(
            align = "center",
            column(
              12,
              textInput(NS(id, "goal"), "Savings Goal",
                        value = "1000", placeholder = "$", width = "50%")
            )
          ),
          fluidRow(
            align = "center",
            column(6,
                   h3("Time to reach", align = "center",),
                   textInput(NS(id, "reach_years"), "Years", value = "0"),
                   textInput(NS(id, "reach_months"), "Months", value = "0")
                   ),
            column(6,
                   h3("Monthly to save", align = "center",),
                   textInput(NS(id, "add_monthly"), "Amount", value = "0")
                   )
          ),
          fluidRow(
            align = "center",
            column(6, actionButton(NS(id, "calc_amount"), "Calculate Monthly Needed", class = "btn-primary")),
            column(6, actionButton(NS(id, "calc_time"), "Calculate Time", class = "btn-primary"))
             )
        ),
        mainPanel(
      ## column(6 ,
          h3("Results", align = "center",),
          fluidRow(
            tags$p(
              "You need ",
              tags$b(textOutput(NS(id, "results"), inline = TRUE)),
              " to reach your goal."
            ),
            plotOutput(NS(id, "pSavings"))
          )
        )
      )
    )
  )
}


#' savings Server Functions
#'
#' @noRd
mod_savings_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    init_amount <- reactive(warn_if_not_numeric(input$initial_amount, "initial_amount"))
    t_years <- reactive(warn_if_not_numeric(input$reach_years, "reach_years"))
    t_months <- reactive(warn_if_not_numeric(input$reach_months, "reach_months"))

    add_monthly <- reactive({
      x <- warn_if_not_numeric(input$add_monthly, "add_monthly")
      shinyFeedback::feedbackWarning("add_monthly", x <= 0, "Value should be more than 0")
      req(x > 0)
      x
    })
    goal <- reactive({
      x <- warn_if_not_numeric(input$goal, "goal")
      shinyFeedback::feedbackWarning("goal", x <= 0, "Value should be more than 0")
      req(x > 0)
      x
    })
    int_rate <- reactive({
      x <- warn_if_not_numeric(input$int_rate, "int_rate")
      shinyFeedback::feedbackWarning("int_rate", x <= 0, "Value should be more than 0")
      req(x > 0)
      x
    })


    int_return <- reactive({
      switch(as.numeric(input$int_return),
             12, 6, 3, 1)
    })

    observeEvent(input$calc_amount, {
      req(init_amount(), goal(), t_years(), t_months(), int_rate(), int_return())
      results <- estimate_monthly(
        init_amount(),
        goal(),
        t_years(),
        t_months(),
        int_rate(),
        int_return()
      )
      output$results = renderText(results$text)
      output$pSavings = renderPlot(
        plot(results$data$Month, results$data$Amount, xlab = "Months", ylab = "$"))
    })

    observeEvent(input$calc_time, {
      req(init_amount(), goal(), add_monthly(), int_rate(), int_return())
      results <- estimate_time(
        init_amount(),
        goal(),
        add_monthly(),
        int_rate(),
        int_return()
      )
      output$results = renderText(results$text)
      output$pSavings = renderPlot(
        plot(results$data$Month, results$data$Amount, xlab = "Months", ylab = "$"))
    })

  })
}

## To be copied in the UI
## mod_savings_ui("savings_1")

## To be copied in the server
## mod_savings_server("savings_1")
