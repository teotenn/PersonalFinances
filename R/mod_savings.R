#' savings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_savings_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6 ,
             h2("Input parameters"),
             fluidRow(
               column(6, textInput(NS(id, "initial_amount"), "Initial Amount", value = "0", placeholder = "$"))
             ),
             fluidRow(
               column(6, textInput(NS(id, "int_rate"), "Interest Rate", value = "3", placeholder = "%"))
             ),
             fluidRow(
               column(6, selectInput(NS(id, "int_return"),
                                     "Interest Return",
                                     choices = list("Annual" = 1, "6 Months" = 2,
                                                    "3 Months" = 3, "Monthly" = 4),
                                     selected = 1))
             ),
             fluidRow(
               column(6, textInput(NS(id, "goal"), "Savings Goal", value = "1000", placeholder = "$"))
             ),
             fluidRow(
               column(4,
                      h3("Time to reach"),
                      textInput(NS(id, "reach_years"), "Years", value = "0"),
                      textInput(NS(id, "reach_months"), "Months", value = "0")
                      ),
               column(4,
                      h3("Time to reach"),
                      textInput(NS(id, "add_monthly"), "Amount", value = "0")
                      )
             ),
             fluidRow(
               column(4, actionButton(NS(id, "calc_amount"), "Calculate Monthly Needed")),
               column(4, actionButton(NS(id, "calc_time"), "Calculate Time"))
             )
             ),
      column(6 ,
             h3("Results"),
             fluidRow(
               textOutput(NS(id, "results")),
               plotOutput(NS(id, "pSavings"))
             )
             )
    )
  )
}

#' savings Server Functions
#'
#' @noRd
mod_savings_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    init_amount <- reactive(as.numeric(input$initial_amount))
    add_monthly <- reactive(as.numeric(input$add_monthly))
    goal <- reactive(as.numeric(input$goal))
    int_rate <- reactive(as.numeric(input$int_rate))
    t_years <- reactive(as.numeric(input$reach_years))
    t_months <- reactive(as.numeric(input$reach_months))

    int_return <- reactive({
      switch(as.numeric(input$int_return),
             12, 6, 3, 1)
    })

    observeEvent(input$calc_amount, {
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
