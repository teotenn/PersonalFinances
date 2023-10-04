#' footer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_footer_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$footer(
      class = "footer-item",
      div(class = "footer-items",
          fluidRow(
            align = "center",
            column(
              6,
              h5("Manuel Teodoro Tenango")
            ),
            column(
              6,
              h5("Contact"),
              p("teotenn@proton.me")
            )
          ))
    )
  )
}

#' footer Server Functions
#'
#' @noRd
mod_footer_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_footer_ui("footer_1")

## To be copied in the server
# mod_footer_server("footer_1")
