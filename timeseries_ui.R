timeseries_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(6, radioButtons(ns("income_expenses"), 
                               label = "Auswahl begrenzen auf", 
                               choices = c("beide", "Einnahmen", "Ausgaben"),
                               inline = TRUE)),
        column(12, plotOutput(ns("plot_ts"))),
        column(12, plotOutput(ns("plot_decomposed"))),
        column(12, plotOutput(ns("plot_arima")))
        
        )
      )
    )
}