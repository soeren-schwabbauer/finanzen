aggregiert_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(12, checkboxInput(ns("average_per_month"), "Durchschnitt pro Monat anzeigen", value = FALSE)),
        column(6, highchartOutput(ns("category_table_income"))),
        column(6, highchartOutput(ns("category_table_expenses"))
        )
      )
    )
  )
}
