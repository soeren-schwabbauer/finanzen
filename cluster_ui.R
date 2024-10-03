cluster_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(6, radioButtons(ns("income_expenses"), label = "Auswahl begrenzen auf", choices = c("Ausgaben", "Einnahmen", "beide"))),
        column(6, sliderInput(ns("n_gruppen"), "Anzahl Gruppen", min = 1, max = 10, value = 4))
      ),
      fluidRow(
        column(6, plotOutput(ns("cluster_gruppen_boxplot"))),  # Hierarchical highchart table for income
        column(6, DTOutput(ns("cluster_subgruppen_count")))
      ),
      fluidRow(
        column(6, plotOutput(ns("cluster_gruppen_count")))
      )
    )
  )
}
