
umsatzanzeige_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        # Umsatzanzeige tab with DT output
        column(12, DTOutput(ns("umsatzanzeige")))
      )
    )
  )
}