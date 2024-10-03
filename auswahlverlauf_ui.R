
auswahlverlauf_ui <- function(id) {
    ns <- NS(id) 
    tagList(
      fluidPage(
        fluidRow(
          column(3,
                 checkboxInput(ns("show_bars"), label = "Einnahmen und Ausgaben einblenden", value = TRUE)
          ),
          column(3,
                 checkboxInput(ns("show_surplus_deficit"), label = "Überschuss/Defizit anzeigen", value = FALSE) 
          ),
          column(3, 
                 checkboxInput(ns("aggregate_savings"), label = "Überschuss/Defizit aggregieren", value = FALSE)
          ),
          column(3,
                 checkboxInput(ns("rolling_average"), label = "3-Monats-Mittel anzeigen", value = FALSE)  
          )
        ),
        
        fluidRow(
          column(12, 
                 highchartOutput(ns("income_vs_expenses_plot"))
          )
        )
      )
    )
  
}
