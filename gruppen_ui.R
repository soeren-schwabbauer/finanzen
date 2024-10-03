# gruppen_ui.R
gruppen_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3, textInput(ns("new_string"), "String")),
      column(3, textInput(ns("new_spalte"), "Spalte")),
      column(3, textInput(ns("new_kategorie"), "Kategorie")),
      column(3, textInput(ns("new_name"), "Name")),
      column(12, actionButton(ns("add_group"), "Add Group"))
    ),
    DTOutput(ns("gruppen_table"))
  )
}
