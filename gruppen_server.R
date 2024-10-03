# gruppen_server.R
gruppen_server <- function(input, output, session, gruppen_data) {
  
  # Reactive value to hold the groups data
  gruppen <- reactiveVal(gruppen_data())
  
  # Render the groups table
  output$gruppen_table <- renderDT({
    DT::datatable(gruppen())
  })
  
  # Add new group
  observeEvent(input$add_group, {
    new_group <- data.frame(
      string = input$new_string,
      spalte = input$new_spalte,
      kategorie = input$new_kategorie,
      name = input$new_name,
      stringsAsFactors = FALSE
    )
    
    # Append the new group to the existing data
    updated_gruppen <- rbind(gruppen(), new_group)
    gruppen(updated_gruppen)
    
    # Optionally, write the updated groups back to the CSV
    write.csv(updated_gruppen, "data/gruppen.csv", row.names = FALSE)
    
    # Clear the input fields
    updateTextInput(session, "new_string", value = "")
    updateTextInput(session, "new_spalte", value = "")
    updateTextInput(session, "new_kategorie", value = "")
    updateTextInput(session, "new_name", value = "")
  })
  
  return(gruppen)  # Return the updated groups reactive
}
