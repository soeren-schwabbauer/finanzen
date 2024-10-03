
umsatzanzeige_server <- function(input, output, session, data, rv) {
  
  ns <- session$ns
  
  # Render editable DT for combined data
  output$umsatzanzeige <- renderDT({
    data() %>%
      rename(
        Datum = datum,
        Gegenseite = gegenseite,
        Verwendungszweck = verwendungszweck,
        `Betrag - angepasst` = betrag_edited,
        `Betrag - unverändert` = betrag) %>%
      select(-konto) %>%
      
      datatable(editable = TRUE) 
  })
  
  # Automatically save changes to the appropriate CSV files on edit
  observeEvent(input$umsatzanzeige_cell_edit, {
    info <- input$umsatzanzeige_cell_edit
    rv$combined_data[info$row, info$col] <- info$value
    
    # Determine the konto of the edited row
    konto <- rv$combined_data$konto[info$row]
    
    # Save the changes back to the appropriate CSV file
    if (konto == "ING") {
      write.csv(rv$combined_data %>% filter(konto == "ING"), "data/umsatzanzeige_ING.csv", row.names = FALSE)
      showNotification("ING data saved!", type = "message")
    } else if (konto == "TradeRepublik") {
      write.csv(rv$combined_data %>% filter(konto == "TradeRepublik"), "data/umsatzanzeige_TradeRepublik.csv", row.names = FALSE)
      showNotification("TradeRepublik data saved!", type = "message");
    } else if (konto == "Sparbrief") {
      write.csv(rv$combined_data %>% filter(konto == "Sparbrief"), "data/umsatzanzeige_Sparbrief.csv", row.names = FALSE);
      showNotification("Sparbrief data saved!", type = "message");
    }
  })
  
}