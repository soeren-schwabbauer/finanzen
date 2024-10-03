auswahlverlauf_server <- function(input, output, session, data) {
  
  ns <- session$ns
  
  # Process the incoming filtered data
  umsatzanzeige <- reactive({
    data() %>% add_groups()  # Add grouping logic based on passed filtered data
  })
  
  # Now use this in further calculations, such as monthly data
  monthly_data <- reactive({
    umsatzanzeige() %>%
      mutate(month = format(as.Date(datum), "%Y-%m")) %>%
      group_by(month) %>%
      summarise(
        income = sum(betrag_edited[betrag_edited > 0], na.rm = TRUE),
        expenses = sum(betrag_edited[betrag_edited < 0], na.rm = TRUE)
      ) %>%
      ungroup()
  })
  
  # Berechnung des 3-Monats-Mittels
  monthly_data_with_rolling_avg <- reactive({
    data <- monthly_data()
    
    if (input$rolling_average) {
      data <- data %>%
        arrange(month) %>%
        mutate(
          income_rolling = zoo::rollmean(income, 3, fill = NA, align = "right"),
          expenses_rolling = zoo::rollmean(expenses, 3, fill = NA, align = "right")
        )
    }
    
    data
  })
  
  # Render the Monthly Income vs. Expenses Plot with savings, 3-month average, and surplus/deficit
  output$income_vs_expenses_plot <- renderHighchart({
    summary_data <- monthly_data_with_rolling_avg()
    
    # Check if summary_data is not empty
    if (nrow(summary_data) == 0) {
      return(NULL)
    }
    
    # Convert month to Date object and then to timestamp in milliseconds
    summary_data <- summary_data %>%
      mutate(month = as.Date(paste0(month, "-01"), format = "%Y-%m-%d"),
             timestamp = as.numeric(as.POSIXct(month)) * 1000)  # Convert to milliseconds
    
    # Prepare data for Highcharts
    income_data <- data.frame(x = summary_data$timestamp, y = summary_data$income)
    expenses_data <- data.frame(x = summary_data$timestamp, y = -summary_data$expenses)
    
    # Berechne den Überschuss/Defizit für jeden Monat
    surplus_deficit_data <- data.frame(x = summary_data$timestamp, y = summary_data$income + summary_data$expenses)
    
    # Calculate savings or debt
    if (input$aggregate_savings) {
      savings_data <- cumsum(summary_data$income + summary_data$expenses)
    } else {
      savings_data <- summary_data$income + summary_data$expenses
    }
    
    # Daten für 3-Monats-Mittel hinzufügen (falls aktiviert)
    if (input$rolling_average) {
      income_rolling_data <- data.frame(x = summary_data$timestamp, y = summary_data$income_rolling)
      expenses_rolling_data <- data.frame(x = summary_data$timestamp, y = -summary_data$expenses_rolling)
    }
    
    # Create the Highchart plot
    chart <- highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(type = "datetime", title = list(text = "Monat"), labels = list(format = "{value:%b %Y}")) %>%
      hc_yAxis(title = list(text = "Betrag"), labels = list(format = "{value} €")) %>%
      hc_title(text = "Einnahmen vs. Ausgaben vs. Sparen/Schulden") %>%
      hc_tooltip(xDateFormat = "%B %Y", pointFormat = "<b>{point.y:.2f} €</b>") %>%
      hc_plotOptions(column = list(pointPadding = 0.1, groupPadding = 0.1, borderWidth = 0))
    
    # Balken Diagramm
    if (input$show_bars) {
      chart <- chart %>%
        hc_add_series(name = "Einnahmen", data = list_parse(income_data), color = "#00A651") %>%
        hc_add_series(name = "Ausgaben", data = list_parse(expenses_data), color = "#FF4E42");
    }
    
    # Überschuss/Defizit
    if (input$show_surplus_deficit) {
      chart <- chart %>%
        hc_add_series(name = "Überschuss/Defizit", data = list_parse(surplus_deficit_data), type = "line", color = "#FFA500");
    }
    
    # Sparen/Schulden-Linie
    if(input$aggregate_savings) {
      chart <- chart %>%
        hc_add_series(name = "Sparen/Schulden", data = list_parse(data.frame(x = summary_data$timestamp, y = savings_data)), type = "line", color = "#1E90FF");
    }
    
    # Füge die 3-Monats-Mittel für Einnahmen und Ausgaben hinzu (falls aktiviert)
    if (input$rolling_average) {
      chart <- chart %>%
        hc_add_series(name = "3-Monats-Mittel Einnahmen", data = list_parse(income_rolling_data), type = "line", color = "#32CD32", dashStyle = "Dash") %>%
        hc_add_series(name = "3-Monats-Mittel Ausgaben", data = list_parse(expenses_rolling_data), type = "line", color = "#DC143C", dashStyle = "Dash");
    }
    
    chart
  })
  
}