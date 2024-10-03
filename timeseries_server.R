
timeseries_server <- function(input, output, session, data) {
  
  ns <- session$ns
  
  data_timeseries <- reactive({
    
    data <- data()
    
    # filter für nur positiv/negativ
    if(input$income_expenses == "Einnahmen") data %<>% filter(betrag_edited > 0)
    if(input$income_expenses == "Ausgaben") data %<>% filter(betrag_edited < 0) %>% mutate(betrag_edited = abs(betrag_edited))
    
    
    data <- data %>%
      filter(!is.na(datum)) %>%
      mutate(datum = as.Date(datum), betrag_edited = as.numeric(betrag_edited)) %>%      
      group_by(datum) %>%
      summarise(betrag_edited = sum(betrag_edited)) %>%
      arrange(datum) %>%
      filter(!is.na(betrag_edited))
    
    return(data)
    
  })
  
  
  output$plot_ts <- renderPlot({
    
    data <- data_timeseries()
    
    plot(data$betrag_edited, type = "line")
    
  })
    
  
  output$plot_decomposed <- renderPlot({
    
    data_daily <- data_timeseries() %>%
      group_by(datum) %>%
      summarise(betrag_total = sum(betrag_edited))
  
    start_date <- min(data_daily$datum)
    end_date <- max(data_daily$datum)
    date_range <- seq.Date(from = start_date, to = end_date, by = "day")
    ts_data <- ts(data_daily$betrag_total, start = c(as.numeric(format(start_date, "%y")), as.numeric(format(start_date, "%m")), as.numeric(format(start_date, "%d"))), frequency = 30)
    
    decomposed_ts <- decompose(ts_data)
    
    plot(decomposed_ts)
    
  })
  
  output$plot_arima <- renderPlot({
    
    arima_model <- auto.arima(ts_data)
    forecast_data <- forecast(arima_model, h = 30)
    plot(forecast_data)
    
  })
  
}




