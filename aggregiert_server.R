aggregiert_server <- function(input, output, session, data) {
  
  ns <- session$ns
  
  # Process the incoming filtered data
  umsatzanzeige <- reactive({
    data <- data() %>% add_groups()  # Add grouping logic based on passed filtered data
    
    return(data)
  })
  
  # Income by category
  income_by_category <- reactive({
    income_data <- umsatzanzeige() %>%
      filter(betrag_edited > 0) %>%
      group_by(kategorie) %>%
      summarise(total_income = sum(betrag_edited, na.rm = TRUE)) %>%
      ungroup()

    return(income_data)
  })
  
  # Expenses by category
  expenses_by_category <- reactive({
    expense_data <- umsatzanzeige() %>%
      filter(betrag_edited < 0) %>%
      mutate(betrag_edited = abs(betrag_edited)) %>%
      group_by(kategorie) %>%
      summarise(total_expenses = sum(betrag_edited, na.rm = TRUE)) %>%
      ungroup()

    return(expense_data)
  })
  
  # Aggregate data by category and subcategory for income
  aggregated_data_income <- reactive({
    income_data <- umsatzanzeige() %>%
      filter(betrag_edited > 0) %>%
      group_by(kategorie, name) %>%
      summarise(total_amount = sum(betrag_edited, na.rm = TRUE)) %>%
      ungroup()
    
    return(income_data)
  })
  
  # Aggregate data by category and subcategory for expenses
  aggregated_data_expenses <- reactive({
    expense_data <- umsatzanzeige() %>%
      filter(betrag_edited < 0) %>%
      mutate(betrag_edited = abs(betrag_edited)) %>%
      group_by(kategorie, name) %>%
      summarise(total_amount = sum(betrag_edited, na.rm = TRUE)) %>%
      ungroup()

    return(expense_data)
  })
  
  # Prepare hierarchical data for income highchart
  hierarchical_data_income <- reactive({
    aggregated <- aggregated_data_income()
    if (nrow(aggregated) == 0) return(NULL)
    
    categories <- unique(aggregated$kategorie)
    
    hierarchy <- lapply(categories, function(category) {
      subcategory_data <- aggregated %>% filter(kategorie == category)
      
      list(
        name = category,
        y = sum(subcategory_data$total_amount),
        drilldown = category
      )
    })
    
    drilldown_data <- lapply(categories, function(category) {
      subcategory_data <- aggregated %>% filter(kategorie == category)
      
      list(
        id = category,
        data = list_parse2(subcategory_data %>% select(name, total_amount))
      )
    })
    
    list(hierarchy = hierarchy, drilldown = drilldown_data)
  })
  
  # Prepare hierarchical data for expenses highchart
  hierarchical_data_expenses <- reactive({
    aggregated <- aggregated_data_expenses()
    if (nrow(aggregated) == 0) return(NULL)
    
    categories <- unique(aggregated$kategorie)
    
    hierarchy <- lapply(categories, function(category) {
      subcategory_data <- aggregated %>% filter(kategorie == category)
      
      list(
        name = category,
        y = sum(subcategory_data$total_amount),
        drilldown = category
      )
    })
    
    drilldown_data <- lapply(categories, function(category) {
      subcategory_data <- aggregated %>% filter(kategorie == category)
      
      list(
        id = category,
        data = list_parse2(subcategory_data %>% select(name, total_amount))
      )
    })
    
    list(hierarchy = hierarchy, drilldown = drilldown_data)
  })
  
  # Render hierarchical highchart for income
  output$category_table_income <- renderHighchart({
    data <- hierarchical_data_income()
    
    if (is.null(data)) {
      return(NULL)  # Prevent rendering if there's no data
    }
    
    highchart() %>%
      hc_chart(type = "pie") %>%
      hc_title(text = "Einnahmen Kategorien und Unterkategorien") %>%
      hc_plotOptions(
        series = list(
          dataLabels = list(enabled = TRUE, format = "{point.name}: {point.y:.2f} €"),
          showInLegend = TRUE
        )
      ) %>%
      hc_add_series(
        name = "Kategorien",
        data = data$hierarchy,
        type = "pie"
      ) %>%
      hc_drilldown(series = data$drilldown)
  })
  
  # Render hierarchical highchart for expenses
  output$category_table_expenses <- renderHighchart({
    data <- hierarchical_data_expenses()
    
    if (is.null(data)) {
      return(NULL)  # Prevent rendering if there's no data
    }
    
    highchart() %>%
      hc_chart(type = "pie") %>%
      hc_title(text = "Ausgaben Kategorien und Unterkategorien") %>%
      hc_plotOptions(
        series = list(
          dataLabels = list(enabled = TRUE, format = "{point.name}: {point.y:.2f} €"),
          showInLegend = TRUE
        )
      ) %>%
      hc_add_series(
        name = "Kategorien",
        data = data$hierarchy,
        type = "pie"
      ) %>%
      hc_drilldown(series = data$drilldown)
  })
  
  # Render the Income by Category Plot (Pie Chart for Selected Months)
  output$income_by_category_plot <- renderHighchart({
    income_data <- income_by_category()
    
    # Check if income_data is not empty
    if (nrow(income_data) == 0) {
      return(NULL)
    }
    
    hchart(income_data, type = "pie", hcaes(name = kategorie, y = total_income)) %>%
      hc_title(text = "Einnahmen nach Kategorien") %>%
      hc_tooltip(pointFormat = "<b>{point.name}: {point.y:.2f} €</b>") %>%
      hc_plotOptions(pie = list(showInLegend = TRUE, dataLabels = list(format = "{point.name}: {point.y:.2f} €")))
  })
  
  # Render the Expenses by Category Plot (Pie Chart for Selected Months)
  output$expenses_by_category_plot <- renderHighchart({
    expenses_data <- expenses_by_category()
    
    # Check if expenses_data is not empty
    if (nrow(expenses_data) == 0) {
      return(NULL)
    }
    
    hchart(expenses_data, type = "pie", hcaes(name = kategorie, y = total_expenses)) %>%
      hc_title(text = "Ausgaben nach Kategorien") %>%
      hc_tooltip(pointFormat = "<b>{point.name}: {point.y:.2f} €</b>") %>%
      hc_plotOptions(pie = list(showInLegend = TRUE, dataLabels = list(format = "{point.name}: {point.y:.2f} €")))
  })
}
