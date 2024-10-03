
cluster_server <- function(input, output, session, data) {
  
  ns <- session$ns
  
  data_clustered <- reactive({
    
    data <- data() %>% add_groups() %>% filter(!is.na(betrag_edited))
    
    # filter für nur positiv/negativ
    if(input$income_expenses == "Einnahmen") data %<>% filter(betrag_edited > 0)
    if(input$income_expenses == "Ausgaben") data %<>% filter(betrag_edited < 0) %>% mutate(betrag_edited = abs(betrag_edited))
    
    # Anzahl der Gruppen
    n_gruppen <- input$n_gruppen
    set.seed(123)
    kmeans_result <- kmeans(data$betrag_edited, centers = n_gruppen)
    data$Gruppe <- as.factor(kmeans_result$cluster)

    # Cluster der Größe nach ordnen
    mean_values <- aggregate(data$betrag_edited, by = list(Cluster = kmeans_result$cluster), FUN = mean)
    mean_values <- mean_values[order(mean_values$x), ]
    new_labels <- setNames(paste0("Gruppe ", 1:input$n_gruppen), mean_values$Cluster)
    data$Gruppe <- factor(data$Gruppe, levels = names(new_labels), labels = new_labels)
    
    return(data)
    
  })
  
  
  output$cluster_gruppen_boxplot <- renderPlot({
    
    ggplot(data_clustered(), aes(x = as.factor(Gruppe), y = betrag_edited, color = Gruppe)) +
      geom_boxplot() +
      geom_jitter() +
      labs(title = "k-means Clustering", y = "Betrag") +
      theme_bw() +
      theme(legend.position = "none",
            axis.title.x = element_blank())
    
  })
  
  output$cluster_gruppen_count <- renderPlot({
    
    ggplot(data_clustered()) +
      geom_bar(aes(x = Gruppe, fill = Gruppe)) +
      labs(title = "Anzahl nach Gruppe", y = "Anzahl") +
      theme_bw() +
      theme(legend.position = "none",
            axis.title.x = element_blank())
    
  })
  
  output$cluster_subgruppen_count <- renderDT({
    
  })
  
}


  
  
