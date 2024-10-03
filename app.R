library(shiny)
library(DT)
library(highcharter)
library(stringr)

library(ggplot2)

library(magrittr)
library(dplyr)

showcase = TRUE
datapath <<- "data/"
datapath <<- if(isTRUE(showcase)) "data_showcase/"

source("functions/add_groups.R")
source("functions/umsatzanzeige_raw.R")

source("umsatzanzeige_server.R")
source("umsatzanzeige_ui.R")

source("auswahlverlauf_server.R")
source("auswahlverlauf_ui.R")

source("aggregiert_server.R")
source("aggregiert_ui.R")

source("cluster_server.R")
source("cluster_ui.R")

source("timeseries_server.R")
source("timeseries_ui.R")

ui <- navbarPage("Finanzen",
                 
                 # SelectInput components
                 fluidRow(
                   column(8, selectInput("subset_months", "Monate Auswählen", 
                                         choices = rev(format(seq.Date(as.Date("2023-08-01"), Sys.Date(), by = "month"), "%Y-%m")),
                                         multiple = TRUE, selected = format(Sys.Date(), "%Y-%m"),
                                         width = "100%")),
                   column(4, selectInput("subset_banks", "Konten auswählen", 
                                         choices = c("ING", "TradeRepublik", "Sparbrief"),
                                         selected = c("ING", "TradeRepublik", "Sparbrief"),
                                         multiple = TRUE,
                                         width = "100%"))
                 ),
                 
                 tabPanel("Umsatzanzeige", umsatzanzeige_ui("umsatzanzeige")),
                 
                 navbarMenu("Deskriptives",
                            tabPanel("Verlauf", auswahlverlauf_ui("auswahlverlauf")),
                            tabPanel("Aggregiert", aggregiert_ui("aggregiert"))
                 ),
                 
                 navbarMenu("Analyse", 
                            tabPanel("Cluster Analyse", cluster_ui("cluster")),
                            tabPanel("Zeitserie", timeseries_ui("timeseries"))
                 ),
                 
)


# Define the server logic
server <- function(input, output, session) {
  
  # prepare data which will be passed into the modules -------------------------
  rv <- reactiveValues(combined_data = NULL) # Reactive values to store the combined data
  
  rv$combined_data <- umsatzanzeige_raw(datapath)
  
  filtered_data <- reactive({
    rv$combined_data %>%
      filter(format(as.Date(datum), "%Y-%m") %in% input$subset_months) %>%
      filter(konto %in% input$subset_banks)
  })
  
  # call remaining modules -----------------------------------------------------
  callModule(umsatzanzeige_server, "umsatzanzeige", data = filtered_data, rv = rv)
  
  callModule(auswahlverlauf_server, "auswahlverlauf", data = filtered_data)
  
  callModule(aggregiert_server, "aggregiert", data = filtered_data)
  
  callModule(cluster_server, "cluster", data = filtered_data)
  
  callModule(timeseries_server, "timeseries", data = filtered_data)
  

}

# Run the application
shinyApp(ui = ui, server = server)
