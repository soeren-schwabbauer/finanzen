library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(httr2)
library(jsonlite)

API_BASE_URL <- "http://localhost:8000"

get_api_data <- function(endpoint) {
  req <- request(paste0(API_BASE_URL, endpoint))
  resp <- req_perform(req)
  data <- resp_body_json(resp, simplifyVector = TRUE)
  
  if (length(data) == 0) {
    return(data.frame())
  }
  
  as.data.frame(data)
}

ui <- page_fluid(
  theme = bs_theme(version = 5),
  
  titlePanel("Finanz-Dashboard"),
  
  layout_column_wrap(
    width = 1,
    card(
      card_header("Aktive Konten"),
      
      div(
        style = "display: flex; gap: 24px; margin-bottom: 24px;",
        actionButton("add", "✚ Eintrag hinzufügen"),
        actionButton("delete", "🗑 Letzte Zeile löschen")
      ),
      
      uiOutput("account_accordion")
    )
  )
)

server <- function(input, output, session) {
  
  account_balances <- reactive({
    get_api_data("/active-account-balances")
  })
  
  transactions <- reactive({
    get_api_data("/active-account-transactions")
  })
  
  output$account_accordion <- renderUI({
    df_accounts <- account_balances()
    df_tx <- transactions()
    
    if (nrow(df_accounts) == 0) {
      return(p("Keine aktiven Konten gefunden."))
    }
    
    if (!"accounts_id" %in% names(df_accounts)) {
      return(paste(
        "Fehler: accounts_id fehlt in active_account_balances. Spalten:",
        paste(names(df_accounts), collapse = ", ")
      ))
    }
    
    if (!"accounts_id" %in% names(df_tx)) {
      return(paste(
        "Fehler: accounts_id fehlt in active_account_transactions. Spalten:",
        paste(names(df_tx), collapse = ", ")
      ))
    }
    
    df_tx <- df_tx |>
      mutate(accounts_id = as.character(.data$accounts_id))
    
    accordion(
      id = "accounts",
      open = FALSE,
      
      !!!lapply(seq_len(nrow(df_accounts)), function(i) {
        acc <- df_accounts[i, ]
        
        account_id <- as.character(acc$accounts_id[[1]])
        
        account_tx <- df_tx |>
          filter(.data$accounts_id == account_id) |>
          arrange(desc(.data$datum)) |>
          select(-id, -accounts_id)
        
        panel_title <- paste0(
          acc$bank[[1]],
          " - ",
          acc$kontotyp[[1]],
          " | ",
          formatC(
            as.numeric(acc$kontostand[[1]]),
            format = "f",
            digits = 2,
            big.mark = ".",
            decimal.mark = ","
          ),
          "€"
        )
        
        accordion_panel(
          title = panel_title,
          
          DT::datatable(
            account_tx,
            filter = "top",
            rownames = FALSE,
            options = list(
              pageLength = 5,
              lengthMenu = c(5, 10, 25, 50),
              autoWidth = TRUE
            )
          )
        )
      })
    )
  })
}

shinyApp(ui, server)