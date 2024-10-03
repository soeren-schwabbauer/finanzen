
umsatzanzeige_raw <- function(datapath) {

  # Load the CSV data initially and combine
  ing_data <- read.csv(paste0(datapath, "umsatzanzeige_ING.csv"))
  tr_data <- read.csv(paste0(datapath, "umsatzanzeige_TradeRepublik.csv"))
  sparbrief_data <- read.csv(paste0(datapath, "umsatzanzeige_Sparbrief.csv"))
  
  # Add a konto column to each data frame
  ing_data$konto <- "ING"
  tr_data$konto <- "TradeRepublik"
  sparbrief_data$konto <- "Sparbrief"
  
  # Convert betrag and betrag_edited to numeric
  ing_data$betrag <- as.numeric(ing_data$betrag)
  tr_data$betrag <- as.numeric(tr_data$betrag)
  sparbrief_data$betrag <- as.numeric(sparbrief_data$betrag)
  
  ing_data$betrag_edited <- as.numeric(ing_data$betrag_edited)
  tr_data$betrag_edited <- as.numeric(tr_data$betrag_edited)
  sparbrief_data$betrag_edited <- as.numeric(sparbrief_data$betrag_edited)
  
  umsatzanzeige_raw <- bind_rows(ing_data, tr_data, sparbrief_data) %>%
    arrange(desc(datum)) %>%
    filter(datum < Sys.Date())
  
  return(umsatzanzeige_raw)
  
}