
add_groups <- function(data) {
  
  # Load group definitions
  gruppen <- read.csv("data/gruppen.csv")
  
  umsatzanzeige_gruppen <- gruppen %>%
    filter(!is.na(string)) %>%
    mutate(string = tolower(string))
  
  # Prepare patterns for matching
  patterns <- tolower(umsatzanzeige_gruppen$string)
  
  # Define the join_groups function
  data %>%
    rowwise() %>%
    mutate(string = {
      lower_gegenseite <- tolower(gegenseite)
      lower_verwendungszweck <- tolower(verwendungszweck)
      
      # Check if either field is empty
      if (nchar(lower_gegenseite) == 0 && nchar(lower_verwendungszweck) == 0) {
        return(NA)
      }
      
      # Filter out empty patterns
      valid_patterns <- patterns[patterns != ""]
      
      matched_pattern <- valid_patterns[sapply(valid_patterns, function(pattern) {
        str_detect(lower_gegenseite, pattern) | str_detect(lower_verwendungszweck, pattern)
      })]
      
      if (length(matched_pattern) > 0) matched_pattern[1] else NA
    }) %>%
    ungroup() %>%
    left_join(umsatzanzeige_gruppen %>% distinct(string, kategorie, name), by = "string") %>%
    mutate(kategorie = ifelse(is.na(kategorie), "sonstige", kategorie),
           name = ifelse(is.na(name), gegenseite, name))
  
  
}
