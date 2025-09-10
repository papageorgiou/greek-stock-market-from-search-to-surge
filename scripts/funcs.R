load_search_data_gkp_csv <- function(file) {
  new=read_tsv(file, skip = 2, locale = locale(encoding = "UTF-16LE")) %>%
    filter(!is.na(Keyword)) %>% 
    pivot_longer(cols = starts_with("Searches:"),
                 names_to = "date_str",
                 values_to = "search_volume") %>% 
    mutate(
      date = as.Date(paste("01", sub("Searches: ", "", date_str)), format = "%d %b %Y"),
      search_term = tolower(Keyword)
    ) %>%
    select(date, search_term, search_volume) 
}
