#' get_tournaments
#'
#' Get tournaments avaible
#'
#' @param x example url 
#' @return tibble(tourn_name, tourn_org, tourn_info, tourn_season, tourn_year, tourn_url)
#'
#' @export
get_tournaments <- function(x){
  
  tourn_list <- read_html(x) %>%
    html_node(".inputSpan") %>%
    html_children() %>%
    html_children() %>%#
    html_text() %>%
    .[-1] %>% 
    str_replace("Concept:", "")
  
  tourn_table <- tourn_list %>% 
    tibble(tourn_org = .) %>% 
    mutate(
      tourn_name = tourn_org %>% 
        as.list() %>% 
        map_chr(~{
          .x %>% 
            str_split("\\d{4}|Season \\d{1,}") %>% 
            unlist() %>% 
            .[1] %>% 
            str_trim
        }),
      tourn_info = tourn_org %>% 
        as.list() %>% 
        map_chr(~{
          .x %>% 
            str_split("\\d{4}|Season \\d{1,}") %>% 
            unlist() %>% 
            .[2] %>% 
            str_trim
        }),
      tourn_season = tourn_org %>% 
        str_extract("\\d{4}|Season \\d{1,}"),
      tourn_year = tourn_season %>% 
        str_extract("\\d{4}"),
      tourn_url = tourn_org %>%
        str_replace_all(., "\\s", "%20") %>%
        paste0(
          "https://lol.gamepedia.com/", 
          "Special:RunQuery/MatchHistoryTournament?MHT%5Btournament%5D=Concept:", .,
          "&MHT%5Btext%5D=Yes&pfRunQueryFormName=MatchHistoryTournament"
        )
    ) %>% 
    tidyr::nest(.key = "details", -tourn_name)
  
  return(tourn_table)
}