#' get_match_meta
#'
#' Get meta data (error handled)
#'
#' @param x html_node
#' @return tibble(match_url, api_url, game_hash, game_id, tournament_id, vod)
#'
#' @export

get_match_meta <- function(x){
  
  # scoreboard <- x %>%
  #   html_children() %>%
  #   .[31] %>% 
  #   str_extract_href() %>% 
  #   paste0(base_url, .)
  
  prep <- x %>%
    html_children()
  
  match_url <- prep %>%
    .[32] %>% 
    str_extract_href()
  
  api_base <- "https://acs.leagueoflegends.com/v1/stats/game"
  
  api_url <- match_url %>% 
    str_replace_all('.*match-details|&amp;tab=overview', '') %>% 
    paste0(api_base, .) %>% 
    ifelse(is.null(.), NA, .)
  
  game_hash <- match_url %>% 
    str_extract("gameHash=.*?&|gameHash=.*?$") %>% 
    str_replace_all("gameHash=|&", "") %>% 
    ifelse(is.null(.), NA, .)
  
  game_id <- match_url %>% 
    str_extract("\\d{3,}") %>% 
    ifelse(is.null(.), NA, .)
  
  tournament_id <- match_url %>% 
    str_extract("#match-details\\/.*?\\/") %>% 
    str_replace_all("#match-details|\\/", "") %>% 
    ifelse(is.null(.), NA, .)
  
  vod <- prep %>%
    .[33] %>% 
    str_extract_href() %>% 
    ifelse(is.null(.), NA, .)
  
  
  match_meta <- tibble(
    match_url, 
    api_url, 
    game_hash, 
    game_id, 
    tournament_id, vod
  )

  return(match_meta)
}