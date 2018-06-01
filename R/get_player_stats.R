#' get_player_stats
#'
#' Get player stats data (basic error handler)
#'
#' @param x html_node
#' @return   players <- tibble(blue_players, blue_player_links, blue_champions, blue_bans, red_players, red_player_links,  red_champions, red_bans) 
#'
#' @export
get_player_stats <- function(x){
  
  prep <- x %>% 
    html_children()
  
  blue_bans <- prep %>% 
    .[6] %>% 
    html_text %>% 
    stringr::str_split(",") %>% 
    unlist() %>% 
    stringr::str_trim() %>% 
    ifelse(length(.) < 5, c(.,rep(NA, (5-length(.)))), .)
  
  red_bans <- prep %>% 
    .[7] %>% 
    html_text %>% 
    stringr::str_split(",") %>% 
    unlist() %>% 
    stringr::str_trim() %>% 
    ifelse(length(.) < 5, c(.,rep(NA, (5-length(.)))), .)
  
  
  blue_champions <- prep %>% 
    .[8] %>% 
    html_text %>% 
    stringr::str_split(",") %>% 
    unlist() %>% 
    stringr::str_trim()
  
  red_champions <- prep %>% 
    .[9] %>% 
    html_text %>% 
    stringr::str_split(",") %>% 
    unlist() %>% 
    stringr::str_trim()
  
  
  blue_players <- prep %>% 
    .[10] %>% 
    html_children() %>% 
    map(html_text) %>% 
    unlist()
  
  blue_player_links <- prep %>% 
    .[10] %>% 
    html_children() %>% 
    map(
      ~ str_extract_href(.x) %>% 
        paste0(base_url, .)
    ) %>% 
    unlist()
  
  red_players <- prep %>% 
    .[11] %>% 
    html_children() %>% 
    map(html_text) %>% 
    unlist()
  
  red_player_links <- prep %>% 
    .[11] %>% 
    html_children() %>% 
    map(
      ~ str_extract_href(.x) %>% 
        paste0(base_url, .)
    ) %>% 
    unlist()
  
  players <- tibble(
    blue_players,
    blue_player_links, 
    blue_champions,
    blue_bans,
    red_players,
    red_player_links, 
    red_champions,
    red_bans
  ) 
  
  return(players)
}