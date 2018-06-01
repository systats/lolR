#' get_match_data
#'
#' Get basic match data
#'
#' @param x html_node
#' @return tibble(date, patch, patch_link, win, blue_team, blue_team_link,  red_team, red_team_link)
#'
#' @export
get_match_data <- function(x){
  
  prep <- x %>% 
    html_children()
  
  date <- prep %>% 
    .[1] %>% 
    html_text %>% 
    stringr::str_trim()
  
  patch <- prep%>% 
    .[2] %>% 
    html_text %>% 
    stringr::str_trim()
  
  patch_link <-prep %>% 
    .[2] %>% 
    str_extract_href() %>% 
    paste0(base_url, .) 
  
  blue_team <- prep %>% 
    .[3] %>% 
    html_text %>% 
    stringr::str_trim()
  
  blue_team_link <- prep %>% 
    .[3] %>% 
    str_extract_href() %>% 
    paste0(base_url, .)
  
  red_team <- prep %>% 
    .[4] %>% 
    html_text %>% 
    stringr::str_trim()
  
  red_team_link <- prep %>% 
    .[4] %>% 
    str_extract_href() %>% 
    paste0(base_url, .)
  
  win <- prep %>% 
    .[5] %>% 
    html_text() %>% 
    stringr::str_trim()
  
  match_data <- tibble(
    date, patch, patch_link, win,
    blue_team, blue_team_link, 
    red_team, red_team_link
  )
  
  return(match_data)
}