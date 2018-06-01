#' get_match_stats
#'
#' Get match stats (not error handled)
#'
#' @param x html_node
#' @return tibble(duration, blue_gold, blue_kills, blue_teamkills, blue_deaths, blue_baron, blue_herald, red_gold, red_kills, red_teamkills, red_deaths, red_baron, red_herald, d_gold, d_kills, d_teamkills, d_deaths, d_baron, d_herald)
#'
#' @export

get_match_stats <- function(x){
  
  new_names <- c("duration", "blue_gold", "blue_kills", "blue_teamkills", "blue_deaths", "blue_baron", "blue_herald", "red_gold", "red_kills", "red_teamkills", "red_deaths", "red_baron", "red_herald", "d_gold", "d_kills", "d_teamkills", "d_deaths", "d_baron", "d_herald")
  
  x %>%
    html_children() %>%
    .[12:30] %>% 
    html_text() %>% 
    str_trim() %>% 
    t() %>% 
    as_tibble %>% 
    purrr::set_names(new_names) %>% 
    mutate(duration = duration %>% str_replace(":", ".") %>% as.numeric) %>% 
    map_df(~str_replace(.x, "k", "") %>% as.numeric)
}