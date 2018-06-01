#' get_match_history
#'
#' Get meta data (error handled)
#'
#' @param x html_node
#' @return get_match_data, get_player_stats, get_match_stats, get_match_meta
#' @export

get_match_history <- function(x){
  match_data <- suppressWarnings(get_match_data(x))
  player_stats <- suppressWarnings(get_player_stats(x)) 
  match_stats <- suppressWarnings(get_match_stats(x))
  match_meta <- suppressWarnings(get_match_meta(x))
  
  matches <- match_data %>% 
    bind_cols(tibble(players = list(player_stats))) %>%
    bind_cols(match_stats, match_meta)
  return(matches)
}


#' get_match_history_safely
#'
#' error handler
#'
#' @export
get_match_history_safely <- purrr::safely(get_match_history)


#' get_match_history
#'
#' Get all matches from a tournament
#'
#' @param x html_node
#' @return get_match_data, get_player_stats, get_match_stats, get_match_meta
#' @export
get_tournament_matches <- function(x){
  x %>%     
    html_nodes(".wikitable") %>% 
    html_children() %>% 
    .[3:(length(.)-1)] %>% 
    #.[1] # map index
    map(get_match_history_safely)
}