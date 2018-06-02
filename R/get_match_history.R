#' get_match_details
#'
#' Get meta data (error handled)
#'
#' @param x html_node
#' @return get_match_data, get_player_stats, get_match_stats, get_match_meta
#' @export

get_match_details <- function(x){
  
  # x <- tourn_table_long[1, ] %>% 
  #   .$tourn_url %>% 
  #   xml2::read_html() %>%
  #   html_nodes(".wikitable") %>% 
  #   html_children() %>% 
  #   .[3:(length(.)-1)] %>% 
  #   .[1] # map index

  match_data <- suppressWarnings(lolR::get_match_data(x))
  player_stats <- suppressWarnings(lolR::get_player_stats(x)) 
  match_stats <- suppressWarnings(lolR::get_match_stats(x))
  match_meta <- suppressWarnings(lolR::get_match_meta(x))
  
  matches <- match_data %>% 
    dplyr::bind_cols(tibble::tibble(players = list(player_stats))) %>%
    dplyr::bind_cols(match_stats, match_meta)
  return(matches)
}


#' get_match_details_safely
#'
#' error handler
#'
#' @export
get_match_details_safely <- purrr::safely(get_match_details)


#' get_matches
#'
#' Get all matches from a tournament
#'
#' @param x html_node
#' @return get_match_data, get_player_stats, get_match_stats, get_match_meta
#' @export
get_matches <- function(x){
  x %>%     
    rvest::html_nodes(".wikitable") %>% 
    rvest::html_children() %>% 
    .[3:(length(.)-1)] %>% 
    #.[1] # map index
    purrr::map(~lolR::get_match_details_safely(.x)) %>% 
    purrr::map("result") %>% 
    dplyr::bind_rows()
}