#' get_player_runes_acs
#'
#' get players runes
#'
#' @param x html_node
#' @return data
#'
#' @export

get_player_runes_acs <- function(x){
  x$participants %>% 
    purrr::map(~{
      .x$runes %>% 
        map(~{
          return(tibble(rune_id = .x$runeId, rank = .x$rank))
        }) %>% 
        bind_rows %>% 
        list() %>% 
        tibble(runes = .)
    }) %>% 
    bind_rows() %>% 
    mutate(team_index = c(rep(100, 5), rep(200, 5))) %>%
    group_by(team_index) %>% 
    mutate(player_index = 1:n()) %>% 
    ungroup()
} 