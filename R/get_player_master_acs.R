#' get_player_master_acs
#'
#' get players from match
#'
#' @param x html_node
#' @return data
#'
#' @export

get_player_master_acs <- function(x){
  x$participants %>% 
    purrr::map(~{
      .x$masteries %>% 
        map(~{
          return(tibble(master = .x$masteryId, rank = .x$rank))
        }) %>% 
        bind_rows %>% 
        list() %>% 
        tibble(master = .)
    }) %>% 
    bind_rows %>% 
    mutate(team_index = c(rep(100, 5), rep(200, 5))) %>%
    group_by(team_index) %>% 
    mutate(player_index = 1:n()) %>% 
    ungroup()  
}