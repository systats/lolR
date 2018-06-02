#' get_player_champion_acs
#'
#' get players champion info
#'
#' @param x html_node
#' @return data
#'
#' @export

get_player_champion_acs <- function(x) {
  x$participants %>% 
    map(~{
      .x %>% 
        .[1:5] %>% 
        as_tibble %>% 
        janitor::clean_names() %>% 
        rename(team_index = team_id)
    }) %>% 
    bind_rows %>% 
    group_by(team_index) %>% 
    mutate(player_index = 1:n()) %>% 
    ungroup() %>% 
    select(-participant_id)
}

