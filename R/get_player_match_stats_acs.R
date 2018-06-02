#' get_player_match_stats_acs
#'
#' get players match stats
#'
#' @param x html_node
#' @return data
#'
#' @export
get_player_match_stats_acs <- function(x){
  x$participants %>%
    map(~{
      .x$stats %>% 
        as_tibble %>% 
        janitor::clean_names() %>% 
        select(-participant_id)
    }) %>% 
    tibble(stats = .) %>%
    mutate(team_index = c(rep(100, 5), rep(200, 5))) %>%
    group_by(team_index) %>% 
    mutate(player_index = 1:n()) %>% 
    ungroup()
}

