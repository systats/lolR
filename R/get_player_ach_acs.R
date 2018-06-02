#' get_player_ach_acs
#'
#' get players highest achievement
#'
#' @param x html_node
#' @return data
#'
#' @export
#' 
get_player_ach_acs <- function(x){
  x$participants %>% 
    purrr::map_chr(~{
      .x$highestAchievedSeasonTier
    }) %>% 
    tibble(highest_tier_season = . ) %>%
    mutate(team_index = c(rep(100, 5), rep(200, 5))) %>%
    group_by(team_index) %>% 
    mutate(player_index = 1:n()) %>% 
    ungroup()
}
