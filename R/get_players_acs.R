#' get_players_acs
#'
#' get players from match
#'
#' @param x html_node
#' @return data
#'
#' @export

get_players_acs <- function(x){
  x$participantIdentities %>% 
    map(~{
      player <- .x[["player"]]$summonerName
      team <- .x[["player"]]$profileIcon
      return(tibble(player, team))
    }) %>% 
    bind_rows() %>% 
    mutate(player = player %>% str_replace("^\\w+ ", "")) %>% 
    mutate(team_abb = player %>% str_extract("^\\w+") %>% ifelse(player == ., NA, .)) %>%
    mutate(team_index = c(rep(100, 5), rep(200, 5))) %>%
    group_by(team_index) %>%
    mutate(player_index = 1:n()) %>% 
    select(-team) %>% 
    ungroup
}
