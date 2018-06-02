#' extract_match
#'
#' get full match information
#'
#' @param x path to json 
#' @return data
#'
#' @export
extract_match <- function(x){
  
  x <- jsonlite::read_json(x)
  
  match_data <- get_match_data_acs(x)
  team_stats <- get_team_stats_acs(x)

  ### Individual data
  player_data <- get_players_acs(x)
  player_champion <- get_player_champion_acs(x) 
  player_master <- get_player_master_acs(x)
  player_runes <- get_player_runes_acs(x)
  player_achieve <- get_player_ach_acs(x)
  player_match_stats <- suppressWarnings(get_player_match_stats_acs(x))
  player_timeline <- get_player_timeline_acs(x)

  
  player_long <- player_data %>%
    left_join(player_champion, by = c("team_index", "player_index")) %>%
    left_join(player_master, by = c("team_index", "player_index")) %>% 
    left_join(player_runes, by = c("team_index", "player_index")) %>%
    left_join(player_achieve, by = c("team_index", "player_index")) %>% 
    left_join(player_match_stats, by = c("team_index", "player_index")) %>% 
    left_join(player_timeline, by = c("team_index", "player_index")) %>% 
    ungroup()
  
  player_stats <- player_long %>% 
    split(c(rep(1, 5), rep(0, 5))) %>% 
    map(~{
      if(.x$team_index[1] == 100){
        colnames(.x) <- paste0("blue_", colnames(.x))
        return(.x)
      } else {
        colnames(.x) <- paste0("red_", colnames(.x))
        return(.x)
      }
    }) %>% 
    bind_cols() %>% 
    dplyr::select(-contains("index")) %>% 
    list() %>% 
    tibble(player_stats = .)
  
  out <- match_data %>% 
    bind_cols(team_stats) %>% 
    bind_cols(player_stats)
  
  return(out)
}


