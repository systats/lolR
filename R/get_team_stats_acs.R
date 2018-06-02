#' get_team_stats_acs
#'
#' get team stats
#'
#' @param x html_node
#' @return data
#'
#' @export

get_team_stats_acs <- function(x){
  
  team_stats <- x$teams %>% 
    as.list() %>%
    map(~{
      .x %>% 
        as_tibble %>% 
        mutate(player_index = 1:n())
    }) %>% 
    bind_rows %>% 
    janitor::clean_names() %>% 
    rename(team_index = team_id) %>% 
    select(-player_index)
  
  bans <- team_stats %>% 
    group_by(team_index) %>%
    select(bans) %>% 
    ungroup() %>% 
    split(.$team_index) %>%
    map(~{
      .x$bans %>% 
        bind_rows()
    })
  
  out <- team_stats %>% 
    select(-bans) %>% 
    group_by(team_index) %>% 
    slice(1) %>% 
    ungroup() %>% 
    mutate(bans = bans) %>% 
    split(.$team_index) %>% 
    map(~{
      if(.x$team_index[1] == 100){
        colnames(.x) <- paste0("blue_", colnames(.x))
        return(.x)
      } else {
        colnames(.x) <- paste0("red_", colnames(.x))
        return(.x)
      }
    }) %>% 
    bind_cols %>% 
    select(-contains("index")) %>% 
    list() %>% 
    tibble(team_stats = .)
  
  return(out)
}

