#' get_player_timeline_acs
#'
#' get players timeline
#'
#' @param x html_node
#' @return data
#'
#' @export
get_player_timeline_acs <- function(x) {
  
  x$participants %>% 
    map(~{
      .x$timeline %>%
        names %>% 
        as.list %>% 
        map2(.x = ., .y = .x$timeline, ~{
          if(length(.y) == 1){
            out <- tibble(.y)
            colnames(out) <- .x
            return(out)
          } else {
            out <- .y %>% list() %>% tibble()
            colnames(out) <- .x
            return(out)
          }
        }) %>%
        bind_cols() %>%
        janitor::clean_names() %>%
        list() %>%
        tibble(timeline = .)
    }) %>% 
    bind_rows() %>% 
    mutate(team_index = c(rep(100, 5), rep(200, 5))) %>%
    group_by(team_index) %>% 
    mutate(player_index = 1:n()) %>% 
    ungroup() 
}  
