#' get_tourn_match_list
#'
#' Get all matches from a tournament
#'
#' @param x a tournament row
#' @return get_tournament_matches look for output
#' @export

get_tourn_match_list <- function(x){
  
  #x <- tourn_table_long[52,]
  
  core <- x$tourn_url %>% 
    xml2::read_html() %>% 
    get_matches
  
  #print("1")
  rep_tourn <- 1:nrow(core) %>% 
    map(~return(x)) %>% 
    bind_rows()
  
  #print("2")
  out <- core %>% 
    dplyr::bind_cols(rep_tourn)
  
  return(out)
}


#' get_tourn_matches
#'
#' Get all matches from a tournament
#'
#' @param data long tournament dataset
#' @return get_tournament_matches look for output
#' @export

get_tourn_matches <- function(data){
  
  get_tourn_match_list_safely <- purrr::safely(get_tourn_match_list)
  get_tourn_match_list_pro <- lolR::progressively(get_tourn_match_list_safely, nrow(data))
  
  #print(1)
  
  tourn_list <- data %>%
    split(1:nrow(.)) %>%
    #as.list() %>% 
    map(get_tourn_match_list_pro) 
  
  return(tourn_list)
}
