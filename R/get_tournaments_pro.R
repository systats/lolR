#' get_tournaments_pro
#'
#' Get all matches from a tournament
#'
#' @param data long tournament dataset
#' @return get_tournament_matches look for output
#' @export

get_tournaments_pro <- function(data){
  
  get_tournament_matches_pro <- lolR::progressively(lolR::get_tournament_matches, nrow(data))
  
  tourn_list <- data %>%
    split(1:nrow(.)) %>%
    #as.list() %>% 
    map(~{
      temp <- .x
      core <- temp$tourn_url %>% 
        xml2::read_html() %>% 
        get_tournament_matches_pro %>% 
        purrr::map("result") %>% 
        dplyr::bind_rows()
      
      #print("1")
      rep_tourn <- 1:nrow(core) %>% 
        map(~return(temp)) %>% 
        bind_rows()
      
      #print("2")
      out <- core %>% 
        dplyr::bind_cols(rep_tourn)
      
      return(out)
    }) 
  
  return(tourn_list)
}