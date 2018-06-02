#' get_match_meta
#'
#' Get match data
#'
#' @param x json file
#' @return data
#'
#' @export

get_match_data_acs <- function(x){
  match_data <- x[1:10] %>% 
    as_tibble %>% 
    janitor::clean_names(.) %>% 
    .[1,]
  
  if(!is.null(match_data$teams)) match_data <- match_data %>% select(-teams)
  return(match_data) 
}
