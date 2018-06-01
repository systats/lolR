#' save_it
#'
#' tidy way to save data
#'
#' @export
save_it <- function(x){
  if(!dir.exists("data")){
    dir.create("data")
  }
  save(x, file = paste0("data/", deparse(substitute(x)), ".Rdata"))
  message(paste0("File was saved under: data/", deparse(substitute(x)), ".Rdata"))
}
#save_it(base_html)

#' progressively
#'
#' gives progressbar capabilities to purrr
#'
#' @export
progressively <- function(.f, .n, ...) {
  pb <- progress::progress_bar$new(total = .n, ...)
  function(...) {
    pb$tick()
    .f(...)
  }
}

#' str_extract_href
#'
#' extract hyperlink
#'
#' @export
str_extract_href <- function(x, append = F){
  x <- x %>% 
    as.character() %>%
    stringr::str_extract('href=\".*?\"') %>% 
    stringr::str_replace_all('href=|\"', '')
  
  if(append) x <- paste0("https://lol.gamepedia.com", x)
  return(x)
}