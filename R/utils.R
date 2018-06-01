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
