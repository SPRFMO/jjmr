#' Get estimated catchability coefficients
#'
#' @param models an object of class jjm.output
#'
#' @return a data frame of estimated catchabilities
#' @export
#'
get_catchabilities <- function(models) {
 
  top_getter <- function(model){
    
  
  qs <-
    lapply(model$output, getter, pattern = "q_\\d") # pull out selectivity objects
  
  qs <-
    purrr::map_df(qs, ~ purrr::map_df(.x, as.data.frame, .id = "index"), .id = "stock")
  
  qs <- dplyr::rename(qs, year = V1, q = V2)  # rename
  }
  
  out <- purrr::map_df(models, top_getter, .id = "model") # flatten and collect across models
  
  
}