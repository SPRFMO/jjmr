#' Get total metrics (biomass, spawning biomass, and recruitment)
#'
#' @param models an object of class jjm.output
#'
#' @return a dataframe of total values
#' @export
#'
get_totals <- function(models){
  
  top_getter <- function(model){
    
    
    things <-
      lapply(model$output,
             getter,
             things = c("SSB", "R", "TotBiom")) # pull out sdreport objects
    
    things <-
      purrr::map_df(things, ~ purrr::map_df(.x, as.data.frame, .id = "metric"), .id = "stock")
    
    names(things)[grepl("^V",names(things))] <- c("year", "value", "stdev", "lowerbound", "upperbound")
    
    return(things)
    
  }
  
  out <- purrr::map_df(models, top_getter, .id = "model") # flatten and collect across models
  
}
