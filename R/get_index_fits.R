#' Get fits to indices
#'
#' @param models an object of class jjm.output
#'
#' @return a tidy dataframe of index fits
#' @export
#'
get_index_fits <- function(models){
  
  top_getter <- function(model){
    
    
    things <-
      lapply(model$output, getter, pattern = "Obs_Survey_") # pull out selectivity objects
    
    things <-
      purrr::map_df(things, ~ purrr::map_df(.x, as.data.frame, .id = "index"), .id = "stock")
    
    names(things)[grepl("^V",names(things))] <- c('year', "observed_ind", "pred_ind", "observed_se", "pearson_resid", "something_resids")
    
    return(things)
    
    
  }
  
  out <- purrr::map_df(models, top_getter, .id = "model") # flatten and collect across models
  
}