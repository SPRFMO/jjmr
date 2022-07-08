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
  
  ind_names <-
    data.frame(
      fleet_type = "ind",
      fleet_name = models[[1]]$data$Inames,
      fleet_number = seq_along(models[[1]]$data$Inames)
    )
  
  fsh_names <-
    data.frame(
      fleet_type = "fsh",
      fleet_name = models[[1]]$data$Fnames,
      fleet_number = seq_along(models[[1]]$data$Fnames)
    )
  
  fleet_names <- rbind(ind_names, fsh_names)
  
  out <- purrr::map_df(models, top_getter, .id = "model") # flatten and collect across models
  
  out <- out %>% 
    tidyr::separate(index, sep = "_", into = c("type","index_type", "fleet_number")) %>% 
    dplyr::mutate(fleet_number = as.integer(fleet_number),
                  fleet_type = "ind") %>% 
    dplyr::left_join(fleet_names, by = c("fleet_number", "fleet_type"))
  
  
}