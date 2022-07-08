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
  
  out <- out %>% 
    tidyr::separate(index, sep = "_", into = c("metric", "fleet_number")) %>% 
    dplyr::mutate(fleet_number = as.integer(fleet_number),
           fleet_type = "ind") %>% 
    dplyr::left_join(fleet_names, by = c("fleet_number", "fleet_type"))
  

}