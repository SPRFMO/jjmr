#' Get fishing mortality at age
#'
#' @param models an object of class jjm.output
#'
#' @return a tidy dataframe of fishing mortality at age
#' @export
#'
get_fishing_mortality <- function(models){
  
  top_getter <- function(model){
    
    
    fs <-
      lapply(model$output, getter, pattern = "TotF") # pull out selectivity objects
    
    fs <-
      purrr::map_df(fs, ~ purrr::map_df(.x, as.data.frame), .id = "stock")
    
    fs <- fs %>% 
      dplyr::group_by(stock) %>% 
      dplyr::mutate(year = seq(model$data$years[1],model$data$years[2])) %>% 
      tidyr::pivot_longer(
        tidyr::matches("^V", perl = TRUE),
        names_to = "age",
        values_to = "mortality",
        names_prefix = "V",
        names_transform = list(age = as.integer)
      ) %>%
      dplyr::mutate(age = age - min(age) + model$data$ages[1])
    
  }
  
  out <- purrr::map_df(models, top_getter, .id = "model") # flatten and collect across models
  
  
  
  
}