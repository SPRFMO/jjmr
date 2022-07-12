#' Get estimated recruits
#'
#' @param models an object of class jjm.output
#'
#' @return a tidy dataframe of recruits
#' @export
#'
get_recruits <- function(models) {
  top_getter <- function(model) {
    devs <-
      lapply(model$output, getter, pattern = "rec_dev") # pull out recruitment deviates
    
    devs <-
      purrr::map_df(devs, ~ purrr::map_df(.x, as.data.frame), .id = "stock")
    
    
    devs <- dplyr::rename(devs, year = V1, rec_dev = V2)  # rename
    
    recs <-
      lapply(model$output, getter, pattern = "^R$") # pull out recruitment deviates
    
    recs <-
      purrr::map_df(recs, ~ purrr::map_df(.x, as.data.frame), .id = "stock")
    
    
    names(recs)[grepl("V", names(recs))] <-
      c("year",
        "recruits",
        "str_error",
        "lower_recruits",
        "upper_recruits")
    
    out <- recs %>%
      dplyr::left_join(devs, by = c("stock", "year"))
    
    
  }
  
  out <-
    purrr::map_df(models, top_getter, .id = "model") # flatten and collect across models
  
  
  
  
}