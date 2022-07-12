#' Get and tidy selectivity-at-age ogives over time by model and fleet
#'
#' @param models an object of class jjm.output
#'
#' @return a tidy data frame of selectivity estimates
#' @export
#'
#' @examples
#' \dontrun{
#' h1.mod <- jjmR::readJJM("h2_0.02", path = "config", input = "input")
#' selectivities <- get_selectivities(h1.mod)
#' }
#' 
get_selectivities <- function(models) {
  
  
  top_getter <- function(model){
    
    
    ind_names <- data.frame(fleet_type = "ind",fleet_name = model$data$Inames, fleet_number = seq_along( model$data$Inames))

    fsh_names <- data.frame(fleet_type = "fsh",fleet_name = model$data$Fnames,
                              fleet_number = seq_along( model$data$Fnames))

    fleet_names <- rbind(ind_names,fsh_names)
    
    sels <- lapply(model$output, getter) # pull out selectivity objects
    
    deeper_getter <- function(z, fleet_names){
      tmp2 <- purrr::map_df(z, as.data.frame, .id = "object") %>% # convert each selectivity object to a data.frame
        dplyr::rename(year = V2, index = V1) %>% # rename
        tidyr::pivot_longer(
          dplyr::starts_with("V"),
          names_to = "age",
          values_to = "selectivity",
          names_prefix = "V",
          names_transform = list(age = as.integer)
        ) %>% # ages to pivot to longer form 
        tidyr::separate(object,
                        sep = "_",
                        into = c("sel", "fleet_type", "fleet_number")) %>% 
        dplyr::mutate(fleet_number = as.integer(fleet_number)) %>% 
        dplyr::left_join(fleet_names, by = c("fleet_type", "fleet_number")) #isolate components of name
      
    }
    
    sels <- purrr::map_df(sels, deeper_getter, fleet_names =fleet_names, .id = "stock") # flatten the selecivity data
    
  }
  
  
  out <- purrr::map_df(models, top_getter, .id = "model") # flatten and collect across models
  
}