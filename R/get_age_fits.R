#' Get fits to indices
#'
#' @param models an object of class jjm.output
#'
#' @return a tidy dataframe of age fits
#' @export
#'
get_age_fits <- function(models) {
  top_getter <- function(model) {
    obs_ages <-
      lapply(model$output, getter, pattern = "^pobs_(?!.*?len)") # pull out pobs that aren't lengths
    
    obs_ages <-
      purrr::map_df(obs_ages,
                    ~ purrr::map_df(.x, as.data.frame, .id = "source"),
                    .id = "stock")
    
    obs_ages$type <- "observed"
    
    
    pred_ages <-
      lapply(model$output, getter, pattern = "^phat_(?!.*?len)") # pull out pobs that aren't lengths
    
    pred_ages <-
      purrr::map_df(pred_ages,
                    ~ purrr::map_df(.x, as.data.frame, .id = "source"),
                    .id = "stock")
    
    pred_ages$type <- "predicted"
    
    
    
    age_fits <- rbind(pred_ages, obs_ages) %>%
      dplyr::rename(year = V1) %>%
      tidyr::pivot_longer(
        tidyr::matches("^V", perl = TRUE),
        names_to = "age",
        values_to = "proportion",
        names_prefix = "V",
        names_transform = list(age = as.integer)
      ) %>%
      dplyr::mutate(age = age - min(age) + model$data$ages[1])
    
    
    tmp <-
      gsub("phat_", "", age_fits$source) # being lazy and doing this twice
    
    tmp <- gsub("pobs_", "", tmp)
    
    age_fits <- age_fits %>%
      dplyr::mutate(source = tmp) %>%
      tidyr::pivot_wider(names_from = type, values_from = proportion)
    
    return(age_fits)
    
    
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

  out <-
    purrr::map_df(models, top_getter, .id = "model") %>% 
    tidyr::separate(source, sep = "_", into = c("fleet_type", "fleet_number")) %>% 
    dplyr::mutate(fleet_number = as.integer(fleet_number)) %>% 
    dplyr::left_join(fleet_names, by = c("fleet_type", "fleet_number"))
    # flatten and collect across models
}