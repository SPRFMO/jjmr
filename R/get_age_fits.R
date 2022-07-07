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
  
  out <-
    purrr::map_df(models, top_getter, .id = "model") # flatten and collect across models
  
}