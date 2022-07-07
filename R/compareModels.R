#' @title Compare combined JJM outputs
#' @description This function takes a vector of model names, reads in the JJM runs, and combines them.
#' Basically a wrapper function for \code{combineModels}.
#' Assumes model runs are in the same folder.
#' @examples
#' \dontrun{
#' 
#' mod_123 = compareModels(c("h1_0.00", "h1_0.01", "h1_0.02")
#' }
#' @export
compareModels <- function(mods)
{
  temp <- list()
  for(i in seq_along(mods)){
    temp[[i]] <- readJJM(mods[i], path = "config", input = "input")
  }
  cmd <- paste0("mods_comb <- combineModels(",paste(paste0("temp[[",seq_along(mods),"]]"),collapse=", "),")")
  eval(parse(text=cmd))
  
  return(mods_comb)
}

