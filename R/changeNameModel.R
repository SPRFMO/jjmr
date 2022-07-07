# Change model name ----------------------------------------------------------
#' @title Change the internal name of a model
#' @description This function internally replaces the name of a JJM output object with a user-specified string.
#' Mostly useful for plots.
#' @examples
#' \dontrun{
#' recmods <- compareModels(c("mod1.00.hl","mod1.00.ll","mod1.00.hs","mod1.00.ls"))
#' 
#' changeNameModel(recmods,c( "h=0.8, full series","h=0.8, short series","h=0.65, full series","h=0.65, short series" ))
#' }
#' @export
changeNameModel = function(modList, nameVector){
  for(i in seq_along(modList)){
    modList[[i]]$info$output$model <- nameVector[i]
  }
  return(modList)
}

