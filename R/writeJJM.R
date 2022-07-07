#' @title Write dat and ctl files from a JJM model stored in R
#' @description Function write to the disk dat and ctl files
#'
#' @param object An object of class jjm.config or jjm.output.
#' @param path Directory where the configuration files will be written.
#' @param ... Additional arguments
#'
#' @examples
#' \dontrun{
#' writeJJM(mod1)
#' }
#' @export
writeJJM = function(object, path, ...) {
  UseMethod("writeJJM")
}

#' @export
writeJJM.jjm.output = function(object, path = NULL, ctlPath=path, datPath=path, ...) {
  
  for(i in seq_along(object)) {
    obj = object[[i]]
    .writeJJM(object = obj$data, outFile = obj$control$dataFile, path = datPath) 
    .writeJJM(object = obj$control, outFile = paste0(names(object)[i], ".ctl"), path = ctlPath, 
              transpose=FALSE) 
  }
  
  return(invisible(NULL))
}

# writeJJM.jjm.config = function(object, path = NULL) {
#   
#   modName = if(is.null(model)) deparse(substitute(object)) else model
#   
#   .writeJJM(object = object$Dat, outFile = object$Ctl$dataFile, path = path) 
#   .writeJJM(object = object$Ctl, outFile = paste0(modName, ".ctl"), path = path)   
#   
#   return(invisible(NULL))
# }

