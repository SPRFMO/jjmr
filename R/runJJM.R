#' @title Run a JJM model
#' @description Function to run one or several JJM models
#'
#' @param models String with the name of the models to be run.
#' @param path Directory where the 'admb' folder is located.
#' @param output Folder to save the outputs, 'arc' by default.
#' @param input Input
#' @param useGuess boolean, to use an initial guess for the parameters?
#' @param guess File with the initial guess for the parameters. If \code{NULL}, will use \code{model.par} in the output folder. 
#' @param iprint iprint parameter for the JJM model, 100 by default.
#' @param piner A number to start the profiling on the meanlogrec
#' @param wait boolean, wait for the model to finish? Forced to be TRUE.
#' @param temp character, path for a temporal directory to run models, if \code{NULL} a temporal folder is automaticaly created.
#' @param exec Path to the jjm executable
#' @param version version of JJM, default to "2015MS" (2015 SC multi-stock).
#' @param parallel Should model run in parallel? A cluster need to be setup to be used with foreach.
#' @param ... Arguments passed from \code{system} function.
#'
#' @examples
#' \dontrun{
#' model = runJJM(models = "mod2.4")
#' }
#' @export
runJJM = function(models, path=NULL, output="results", input=NULL, 
                  exec=NULL, version=NULL, useGuess=FALSE, guess=NULL, piner=NULL,
                  iprint=100, wait = TRUE, parallel=FALSE, 
                  temp=NULL, ...) {
  UseMethod("runJJM")
}


