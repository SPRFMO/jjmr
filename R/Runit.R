#' @title Fit, run, read and plot a JJM model
#' @description Shortcut to fit, run, read and plot a JJM model
#'
#' @param mod A character specifying the name of a model (by it's ctl filename).
#' @param est Boolean, should we run the parameter estimation for a model?
#' @param exec Path to the JJM executable file. By default, 'jjms' will be used.
#' @param path Directory where the configuration files will be written.
#' @param input Input
#' @param output Folder to save the outputs, 'arc' by default.
#' @param version version of JJM, default to "2015MS" (2015 SC multi-stock).
#' @param pdf Produce outputs in a pdf file?
#' @param portrait Orientation of the pdf output, default TRUE.
#'
#' @examples
#' \dontrun{
#' writeJJM(mod1)
#' }
#' @export
runit = function(mod, est=FALSE, exec=NULL, path="config", input="input", output="results",
                 version="2015MS", pdf=FALSE, portrait=TRUE) {
  
  
  if(isTRUE(est)) {
    if(is.null(exec)) {
      exec = "jjms"
      message(sprintf("Using '%s' as default executable, check 'exec' argument.", exec))
    }
    runJJM(mod, path=path, input=input, output=output, version=version, exec=exec)
  }
  modtmp = readJJM(mod, path=path, input=input, output=output, version=version)
  
  dims = if(isTRUE(portrait)) c(9,7) else c(7,9)
  
  if(pdf) {
    pdf(file.path(output, paste0(mod,".pdf")), height=dims[1], width=dims[2])
    plot(diagnostics(modtmp))
    dev.off()
  }
  return(modtmp)
}

