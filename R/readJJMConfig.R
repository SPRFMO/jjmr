#' @title Read dat and ctl files from disk to create a jjm.config object.
#' @description Store in an R object (of class jjm.config) the dat and ctl files needed
#' to run a model.
#'
#' @param model Model object or outputs
#' @param path Path to the ctl file
#' @param input Path to the input files
#' @param ... Additional arguments passed to other functions.
#'
#' @examples
#' \dontrun{
#' readJJMConfig(mod1)
#' }
#' @export
readJJMConfig = function(model, path, input=NULL, ...) {
  UseMethod("readJJMConfig")
}

#' @export
readJJMConfig.default = function(model, path=NULL, input=NULL, output="results", ...) {
  
  ctl  = .getCtlFile(model=model, path=path) # path to ctl file
  dat  = .getDatFile(ctl=ctl, input=input) # path to dat file
  
  reps = .getRepFiles(model=model, output=output)
  yld  = .getYldFile(model=model, output=output)
  outputs    = .readOutputsJJM(files=reps, yld=yld)
  data       = .readDat(dat=dat, version=.versionJJM(ctl))
  info       = .getInfo(data=data, output=outputs, model=model)
  control    = .readCtl(ctl=ctl, info=info)
  
  output = .getJjmConfig(data = data, control = control)
  
  return(output)
  
}

#' @export
readJJMConfig.jjm.output = function(model, path, input=NULL, ...) {
  
  ctl  = .getCtlFile(model=model, path=path) # path to ctl file
  dat  = .getDatFile(ctl=ctl, input=input) # path to dat file
  
  reps = .getRepFiles(model=model, output=output)
  yld  = .getYldFile(model=model, output=output)
  outputs    = .readOutputsJJM(files=reps, yld=yld)
  data       = .readDat(dat=dat, version=.versionJJM(ctl))
  info       = .getInfo(data=data, output=outputs, model=model)
  control    = .readCtl(ctl=ctl, info=info)
  
  output = .getJjmConfig(data = data, control = control)
  
  return(output)
  
}

