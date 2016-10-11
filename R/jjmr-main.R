# jjmR package Rd file ------------------------------------------------
#' Tools to process and get results from Joint Jack Mackerel (JJM) model outputs.
#' 
#' Graphics and diagnostics tools for SPRFMO's Joint Jack Mackerel model.
#' 
#' \tabular{ll}{ Package: \tab jjmR\cr Type: \tab Package\cr Version: \tab
#' 1.0\cr Date: \tab 2014-08-15\cr License: \tab TBD\cr }
#' 
#' @name jjmR-package
#' @aliases jjmR-package jjmR
#' @docType package
#' @author Ricardo Oliveros-Ramos, Wencheng Lau-Medrano, Giancarlo Moron 
#' Josymar Torrejon and Niels Hintzen
#' @seealso Joint Jack Mackerel Repository
#' \code{\link{https://github.com/SPRFMO/jjm}}
#' @keywords jjmr
#' 
#' 
NULL

# readJJM function --------------------------------------------------------

#' @title Read a model or list of models
#' @description Function to read models and list if models and generate results
#' @param model String with the name of model that will be readed or run.
#' @param path Directory where the 'admb' folder is located.
#' @param modelName alias for \code{model} (to be deprecated).
#' @param ... Extra arguments
#' @examples
#' readJJM(model = "mod2.4")
#' @export
readJJM = function(model, path = NULL, output="results", input=NULL, 
                   version="2015MS", ...) {
  
  ctl  = .getCtlFile(model=model, path=path) # path to ctl file
  dat  = .getDatFile(ctl=ctl, input=input) # path to dat file
  yld  = .getYldFile(model=model, output=output)
  par  = .getParFile(model=model, output=output)
  reps = .getRepFiles(model=model, output=output)

  # basic info
  
  modelName = .getModelName(ctl)  

  outputs    = .readOutputsJJM(files=reps, yld=yld)
  data       = .readDat(dat=dat, version=.versionJJM(ctl))
  info       = .getInfo(data=data, output=outputs, model=modelName)
  control    = .readCtl(ctl=ctl, info=info)
  parameters = .readPar(par=par, control=control, info=info)
  
  
  # Group in a list
  output = list()    										
  output[[modelName]] = list(info = info, data = data, control = control, 
                             parameters = parameters, output = outputs)
  
  class(output) = c("jjm.output")
  return(output)  
  
}

# Run JJM model -----------------------------------------------------------

#' @title Run a JJM model
#' @description Function to run one or several JJM models
#' @param models String with the name of the models to be run.
#' @param path Directory where the 'admb' folder is located.
#' @param output Folder to save the outputs, 'arc' by default.
#' @param useGuess boolean, to use an initial guess for the parameters?
#' @param guess File with the initial guess for the parameters. If \code{NULL}, will use \code{model.par} in the output folder. 
#' @param iprint iprint parameter for the JJM model, 100 by default.
#' @param piner A number to start the profiling on the meanlogrec
#' @param wait boolean, wait for the model to finish? Forced to be TRUE.
#' @param temp character, path for a temporal directory to run models, if \code{NULL} a temporal folder is automaticaly created.
#' @param ... Arguments passed from \code{system} function.
#' @examples
#' model = runJJM(models = "mod2.4")
#' @export
runJJM = function(models, ...) {
  UseMethod("runJJM")
}


# Diagnostics -------------------------------------------------------------

#' @title Generate Assessment plots from single model
#' @description Function to generate plots from results of readJJM function
#' @param object Object ob class outputs.
#' @param ... Extra arguments
#' @examples
#' model = readJJM(modelName = "mod2.4")
#' diagnostics(object = model)
#' @export
diagnostics = function(object, ...) {
  
  # Take an output object and get diagnostic plots extracting outputs, data and YPR
  output = list()
  
  for(i in seq_along(object)) {
     
    jjmStocks = object[[i]]$output
    version = object[[i]]$info$data$version
	
    output[[i]] = list()
    
    for(j in seq_along(jjmStocks)) {
	
		if(version != "2015MS")	{
			object[[i]]$data$wt_temp = object[[i]]$data$Pwtatage[,1]
			object[[i]]$data$mt_temp = object[[i]]$data$Pmatatage[,1]
			toJjm.in = object[[i]]$data
		} else {
			object[[i]]$control$wt_temp = t(object[[i]]$control$Pwtatage)[,j]
			object[[i]]$control$mt_temp = t(object[[i]]$control$Pmatatage)[,j]
			toJjm.in = c(object[[i]]$data, object[[i]]$control)
		}
	  
      output[[i]][[j]] = .diagnostics(jjm.info = object[[i]]$info$output,
                                  jjm.out  = jjmStocks[[j]], 
                                  jjm.in   = toJjm.in, ...)
      
    }
    
    names(output[[i]]) = names(jjmStocks)
    
  }
  
  names(output) = names(object)
  # Return a jjm.diag object
  class(output) = c("jjm.diag", class(output))
  return(output)
}

# Combine models ----------------------------------------------------------
#' @title Combine outputs
#' @description This function takes model objects (class \code{outputs}) of JJM and generate an object 
#' with combined models.
#' @param ... One or more output objects, to be combined to list of models.
#' @examples
#' mod1 <- runJJM(modelName = "mod2.1")
#' mod2 <- runJJM(modelName = "mod2.2")
#' mod3 <- runJJM(modelName = "mod2.3")
#' 
#' mod_123 = combineModels(mod1, mod2, mod3)
#' @export
combineModels = function(...)
{
  output = .combineModels(...)
  
  return(output)
}

# Write jjm files ---------------------------------------------------------------

#' @title Write dat and ctl files from a JJM model stored in R
#' @description Function write to the disk dat and ctl files
#' @param object An object of class jjm.config or jjm.output.
#' @param path Directory where the configuration files will be written.
#' @examples
#' writeJJM(mod1)
#' @export
writeJJM = function(object, path, ...) {
UseMethod("writeJJM")
}

writeJJM.jjm.output = function(object, path = NULL, ctlPath=path, datPath=path) {
  
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
# Read jjm config ---------------------------------------------------------------

#' @title Read dat and ctl files from disk to create a jjm.config object.
#' @description Store in an R object (of class jjm.config) the dat and ctl files needed
#' to run a model.
#' @param data Path to the data file.
#' @param control Path to the control file.
#' @param ... Additional arguments passed to other functions.
#' @examples
#' readJJMConfig(mod1)
#' @export
readJJMConfig = function(data, control, ...){
    UseMethod("readJJMConfig")
}

readJJMConfig.default = function(data, control, ...){
		
  ctl  = .getCtlFile(model=model, path=path) # path to ctl file
  dat  = .getDatFile(ctl=ctl, input=input) # path to dat file
  
  output <- .getJjmConfig(data = data, control = control, ...)
  
  return(output)
	
}

readJJMConfig.jjm.output = function(data, control, ...){
  
  ctl  = .getCtlFile(model=model, path=path) # path to ctl file
  dat  = .getDatFile(ctl=ctl, input=input) # path to dat file
  
  output <- .getJjmConfig(data = data, control = control, ...)
  
  return(output)
  
}

# Combine jjm config ---------------------------------------------------------------

combineConfig = function(...){
		
  output <- .combineConfig(...)
  
  return(output)
	
}

# Read external files ---------------------------------------------------------------

readExFiles = function(fileName, type, path = NULL, version = "2015MS", parameters = FALSE,  
                       parData, nameFishery, nameIndex, nAges, nStock = NULL){
  
  fileName = if(is.null(path)) fileName else file.path(path, fileName)
  
  if( type != "data" & type != "control") stop("File must be data or control type")
  
  if(type == "data"){
    outList = .read.datEx(filename = fileName, version = version)
  }
  
  if(type == "control"){
    if(is.null(nStock)) stop("The number of stocks is necessary")
    
    if(parameters){
      info = list()
      info$fisheryNames = .splitPor(parData$nameFish)
      info$indexModel = .splitPor(parData$nameIndex)
      info$nStock = nStock
      info$filename = fileName
      infoDat = list()
      infoDat$age = c(1, parData$LastAge)
    } 
    if(!parameters){
      info = list()
      info$fisheryNames = nameFishery
      info$indexModel = nameIndex
      info$nStock = nStock
      info$filename = fileName
      infoDat = list()
      infoDat$age = c(1, nAges)
    }
    
    if(version != "2015MS"){ 
      outList = .read.ctl(filename = fileName, info = info, infoDat = infoDat)
    } else {
      outList = .read.ctlMS(filename = fileName, info = info, infoDat = infoDat)
    }
  }
  
  return(outList)
  
}

