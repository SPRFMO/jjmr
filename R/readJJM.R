#' @title Read a model or list of models
#' @description Function to read models and list if models and generate results
#'
#' @param model String with the name of model that will be readed or run.
#' @param path Directory where the 'admb' folder is located.
#' @param output Path to the model outputs directory.
#' @param input Path to model inputs directory.
#' @param version version of JJM, default to "2015MS" (2015 SC multi-stock).
#' @param ... Extra arguments
#'
#' @examples
#' \dontrun{
#' readJJM(model = "mod2.4")
#' }
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
  
  msy_mt_names <- c("year", "fspr", "survivespr", "f_fmsy", "fmsy","f","fsprmsy",'msy',"msyl","bmsy","bzero","ssb","b_bmsy") #names of columns of msy_mt provided by Lee Qi
# // Yr Fspr 1-Fspr F/Fmsy Fmsy F Fsprmsy MSY MSYL Bmsy Bzero SSB B/Bmsy
  
  namer <- function(x, msy_mt_names) {
    
    if (ncol(x$msy_mt) != length(msy_mt_names)){
      stop("Dimensions of msy_mt no longer match supplied names of each column")
    }
    colnames(x$msy_mt) = msy_mt_names # assign column names for core outputs
    return(x)
  }
  
  outputs <- lapply(outputs,namer,msy_mt_names = msy_mt_names)
  
  
  # Group in a list
  output = list()    										
  output[[modelName]] = list(info = info, data = data, control = control, 
                             parameters = parameters, output = outputs)
  
  class(output) = c("jjm.output")
  return(output)  
  
}

