# Retrospective analysis --------------------------------------------------

#' @title Run a retrospective analysis diagnostic for a JJM model
#' @description Run a retrospective analysis for a model
#' @param models An object of class jjm.output
#' @param n Number of years to run a retrospective analysis.
#' @param output Path to save results.
#' @param exec Path to JJM executable file.
#' @param parallel Boolean flag to run models in parallel.
#' @param temp Folder to run retrospective analysis. If NULL, a temporal folder is used. 
#' @param ... Additional arguments passed to other functions.
#' @examples
#' readJJMConfig(mod1)
#' @export
# retro = function(models, n=5, output="results", exec=NULL, iprint=100, 
#                  wait = TRUE, parallel=FALSE, temp=NULL, ...) {
#   
#   output = list()
#   for(i in seq_along(models)) {
#     
#     output[[i]] = .retro(model=models[i], n=n, output=output, exec=exec, iprint=iprint, 
#                    wait=wait, parallel=parallel, ...)
#     
#   }
#   
#   names(output) = names(models)
#   
#   return(output)
#   
# }

retro = function(model, n=5, output="results", exec=NULL, iprint=100, 
                  wait = TRUE, parallel=FALSE, temp=NULL, ...) {
  
  oPath = output
  if(length(model)>1) stop("only one model is allowed.")
  modName = names(model)
  
  models = list()
  for(i in 1:n) {
    mod = model[[1]]
    mod$control$Retro = i
    models[[i]] = mod
  }
  
  class(models) = c("jjm.output", class(models))
  
  names(models) = sprintf("%s_r%02d", modName, 1:n)
  
  temp = if(is.null(temp)) tempdir() else temp
  runJJM(models, output=oPath, exec=exec, iprint=iprint, wait=wait, parallel=parallel, temp=temp)
  
  ifsh = grep(x=names(model[[1]]$output[[1]]), patt="F_fsh", value = TRUE)
  ivar = c("SSB", "R", ifsh)
  .getRetro = function(x, ind) x[ind]
  
  output = list()
  output[[1]] = lapply(model[[1]]$output, FUN=.getRetro, ind=ivar)
  for(i in 1:n) {
    output[[i+1]] = .getRetroData(names(models)[i], output=oPath, ind=ivar)
  }
  names(output) = c(modName, names(models))
  class(output) = c("jjm.retro", class(output))
  
  oFile = file.path(oPath, paste(modName, "_retrospective.RData", sep=""))
  save(list = "output", file=oFile)
  return(output)
  
}

profiler = function(model, par, values, output="results", exec=NULL, iprint=100, 
                  wait = TRUE, parallel=FALSE, temp=NULL, ...) {
  

  if(length(model)>1) stop("only one model is allowed.")
  modName = names(model)
  
  models = list()
  for(i in seq_along(values)) {
    mod = model[[1]]
    mod$control[par] = 
    models[[i]] = mod
  }
  
  class(models) = c("jjm.output", class(models))
  
  names(models) = sprintf("%s_r%02d", modName, 1:n)
  
  temp = if(is.null(temp)) tempdir() else temp
  runJJM(models, output=output, exec=exec, iprint=iprint, wait=wait, parallel=parallel, temp=temp)
  
  ifsh = grep(x=names(model[[1]]$output[[1]]), patt="F_fsh", value = TRUE)
  ivar = c("SSB", "R", ifsh)
  .getRetro = function(x, ind) x[ind]
  
  output = list()
  output[[1]] = lapply(model$output, FUN=.getRetro, ind=ivar)
  for(i in 1:n) {
    output[[i+1]] = .getRetroData(model, output=file.path(temp, names(models)[i]), ind=ivar)
  }
  names(output) = c(modName, names(models))
  
  output = sortRetro(output)
  class(output) = c("jjm.retro", class(output))
  
  oFile = file.path(oPath, paste(modName, "_retrospective.RData", sep=""))
  save(list = "output", file=oFile)  
  return(output)
  
}


.getRetroData = function(model, output, ind, ...) {
  
  yld  = .getYldFile(model=model, output=output)
  reps = .getRepFiles(model=model, output=output)
  
  out = .readOutputsJJM(files=reps, yld=yld)
  
  .getRetro = function(x, ind) x[ind]
  output = lapply(out, FUN = .getRetro, ind=ind)
  
  return(output)
}


.xtract = function(x, s, i) x[[s]][[i]] 

.mutateMatrix = function(x, n, MARGIN=1) {
  if(n<=nrow(x)) return(x)
  newx = matrix(nrow=n, ncol=ncol(x))
  newx[1:nrow(x ), ] = x
  return(newx)
}

.sortRetro = function(object, iStock, iVar, n) {
  x = lapply(object, FUN=.xtract, s=iStock, i=iVar)
  nrowmax = max(sapply(x, nrow))
  x = do.call(cbind, lapply(x, FUN=.mutateMatrix, n=nrowmax))
  dim(x) = c(dim(x)[1], dim(x)[2]/n, n)
  time = x[,1,1]
  x = x[,-1,]
  output = list(time=time, var=x)
  return(output)
}

.sortRetroByStock = function(object, iStock, n) {
  
  nV = length(object[[1]][[1]])
  varNames = names(object[[1]][[1]])
  
  out = list()
  for(iVar in seq_len(nV)) {
    out[[iVar]] = .sortRetro(object=object, iStock=iStock, iVar=iVar, n=n)
  }
  names(out) = varNames
  
  return(out)
}

sortRetro = function(object) {
  
  n = length(object) # number of models (r+1)
  nS = length(object[[1]])
  stockNames = names(object[[1]])
  
  out = list()
  for(iStock in seq_len(nS)) {
    out[[iStock]] = .sortRetroByStock(object=object, iStock=iStock, n=n)
  }
  names(out) = stockNames
  return(out)
  
}
