# Retrospective analysis --------------------------------------------------

#' @title Run a retrospective analysis diagnostic for a JJM model
#' @description Run a retrospective analysis for a model
#' @param model An object of class jjm.output
#' @param n Number of years to run a retrospective analysis.
#' @param output Path to save results.
#' @param exec Path to JJM executable file.
#' @param parallel Boolean flag to run models in parallel.
#' @param temp Folder to run retrospective analysis. If NULL, a temporal folder is used. 
#' @param wait Boolean, passed to runJJM, should we wait for the parameter estimation?
#' @param iprint Command line argument passed to jjm.
#' @param ... Additional arguments passed to other functions.
#' @examples
#' \dontrun{
#' retro(mod1)
#' }
#' @export
retro = function(model, n=5, output="results", exec=NULL, parallel=FALSE,  
                  temp=NULL, wait = TRUE,  iprint=100, ...) {
  
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
  
  ifsh = grep(x=names(model[[1]]$output[[1]]), pattern="F_fsh", value = TRUE)
  ivar = c("SSB", "R", ifsh)
  .getRetro = function(x, ind) x[ind]
  
  output = list()
  output[[1]] = lapply(model[[1]]$output, FUN=.getRetro, ind=ivar)
  for(i in 1:n) {
    output[[i+1]] = .getRetroData(names(models)[i], output=oPath, ind=ivar)
  }
  names(output) = c(modName, names(models))

  output = sortRetro(output)

  class(output) = c("jjm.retro", class(output))
  
  oFile = file.path(oPath, paste(modName, "_retrospective.RData", sep=""))
  save(list = "output", file=oFile)  
  return(output)
}

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


# plot method -------------------------------------------------------------

#' @export
plot.jjm.retro = function(x, var=NULL, std=FALSE, ci=TRUE, lty=1, lwd=2, alpha=0.12, xlim=NULL, ylim=NULL, ...) {
  lapply(x, FUN = .plotRetroByStock, var=var, ci=ci, lty=lty, lwd=lwd, std=std, alpha=alpha, xlim=xlim, ylim=ylim, ...)
  return(invisible())
}

# auxiliar

.normRetro = function(x) {
  return(x/as.numeric(x[,1,1]) - 1)
}

.plotRetro = function(object, var, ci = TRUE, lty=1, lwd=2, std=TRUE, alpha=0.12, xlim=NULL, ylim=NULL, ...) {
  # ssb = object[[iStock]][[var]]
  ssb = object[[var]]
  ylab = var

  ci = if(dim(ssb$var)[2]==4) ci else FALSE
    
  if(isTRUE(std)) {
    ssb$var = .normRetro(ssb$var)
    ylab = sprintf("%s relative change", var)
  }
  
  n = dim(ssb$var)[3]
  
  if(is.null(xlim)) xlim = range(ssb$time, na.rm=TRUE)
  if(is.null(ylim)) ylim = range(ssb$var[, -2, ], na.rm=TRUE)*c(0.8, 1.2)
  plot.new()
  plot.window(xlim=xlim, ylim=ylim)
  
  for(i in seq_len(n)) {
    if(isTRUE(ci)) {
      .linesCI(ssb$time, ssb$var[, c(1,3,4), i], lwd=3, col=i, alpha=alpha)
    } else {
      lines(ssb$time, ssb$var[, 1, i], lwd=3, col=i)
    }
  }
  mtext(var, 3, line=-2, adj=0.05, cex=1.8)
  axis(1)
  axis(2, las=2)
  box()

  legend("topright", lty=lty, col=1:n, legend = 1:n - 1, lwd=lwd, bty="n")
  return(invisible())
}


.plotRetroByStock = function(x, var=NULL, lty=1, lwd=2, std=TRUE, ci=TRUE, alpha=0.12, xlim=NULL, ylim=NULL, ...) {
  if(is.null(var)) var = names(x)
  for(iVar in var)
    .plotRetro(object=x, var=iVar, ci=ci, lty=lty, lwd=lwd,std=std, alpha=alpha, xlim=xlim, ylim=ylim, ...)
  return(invisible())
}

.linesCI = function(x, y, col="grey", alpha=0.12, ...) {
  
  ind = complete.cases(y)
  y = y[ind, ]
  x = x[ind]
  
  binf = y[, 2]
  bsup = y[, 3]
  b50  = y[, 1]
  
  x.pol = c(x, rev(x), x[1])
  y.pol = c(binf, rev(bsup), binf[1])
  
  polygon(x.pol, y.pol, col=makeTransparent(col, alpha=alpha), border=NA)
  lines(x, b50, col=col, ...)
  return(invisible())
}

