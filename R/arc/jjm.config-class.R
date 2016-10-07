
.getJjmCongif = function(data, control, ...) {
  
  out = list()
  out[[1]] = list()
  Mname = control$modelName
  
  out[[1]]$Dat = data
  out[[1]]$Ctl = control
  names(out) = Mname
  
  # Define jjm.output class
  class(out) = c("jjm.config")
  
  return(out)
  
}


print.jjm.config = function(x, ...) {
  
  return(invisible())
}

summary.jjm.config = function(object,...) {
  
  output = NULL
  
  class(output) = "summary.jjm.config"
  
  return(output)  
}

print.summary.jjm.config = function(x, ...) {
  
  return(invisible())
}


# runJJM ------------------------------------------------------------------

runJJM.default = function(models, path=NULL, output="results", input=NULL, 
                          exec=NULL, version=NULL, useGuess=FALSE, guess=NULL, 
                          iprint=100, wait = TRUE, parallel=FALSE, 
                          temp=NULL, ...) {
  
  oldwd = getwd()
  on.exit(setwd(oldwd))
  
  if(!file.exists(output)) dir.create(output, recursive = TRUE)
  output = normalizePath(output, mustWork = TRUE)
  
  guess  = .checkGuess(models, guess, output) 
  
  if(!is.null(path)) models = file.path(path, models)
  
  exec   = .checkExecutable(exec=exec, version=version)
  models = .checkModels(models)
  
  # Run models
  base  = getwd()
  start = proc.time() 
  
  if(is.null(temp)) temp = tempdir()
  
  if(!isTRUE(parallel)) {
    
    cat("\nRunning models", paste(models, collapse=", "), "\n")
    cat("\tStarting at", as.character(Sys.time()), "\n")
    
    res = NULL
    for(i in seq_along(models)) {
      rtime = .runJJM(model=models[i], output=output, input=input, exec=exec, 
                      useGuess=useGuess, guess=guess[i], iprint=iprint, 
                      wait=wait, temp=temp, ...)
      res = c(res, rtime)  
    }
    
  } else {
    
    cat("\nRunning models", paste(models, collapse=", "), "in parallel.\n")
    cat("\tStarting at", as.character(Sys.time()), "\n")
    tempDir = tempdir()
    res = foreach(i=seq_along(models), .combine=c) %dopar% {
      setwd(base)
      .runJJM(model=models[i], output=output, exec=exec, useGuess=useGuess, 
              guess=guess[i], iprint=iprint, wait=wait, temp=temp, ...)  
    }  
    
  }
  
  setwd(base)
  cat("\tEnded at", as.character(Sys.time()), "\n")
  
  elapsed = proc.time() - start
  names(res) = models
  cat("\nModel runs finished.\nTotal models run time:\n")
  print(res)
  cat("\nEffective running time:", round(elapsed[3], 1), "s.\n")
  
  cat("\n Models were run at", temp, "folder.")
  
  return(invisible(temp))
}


runJJM.jjm.config = function(models, path=NULL, output="results", input=NULL, 
							exec=NULL, version=NULL, useGuess=FALSE, guess=NULL, 
							iprint=100, wait = TRUE, parallel=FALSE, 
							temp=NULL, ...) {
  
  modNames = tolower(names(models))
  if(is.null(temp)) temp = tempdir()
  
  for(i in seq_along(models)){
	writeJJM(object = models[[i]]$Dat, outFile = models[[i]]$Ctl$dataFile, path = temp) 
	writeJJM(object = models[[i]]$Ctl, outFile = paste0(modNames, ".ctl"), path = temp) 
  }
  
  runJJM.default(models = modNames, path=temp, output=output, input=temp, 
                 exec=exec, version=version, useGuess=useGuess, guess=guess, 
                 iprint=iprint, wait = wait, parallel=parallel, 
                 temp=temp, ...)
  
  return(invisible())
}
