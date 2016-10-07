
print.jjm.diag = function(x, ...) {
  
  cat("Model name (s):\n")
  
  cat(names(x), "\n")
  
  cat("\n")
  
  for(i in seq_along(x)) {
  
  obj = x[[i]]
  
  for(j in seq_along(obj)){
    OBJ = obj[[j]]
    #cat(paste(names(x[[i]])[j]), "\n\n")
    cat(paste(names(x[[i]])[j]), "Input Plots:\n", paste(OBJ$info$data, collapse = "\n "), "\n\n")
    cat(paste(names(x[[i]])[j]), "Output Plots:\n", paste(OBJ$info$output, collapse = "\n "), "\n\n")  
    }
  
  }
  
  return(invisible())  
}


summary.jjm.diag = function(object,...) {
  
  for(i in seq_along(object)){
    obj = object[[i]]
    for(j in seq_along(obj)){
      OBJ = obj[[j]]
      namesPlots = names(OBJ$info)[-1]
      output = lapply(namesPlots, .getResume, object = OBJ)
    }
  }

  names(output) = namesPlots
  
  class(output) = "summary.jjm.diag"
  
  return(output)  
}


print.summary.jjm.diag = function(x, ...) {
  
  class(x) = "list"
  
  cat("\nDetailed Input Plots:\n\n")
  
  print(x[[1]], ...)
  
  cat("\nDetailed Output Plots:\n\n")
  
  print(x[[2]], ...)
  
  return(invisible())
}


plot.jjm.diag = function(x, what = c("data", "output"), model=NULL, stock=NULL, 
                         var=NULL, fleet=NULL, plot=TRUE, ...) {
  what = tolower(what)
  
  if(any(grepl(pattern = "input", x=what))) {
    message("Parameter what='input' is deprecated, use 'data' instead.")
    what = gsub(pattern = "input", replacement = "data", x=what)
  }
  
  if(any(is.na(match(what, c("data", "output")))))
    stop("Incorrect values for parameter 'what'.")

  if(is.null(model)) model = names(x)
  stocks = stock

  if(isTRUE(plot)) {
    
    for(imodel in model) {
      if(is.null(stock)) stocks = names(x[[imodel]])
      for(istock in stocks)
        for(i in what)
          .plotDiag(x=x[[imodel]][[istock]][[i]], var=var, fleet=fleet, 
                    plot=plot, ...)
      
    }
    
    return(invisible())
    
  } else {
    
    out = list()
    j = 1
    
    for(imodel in model) {
      if(is.null(stock)) stocks = names(x[[imodel]])
      for(istock in stocks)
        for(i in what) {
          out[j] = .plotDiag(x=x[[imodel]][[istock]][[i]], var=var, fleet=fleet, 
                             plot=plot, ...)
          j = j+1
        }
    }
    
    return(invisible(out))
    
  }
  
}
