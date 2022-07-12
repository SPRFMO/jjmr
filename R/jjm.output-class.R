

#' @export
print.jjm.output = function(x, ...) {
  
  for(i in seq_along(x)){
  
  obj = x[[i]]
  
  cat("JJM version: ", obj$info$data$version, "\n")
  cat("Model name: ", obj$info$output$model, "\n")
  cat("Stock number: ", paste(obj$info$output$nStock, collapse = ", "), "\n")
  
  if(obj$info$data$version == "2015MS"){
  stockNames = strsplit(obj$control$nameStock, "%")[[1]]
  cat("Stock names: ", paste(stockNames, collapse = ", "), "\n")
  }
  
  cat("Number of variables: ", obj$info$data$variables, "\n", sep = "")
  cat("Years from: ", obj$info$data$year[1] ,"to", obj$info$data$year[2], "\n", sep = " ")
  cat("Ages from: ", obj$info$data$age[1] ,"to", obj$info$data$age[2], "\n", sep = " ")
  cat("Lengths from: ", obj$info$data$length[1] ,"to", obj$info$data$length[2], "\n", sep = " ")
  cat("Fisheries names: ", paste(obj$info$output$fisheryNames, collapse = ", "), "\n")
  cat("Associated indices: ", paste(obj$info$output$indexModel, collapse = ", "), "\n")
  cat("jjm.data from ", sQuote(obj$info$data$file), "\n", sep = "")
  #cat("Projection years number: ", paste(length(obj$output$SSB_fut_1), collapse = ", "), "\n")
  cat(" ", "\n")
  
  }
  
  return(invisible())
  
}

#' @export
summary.jjm.output = function(object, Projections = FALSE, Fmult = NULL,
                              BiomProj = NULL, 
                              CapProj = NULL, 
                              MRS = NULL, ...) {
  
  pic = list()
  namesPlot = NULL
  for(i in seq_along(object)){
    
    jjm.stocks = object[[i]]$output
    
    for(j in seq_along(jjm.stocks)){
      
      jjm.out = jjm.stocks[[j]]
      jjm.in  = object[[i]]$data
      jjm.ypr = jjm.stocks[[j]]$YPR
      namesPlot[j] = as.list(names(object[[i]]$output))[[j]]
      
      pic[[j]] = .fit_summarySheet3FUN(jjm.out, scales = list(alternating = 1, tck = c(1,0),
                                                              y = list(relation = "free", rot = 0),
                                                              axs = "i"), ...)
    }

  }
  
  names(pic) = namesPlot

  output = list()
  output$parameters = .ParTable(object)
  output$like = .LikeTable(object)
  output$projections = .ProjTable(object, Projections = Projections,
                                  Fmult = Fmult, BiomProj = BiomProj, 
                                  CapProj = CapProj, MRS = MRS)
  output$plots = pic
    
  class(output) = "summary.jjm.output"

  return(output)
  
}

#' @export
print.summary.jjm.output = function(x, ...) {
  
  
  cat("\nParameter Table:\n\n")
  print(x$parameters, ...)
  
  cat("\nLikelihood Table:\n\n")
  print(x$like, ...)
  
  if(!is.null(x$projections)) cat("\nProjection Table (s):\n\n")
  
  for(i in seq_along(x$projections)) {
    
    cat(names(x$projections)[i], "\n")
    print(x$projections[[i]], ...)
    cat(" ", "\n")
    
  }
  
  cat("\nSummary Plot (s):\n\n")
  
  for(i in seq_along(x$plots)){
    
    cat(names(x$plots)[i], "\n")
    grid.newpage()
    grid.draw(x$plots[[i]])
  
  }
  
  return(invisible(x))
}


#' @export
plot.jjm.output = function(x, what = "biomass", stack = TRUE, endvalue = FALSE, total = FALSE, combine = FALSE,
                           cols = NULL, poslegend = "right", scen = 1, ...){
  
    switch(what, biomass     = .funPlotSeries(x, what, cols, stack, endvalue, poslegend, total, combine, ...),
               recruitment   = .funPlotSeries(x, what, cols, stack, endvalue, poslegend, total, combine, ...),
               ssb           = .funPlotSeries(x, what, cols, stack, endvalue, poslegend, total, combine, ...),
               noFishTB      = .funPlotSeries(x, what, cols, stack, endvalue, poslegend, total, combine, ...),
               ftot          = .funPlotSeries(x, what, cols, stack, endvalue, poslegend, total, combine, ...),
               kobe          = .funPlotKobe(x, what, cols, stack, endvalue, poslegend, ...),
               catchProj     = .funPlotProj(x, what, cols, stack, endvalue, poslegend, ...),
               ssbProj       = .funPlotProj(x, what, cols, stack, endvalue, poslegend, ...),
			   totalProj     = .funPlotTotProj(x, what, cols, stack, endvalue, poslegend, scen, ...), #review
			   catchProjScen = .funPlotScen(x, what, cols, stack, endvalue, poslegend, ...),          
			   ssbProjScen   = .funPlotScen(x, what, cols, stack, endvalue, poslegend, ...),         
			   ratioSSB_F    = .funPlotRatioSSB_F(x, what, cols, stack, endvalue, poslegend, ...),
			   ratioSSB      = .funPlotRatioSSB(x, what, cols, stack, endvalue, poslegend, ...),
			   selectivity = plot_selectivities(get_selectivities(x),...))
  
}

