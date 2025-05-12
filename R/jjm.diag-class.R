#' Print method for jjm.diag objects
#'
#' Prints a summary of the JJM diagnostic object, displaying information about
#' available input and output plots for each model and stock.
#'
#' @param x An object of class 'jjm.diag'
#' @param ... Additional arguments passed to print methods
#'
#' @return Invisibly returns the input object (for use in pipelines)
#' @export
#' 
#' @examples
#' \dontrun{
#' diagnostics <- diag.jjm("path/to/model")
#' print(diagnostics)
#' }
print.jjm.diag <- function(x, ...) {
  
  cat("Model name(s):\n")
  cat(names(x), "\n\n")
  
  for(i in seq_along(x)) {
    obj <- x[[i]]
    
    for(j in seq_along(obj)) {
      OBJ <- obj[[j]]
      cat(paste(names(x[[i]])[j]), "Input Plots:\n", 
          paste(OBJ$info$data, collapse = "\n "), "\n\n")
      cat(paste(names(x[[i]])[j]), "Output Plots:\n", 
          paste(OBJ$info$output, collapse = "\n "), "\n\n")  
    }
  }
  
  return(invisible(x))  
}

#' Summary method for jjm.diag objects
#'
#' Creates a detailed summary of JJM diagnostics, including information
#' about input and output plots for model evaluation.
#'
#' @param object An object of class 'jjm.diag'
#' @param ... Additional arguments passed to internal functions
#'
#' @return An object of class 'summary.jjm.diag' containing detailed diagnostics
#' @export
#'
#' @examples
#' \dontrun{
#' diagnostics <- diag.jjm("path/to/model")
#' diag_summary <- summary(diagnostics)
#' print(diag_summary)
#' }
summary.jjm.diag <- function(object, ...) {
  
  output <- NULL
  
  for(i in seq_along(object)) {
    obj <- object[[i]]
    for(j in seq_along(obj)) {
      OBJ <- obj[[j]]
      namesPlots <- names(OBJ$info)[-1]
      output <- lapply(namesPlots, .getResume, object = OBJ)
    }
  }
  
  names(output) <- namesPlots
  class(output) <- "summary.jjm.diag"
  
  return(output)  
}

#' Print method for summary.jjm.diag objects
#'
#' Prints the detailed diagnostics information from a summary.jjm.diag object,
#' showing both input and output plot details.
#'
#' @param x An object of class 'summary.jjm.diag'
#' @param ... Additional arguments passed to print methods
#'
#' @return Invisibly returns the input object (for use in pipelines)
#' @export
print.summary.jjm.diag <- function(x, ...) {
  
  class(x) <- "list"
  
  cat("\nDetailed Input Plots:\n\n")
  print(x[[1]], ...)
  
  cat("\nDetailed Output Plots:\n\n")
  print(x[[2]], ...)
  
  return(invisible(x))
}

#' Plot method for jjm.diag objects
#'
#' Creates diagnostic plots from the JJM model diagnostics based on specified parameters.
#' Can generate plots for model inputs (data) or outputs across different models and stocks.
#'
#' @param x An object of class 'jjm.diag'
#' @param what Character vector specifying which type of diagnostics to plot.
#'        Options are "data" (or deprecated "input") and "output". Default is c("data", "output").
#' @param model Character vector of model names to include. If NULL (default), all models are used.
#' @param stock Character vector of stock names to include. If NULL (default), all stocks are used.
#' @param var Character vector of variable names to include. If NULL (default), all variables are used.
#' @param fleet Character vector or numeric vector of fleet indices to include. 
#'        If NULL (default), all fleets are used.
#' @param plot Logical. If TRUE (default), plots are displayed. If FALSE, plot objects are returned.
#' @param ... Additional arguments passed to internal plotting functions
#'
#' @return If plot=TRUE, returns invisibly. If plot=FALSE, returns a list of plot objects.
#' @export
#'
#' @examples
#' \dontrun{
#' diagnostics <- diag.jjm("path/to/model")
#' # Plot all diagnostics
#' plot(diagnostics)
#' 
#' # Plot only data diagnostics for a specific model and stock
#' plot(diagnostics, what = "data", model = "Model1", stock = "Stock1")
#' 
#' # Return plot objects without displaying
#' plot_objects <- plot(diagnostics, plot = FALSE)
#' }
plot.jjm.diag <- function(x, what = c("data", "output"), model = NULL, stock = NULL, 
                          var = NULL, fleet = NULL, plot = TRUE, ...) {
  what <- tolower(what)
  
  if(any(grepl(pattern = "input", x = what))) {
    message("Parameter what='input' is deprecated, use 'data' instead.")
    what <- gsub(pattern = "input", replacement = "data", x = what)
  }
  
  if(any(is.na(match(what, c("data", "output"))))) {
    stop("Incorrect values for parameter 'what'. Valid options are 'data' and 'output'.")
  }
  
  if(is.null(model)) {
    model <- names(x)
  }
  
  stocks <- stock
  
  if(isTRUE(plot)) {
    for(imodel in model) {
      if(is.null(stock)) {
        stocks <- names(x[[imodel]])
      }
      
      for(istock in stocks) {
        for(i in what) {
          .plotDiag(x = x[[imodel]][[istock]][[i]], 
                    var = var, 
                    fleet = fleet, 
                    plot = plot, 
                    ...)
        }
      }
    }
    
    return(invisible())
  } else {
    out <- list()
    j <- 1
    
    for(imodel in model) {
      if(is.null(stock)) {
        stocks <- names(x[[imodel]])
      }
      
      for(istock in stocks) {
        for(i in what) {
          out[j] <- .plotDiag(x = x[[imodel]][[istock]][[i]], 
                              var = var, 
                              fleet = fleet, 
                              plot = plot, 
                              ...)
          j <- j + 1
        }
      }
    }
    
    return(invisible(out))
  }
}
