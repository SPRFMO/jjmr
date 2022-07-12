#' @title Generate Assessment plots from single model
#' @description Function to generate plots from results of readJJM function
#' @param object Object ob class outputs.
#' @param ... Extra arguments
#' @examples
#' \dontrun{
#' model = readJJM(modelName = "mod2.4")
#' diagnostics(object = model)
#' }
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

