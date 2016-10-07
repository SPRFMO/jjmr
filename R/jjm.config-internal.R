.combineConfig = function(...){
  
  modelList = list(...)
  
  for(i in modelList)
    if(class(i) != "jjm.config")
      stop("Objects must be of class 'jjm.config'.")
  
  modelList = c(...)
  
  # Remove repeated models from modelList 
  modelList2 = modelList
  for(i in seq_along(modelList[-length(modelList)]))
    for(j in seq(from = i + 1, to = length(modelList)))
      if(identical(modelList[[i]], modelList[[j]]))
        modelList2[[j]] = NULL
  
  modelList = modelList2; rm("modelList2")

  class(modelList) = c("jjm.config")
  
  return(modelList)

}
