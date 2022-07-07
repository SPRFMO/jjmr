#' @title Read external files
#' @description Read external files
#' 
#' @param fileName filename
#' @param type type
#' @param path path
#' @param version version of JJM, default to "2015MS" (2015 SC multi-stock).
#' @param parameters parameters
#' @param parData parData
#' @param nameFishery nameFishery
#' @param nameIndex nameIndex
#' @param nAges nAges
#' @param nStock nStock
#'
#' @export
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

