
report = function(object, format, output, ...) {
  UseMethod("report")
}

report.jjm.output = function(object, format="latex", output=NULL, Fmult = NULL,
                             BiomProj = NULL, CapProj = NULL, verbose=TRUE,
                             MRS = NULL, tangle=FALSE, tidy=TRUE, ...) {
  
  if(is.null(BiomProj)) nBiom = 3
  if(!is.null(BiomProj)) nBiom = length(BiomProj)
  if(is.null(CapProj)) nCap = 2
  if(!is.null(CapProj)) nCap = length(CapProj)
  
  modelName = deparse(substitute(object))
  
  if(is.null(output)) output = getwd()
  
  skeleton = system.file("reports", "report-jjm.output.Rmd", package = "jjmR")
  
  if(isTRUE(tangle)) {
    knit(skeleton, tangle=TRUE)
    f1 = gsub(pattern = ".Rmd", replacement = "\\.R", skeleton)
    file.rename(from=basename(f1), to=paste0(modelName, ".R"))
  }
  
  outputFile = paste0(modelName, "_output.pdf")
  render(skeleton, c("pdf_document"), output_file=outputFile, output_dir=output)
  
  if(isTRUE(open)) shell.exec(outputFile)
  
  return(invisible(file.path(output, outputFile)))
  
}

report.jjm.diag = function(object, format="latex", output=NULL, tangle=FALSE, 
                           tidy=TRUE, open=TRUE, ...) {
  
  modelName = deparse(substitute(object))
  
  if(is.null(output)) output = getwd()
  
  skeleton = system.file("reports", "report-jjm.diag.Rmd", package = "jjmR")
  
  if(isTRUE(tangle)) {
    knit(skeleton, tangle=TRUE)
    f1 = gsub(pattern = ".Rmd", replacement = "\\.R", skeleton)
    file.rename(from=basename(f1), to=paste0(modelName, ".R"))
  }
  
  outputFile = paste0(modelName, "_diag.pdf")
  render(skeleton, c("pdf_document"), output_file=outputFile, output_dir=output)
  
  if(isTRUE(open)) shell.exec(outputFile)
  
  return(invisible(file.path(output, outputFile)))
  
}