#' @title Create a report from JJM outputs
#' @description Function to create and save reports in PDF and MS Word formats.
#' @param object The object to create the report with, can be of classes 
#' 'jjm.output' or 'jjm.diag' as created with \code{readJJM} or \code{diagnostics}.
#' @param format Format for the report: either "pdf", "html" or "word".
#' @param output Path to save the report, by default the working directory.
#' @param tangle Boolean, if TRUE the R script to create the report is produced.
#' @param tidy Boolean, if TRUE the intermediate files (Rmd, tex) are deleted. 
#' @param ... Extra arguments
#' @examples
#' \dontrun{
#' report(mod0.0)
#' }
#' @export
report = function(object, format, output, tidy, tangle, ...) {
  UseMethod("report")
}

report.jjm.output = function(object, format="pdf", output=NULL, Fmult = NULL,
                             BiomProj = NULL, CapProj = NULL, verbose=TRUE,
                             MRS = NULL, tangle=FALSE, tidy=TRUE, ...) {
  
  nBiom =  if(is.null(BiomProj))  3 else length(BiomProj)
  nCap = if(is.null(CapProj))  2 else length(CapProj)
  
  modelName = deparse(substitute(object))
  
  if(is.null(output)) output = getwd()
  
  skeleton = system.file("reports", "report-jjm.output.Rmd", package = "jjmR")
  
  if(isTRUE(tangle)) {
    knit(skeleton, tangle=TRUE)
    f1 = gsub(pattern = ".Rmd", replacement = "\\.R", skeleton)
    file.rename(from=basename(f1), to=paste0(modelName, ".R"))
  }
  
  extFormat = if(format=="word") "docx" else format
  outputFile = paste0(modelName, "_output.", extFormat)
  render(skeleton, paste(format, "_document", sep=""), output_file=outputFile, output_dir=output)
  
  if(isTRUE(open)) shell.exec(outputFile)
  
  return(invisible(file.path(output, outputFile)))
  
}

report.jjm.diag = function(object, format="pdf", output=NULL, tangle=FALSE, 
                           tidy=TRUE, open=TRUE, ...) {
  
  modelName = deparse(substitute(object))
  
  if(is.null(output)) output = getwd()
  
  skeleton = system.file("reports", "report-jjm.diag.Rmd", package = "jjmR")
  
  if(isTRUE(tangle)) {
    knit(skeleton, tangle=TRUE)
    f1 = gsub(pattern = ".Rmd", replacement = "\\.R", skeleton)
    file.rename(from=basename(f1), to=paste0(modelName, ".R"))
  }
 
  extFormat = if(format=="word") "docx" else format
  outputFile = paste0(modelName, "_diag.", extFormat)
  render(skeleton, paste(format, "_document", sep=""), output_file=outputFile, output_dir=output)
  
  if(isTRUE(open)) shell.exec(outputFile)
  
  return(invisible(file.path(output, outputFile)))
  
}
