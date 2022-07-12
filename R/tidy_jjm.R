


#' Tidy results of JJM model
#'
#' @param models an object of class jjm.output
#'
#' @return a list of tidy dataframes
#' @export
#'
#' @examples
#'
#'\dontrun{
#' mod0.00 <- readJJM("h2_0.00", path = "config", input = "input")
#' tidy_jjm_results <- tidy_JJM(mod0.00)
#'
#' }
#'
#'
#'
#'
tidy_JJM <- function(models){
  
  gets <-  ls("package:jjmR")
  
  gets <- gets[grepl("^get_", gets)]
  
  thing_names <- gsub("get_","",gets)
  
  names(gets) <-thing_names
  
  foo <- function(getfoo, models){
    
    tmpfoo <- get(getfoo)
    
    out <- tmpfoo(models)
  }
  
  out <- lapply(gets, foo, models = models)
  
}