#' Get elements of a list by string matching on name
#'
#' @param x the object
#' @param pattern the string pattern (regex supported) to search for
#'
#' @return an object subset to matches in names with strings
#' @export
#'
getter <- function(x, pattern = "^sel_", things = NA) {
  
  if (all(is.na(things))){
    inds <-
      which(grepl(pattern, (names(x)), perl = TRUE)) 
  } else {
    inds <-
      which(names(x) %in% things) 
  }

  
  out <- x[inds] # pull out selectivity objects
  
}