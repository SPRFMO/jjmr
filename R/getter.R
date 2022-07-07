#' Get elements of a list by string matching on name
#'
#' @param x the object
#' @param pattern the string pattern (regex supported) to search for
#'
#' @return an object subset to matches in names with strings
#' @export
#'
getter <- function(x, pattern = "^sel_") {
  inds <-
    which(grepl(pattern, (names(x)))) # find entries that start with some form of sel_
  
  out <- x[inds] # pull out selectivity objects
  
}