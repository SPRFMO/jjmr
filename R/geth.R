#' Add hypothesis number to a model name
#'
#' @param mod A character string of a model name.
#' @param h A character string containing the hypothesis to use.
#' @return A character string containing the hypothesis name and the model name.
#' @examples
#' geth("1.00", "h1")
#' @export

geth <- function(mod,h=hyp) paste0(h,"_", mod)