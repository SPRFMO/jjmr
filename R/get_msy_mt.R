#' Get and tidy msy_my table
#'
#' @param models series of class jjm.output
#'
#' @return a tidy msy_mt
#' @export
#'
#' @examples
#' \dontrun{
#'
#' mod0.00 <- readJJM("h2_0.00", path = "config", input = "input")
#' get_msy_mt(mod0.00)
#' }
#'
get_msy_mt <- function(models) {
  flatten_stocks <- function(x) {
    flat_msy_mt <-
      (purrr::map_df(x$output, function(z)
        as.data.frame(z$msy_mt), .id = "stock"))
    
    # in case you are reading in an old model fit that wasn't run with updated readJJM
    if (any(grepl("^V", names(flat_msy_mt)))) {
      msy_mt_names <-
        c(
          "year",
          "fspr",
          "survivespr",
          "f_fmsy",
          "fmsy",
          "f",
          "fsprmsy",
          'msy',
          "msyl",
          "bmsy",
          "bzero",
          "ssb",
          "b_bmsy"
        )
      
      names(flat_msy_mt)[grepl("^V", names(flat_msy_mt))] <-
        msy_mt_names
      
      #names of columns of msy_mt provided by Lee Qi
      # // Yr Fspr 1-Fspr F/Fmsy Fmsy F Fsprmsy MSY MSYL Bmsy Bzero SSB B/Bmsy
      
    }
    
    return(flat_msy_mt)
  }
  
  
  msy_mt_results <-
    purrr::map_df(models, flatten_stocks, .id = "model")
  
  return(msy_mt_results)
  
}