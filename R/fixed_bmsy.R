#' Calculate or input a fixed Bmsy value for the jjm model
#' Updates the msy_mt table in the jjm output with new B/Bmsy ratios.
#'
#' @param mod jjm object that is a list of lists
#' @param bmsy_in A number (or vector of numbers if multiple stocks) to input as Bmsy. If not filled, calculated as the average of the Bmsy estimated for the last ten years (as determined in SCW14 benchmark 2022)
#' @param dyn Logical value to indicate whether a dynamic Bmsy should be calculated
#' @return A model
#' @examples
#' # fixed_bmsy(mod_h1_1.00, bmsy_in=5500) # To input a fixed Bmsy
#' # fixed_bmsy(mod_h1_1.00) # To calculate the Bmsy
#' @export

fixed_bmsy <- function(mod, bmsy_in = NULL, dyn = FALSE) {
  mod_new <- mod

  for(s in 1:mod[[1]]$info$output$nStock) {
    msy_mt <- mod[[1]]$output[[s]]$msy_mt

    if(is.null(bmsy_in)) {
      bmsy <- mean(rev(msy_mt[,10])[1:10])
    } else {
      if(is.matrix(bmsy_in)) {
            bmsy <- bmsy_in[,s]
      } else{
          if(length(bmsy_in) == mod[[1]]$info$output$nStock) {
          bmsy <- bmsy_in[s]
          }
        else(bmsy <- bmsy_in)
      }
    }

    if(dyn) {
      if(is.null(bmsy_in)) {
        df1 <- mod[[1]]$output[[s]]$df1
        rows2use <- grep("SSB_Nofishing", df1[,1])
        dyn_ssbzero <- as.numeric(df1[rows2use, 3])
        dyn_ssbzero <- c(dyn_ssbzero[1], dyn_ssbzero)

        bzero <- msy_mt[1,11]
        dyn_bmsy <- (bmsy / bzero) * dyn_ssbzero

        bmsy <- dyn_bmsy
      } else {
        	if(is.matrix(bmsy_in)) {
            bmsy <- bmsy_in[,s]
          } else{
            bmsy <- bmsy_in
        }
      }
    }

    mod_new[[1]]$output[[s]]$msy_mt <- cbind(msy_mt, oldbmsy = mod[[1]]$output[[s]]$msy_mt[,10])
    old_rat <- (mod[[1]]$output[[s]]$msy_mt[,13])
    new_rat <- (mod[[1]]$output[[s]]$msy_mt[,12]/ bmsy)
    mod_new[[1]]$output[[s]]$msy_mt[,13] <- new_rat
    mod_new[[1]]$output[[s]]$msy_mt[,10] <- bmsy
  }

  return(mod_new)
}
