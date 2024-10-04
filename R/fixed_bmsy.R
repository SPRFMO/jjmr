#' Calculate or input a fixed Bmsy value for the jjm model
#' Updates the msy_mt table in the jjm output with new B/Bmsy ratios.
#'
#' @param mod jjm object that is a list of lists
#' @param refpt A number (or vector of numbers if multiple stocks) to input as Bmsy. If not filled, calculated as the average of the Bmsy estimated for the last ten years (as determined in SCW14 benchmark 2022)
#' @return A model
#' @examples
#' # fixed_bmsy(mod_h1_1.00, refpt=5500) # To input a fixed Bmsy
#' # fixed_bmsy(mod_h1_1.00) # To calculate the Bmsy
#' @export

fixed_bmsy <- function(mod, refpt=NULL){
	for(s in 1:mod[[1]]$info$output$nStock) {

		if(is.null(refpt)) {newrefpt <- mean(rev(mod[[1]]$output[[s]]$msy_mt[,10])[1:10])}
			else{newrefpt <- refpt[s]}
	  	mod[[1]]$output[[s]]$oldbmsy <- mod[[1]]$output[[s]]$msy_mt[,10]
	  	old_rat <- (mod[[1]]$output[[s]]$msy_mt[,13])
	  	new_rat <- (mod[[1]]$output[[s]]$msy_mt[,12]/ newrefpt)
	  	mod[[1]]$output[[s]]$msy_mt[,13] <- new_rat
	  	mod[[1]]$output[[s]]$msy_mt[,10] <- newrefpt
	  }
  return(mod)
}
