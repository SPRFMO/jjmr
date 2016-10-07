# auxiliary functions

makeTransparent = function(..., alpha=0.5) {
  
  if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
  
  alpha = floor(255*alpha) 
  newColor = col2rgb(col=unlist(list(...)), alpha=FALSE)
  
  .makeTransparent = function(col, alpha) {
    rgb(red = col[1], green = col[2], blue = col[3], alpha = alpha, maxColorValue = 255)
  }
  
  newColor = apply(newColor, 2, .makeTransparent, alpha=alpha)
  
  return(newColor)
}


.my.panel.bands = function(x, y, upper, lower, fill, col,
                            subscripts, ..., font, fontface)
{
  upper = upper[subscripts]
  lower = lower[subscripts]
  panel.polygon(c(x, rev(x)), c(upper, rev(lower)),
                col = makeTransparent(fill), border = FALSE,
                ...)
}

.splitPor = function(obj){
	out = strsplit(obj, "%")[[1]]
	return(out)
}