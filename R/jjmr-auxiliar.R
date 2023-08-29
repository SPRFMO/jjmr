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

# From lattice package (https://github.com/deepayan/lattice/blob/master/R/settings.R)
# Copyright (C) 2001-2006 Deepayan Sarkar <Deepayan.Sarkar@R-project.org>
lower.saturation <-
    function(x, f = 0.2, space = c("RGB", "HCL"))
{
    ## lower saturation in RGB or HCL space?
    space <- match.arg(tolower(space), c("rgb", "hcl"))
    
    ## for HCL space ideally colorspace::lighten() should be used
    if((space == "hcl") && requireNamespace("colorspace")) {
        return(colorspace::lighten(x, amount = 1 - f))
    }
    
    ## for RGB space the old implementation from lattice is used,
    ## for HCL space (if colorspace is unavailable) an approximation in LUV is used
    RGB <- col2rgb(x)
    if(space == "rgb") {
        RGB[] <- 255 - RGB
        RGB[] <- round(f * RGB)
        RGB[] <- 255 - RGB
    } else {
        ## adjust L coordinate of HCL/LUV only, chroma is left as it is
        LUV <- convertColor(t(RGB), from = "sRGB", to = "Luv", scale.in = 255)
        Lold <- pmin(100, pmax(0, LUV[, "L"]))
        LUV[, "L"] <- 100 - (100 - Lold) * f
        RGB[] <- t(convertColor(LUV, from = "Luv", to = "sRGB", scale.out = 255))
    }
    rgb(RGB["red", ],
        RGB["green", ],
        RGB["blue", ],
        maxColorValue = 255)
}