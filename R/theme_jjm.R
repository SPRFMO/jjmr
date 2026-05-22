#' ggplot2 theme for jjmR
#'
#' @param base_size Base font size.
#' @param ... Additional arguments passed to [ggplot2::theme_bw()].
#'
#' @return a ggplot2 theme object
#' @export
#' 
#' @examples  
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg)) + geom_histogram() + theme_jjm()
#' 
theme_jjm <- function(base_size = 12, ...) {
  ggplot2::theme_bw(base_size = base_size, ...) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "grey92", linewidth = 0.4),
      strip.background = ggplot2::element_rect(fill = "grey95", colour = "grey70"),
      strip.text = ggplot2::element_text(size = ggplot2::rel(0.85), face = "bold"),
      legend.position = "bottom",
      legend.key = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(size = ggplot2::rel(0.9)),
      axis.ticks = ggplot2::element_line(linewidth = 0.4),
      plot.title = ggplot2::element_text(size = ggplot2::rel(1), face = "bold"),
      plot.subtitle = ggplot2::element_text(size = ggplot2::rel(0.85), colour = "grey40")
    )
}
