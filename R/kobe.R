#' @title Kobe plot
#' @description This function create a kobe plot from JJM  model outputs
#'
#' @param obj a jjm model outputs object.
#' @param add boolean, add to an existing kobe plot?
#' @param col color for the lines and points.
#' @param stock Number of the stock chosen for the kobe plot.
#' @param Bref Reference point for B/B_MSY, default=1.
#' @param Fref Reference point for F/F_MSY, default=1.
#' @param Blim Limit reference point for B/B_MSY, default=0.5.
#' @param Flim Limit reference point for F/F_MSY, default=1.5.
#' @param xlim 'x' axis limits.
#' @param ylim 'y' axis limits.
#' @param ... Additional parameters passed to plot.
#'
#' @examples
#' \dontrun{
#' kobe(model)
#' }
#' @export
#'
kobe = function(obj,
                add = FALSE,
                col = "black",
                stock = 1,
                Bref = 1,
                Fref = 1,
                Blim = Bref,
                Flim = Fref,
                xlim = NULL,
                ylim = NULL,
                engine = "ggplot",
                ...) {
  if (engine == "ggplot") {
    
    msy_mt_results <- get_msy_mt(obj)
    
    
    quadrants <-  data.frame(
      panel = c("bottom_left", "top_right",
                "bottom_right", "top_left"),
      fill = c(
        rgb(1, 1, 0, alpha = 0.5),
        rgb(1, 1, 0, alpha = 0.5),
        rgb(0, 1, 0, alpha = 0.5),
        rgb(1, 0, 0, alpha = 0.5)
      ),
      xmin = c(-Inf, Bref, Bref,-Inf),
      xmax = c(Bref, Inf, Inf, Bref),
      ymin = c(-Inf, Fref,-Inf, Fref),
      ymax = c(Fref, Inf, Fref, Inf)
    )
    
    quadrants <-
      tidyr::expand_grid(
        model = unique(msy_mt_results$model),
        stock = unique(msy_mt_results$stock),
        things = quadrants
      ) %>%
      tidyr::unnest(cols = things)
    
    kobe_plot <- msy_mt_results %>%
      ggplot() +
      ggplot2::geom_rect(
        data = quadrants,
        aes(
          xmin = xmin,
          ymin = ymin,
          xmax = xmax,
          ymax = ymax,
          group = fill
        ),
        fill = quadrants$fill
      ) +
      ggplot2::geom_hline(aes(yintercept = Fref), linetype = 2) +
      ggplot2::geom_vline(aes(xintercept = Bref), linetype = 2) +
      ggplot2::geom_path(aes(b_bmsy, f_fmsy), color = "darkgrey") +
      ggplot2::geom_point(aes(b_bmsy, f_fmsy), size = 3, color = col) +
      ggplot2::scale_x_continuous(
        name = bquote(B / B[MSY]),
        breaks = seq(0, max(2, 1.1 * max(
          msy_mt_results$b_bmsy
        )), by = 0.5),
        limits = c(0, NA),
        expand = ggplot2::expansion(mult = c(0, .1))
      ) +
      ggplot2::scale_y_continuous(
        name = bquote(F / F[MSY]),
        breaks = seq(0, max(2, 1.1 * max(
          msy_mt_results$f_fmsy
        )), by = 0.5),
        limits = c(0, NA),
        expand = ggplot2::expansion(mult = c(0, .1))
      ) +
      ggplot2::facet_grid(model ~ stock) +
      theme_jjm()
    
    
    return(kobe_plot)
  } else if (engine == "lattice") {
    for (i in seq_along(obj)) {
      object = obj[[i]]
      
      kobe_plot <- .kobe1(
        x = object,
        stock = stock,
        add = add,
        col = col,
        Bref = Bref,
        Fref = Fref,
        Blim = Bref,
        Flim = Fref,
        xlim = xlim,
        ylim = ylim,
        ...
      )
      
      return(invisible())
    } # close for loop
    
    
  }
  
  
}





.kobe1 = function(x,
                  stock,
                  add,
                  col,
                  Bref,
                  Fref,
                  Blim,
                  Flim,
                  xlim,
                  ylim,
                  ...) {
  #if(class(obj) == "jjm.output") kob = x$output$msy_mt
  #if(class(obj) == "jjm.diag") kob = x$
  
  kob = x$output[[stock]]$msy_mt
  
  F_Fmsy = kob[, 4]
  B_Bmsy = kob[, 13]
  years  = kob[, 1]
  
  n = length(B_Bmsy)
  
  if (!isTRUE(add)) {
    if (is.null(xlim))
      xlim = range(pretty(c(0, B_Bmsy)))
    if (is.null(ylim))
      ylim = range(pretty(c(0, F_Fmsy)))
    
    plot.new()
    plot.window(
      xlim = xlim,
      ylim = ylim,
      xaxs = "i",
      yaxs = "i"
    )
    par(xpd = TRUE)
    
    ylim = par()$usr[3:4]
    zero = ylim[1]
    
    polygon(
      x = c(0, 0, Bref, Bref),
      y = c(Fref, ylim[2], ylim[2], Fref),
      col = rgb(1, 165 / 255, 0, alpha = 0.5),
      border = NA
    )
    polygon(
      x = c(0, 0, Bref, Bref),
      y = c(zero, Fref, Fref, zero),
      col = rgb(1, 1, 0, alpha = 0.5),
      border = NA
    )
    polygon(
      x = c(Bref, Bref, xlim[2], xlim[2]),
      y = c(Fref, ylim[2], ylim[2], Fref),
      col = rgb(1, 1, 0, alpha = 0.5),
      border = NA
    )
    polygon(
      x = c(Bref, Bref, xlim[2], xlim[2]),
      y = c(zero, Fref, Fref, zero),
      col = rgb(0, 1, 0, alpha = 0.5),
      border = NA
    )
    polygon(
      x = c(0, 0, Blim, Blim),
      y = c(Flim, ylim[2], ylim[2], Flim),
      col = rgb(1, 0, 0, alpha = 1),
      border = NA
    )
    
    mtext(toExpress("WTF[msy]"), 2, line = 2.5)
    mtext(toExpress("B/B[msy]"), 1, line = 2.5)
    axis(1, las = 1)
    axis(2, las = 2)
    box()
  }
  
  text(
    B_Bmsy[c(1, n)] + 0.01,
    F_Fmsy[c(1, n)] + 0.1,
    labels = range(years),
    cex = 0.6,
    adj = -0.2,
    col = col
  )
  lines(B_Bmsy, F_Fmsy, type="b", cex=0.5, col=col)
  points(
    B_Bmsy[c(1, n)],
    F_Fmsy[c(1, n)],
    pch = c(15, 17),
    col = col,
    cex = 0.8
  )
  
  return(invisible())
  
}
