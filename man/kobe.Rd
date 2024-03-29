% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kobe.R
\name{kobe}
\alias{kobe}
\title{Kobe plot}
\usage{
kobe(
  obj,
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
  ...
)
}
\arguments{
\item{obj}{a jjm model outputs object.}

\item{add}{boolean, add to an existing kobe plot?}

\item{col}{color for the lines and points.}

\item{stock}{Number of the stock chosen for the kobe plot.}

\item{Bref}{Reference point for B/B_MSY, default=1.}

\item{Fref}{Reference point for F/F_MSY, default=1.}

\item{Blim}{Limit reference point for B/B_MSY, default=0.5.}

\item{Flim}{Limit reference point for F/F_MSY, default=1.5.}

\item{xlim}{'x' axis limits.}

\item{ylim}{'y' axis limits.}

\item{...}{Additional parameters passed to plot.}
}
\description{
This function create a kobe plot from JJM  model outputs
}
\examples{
\dontrun{
kobe(model)
}
}
