% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jjmr-main.R
\name{readJJMConfig}
\alias{readJJMConfig}
\title{Read dat and ctl files from disk to create a jjm.config object.}
\usage{
readJJMConfig(data, control, ...)
}
\arguments{
\item{data}{Path to the data file.}

\item{control}{Path to the control file.}

\item{...}{Additional arguments passed to other functions.}
}
\description{
Store in an R object (of class jjm.config) the dat and ctl files needed
to run a model.
}
\examples{
readJJMConfig(mod1)
}
