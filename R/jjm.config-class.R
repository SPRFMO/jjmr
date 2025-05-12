#' Create a jjm.config object
#'
#' @description
#' Construct a \\code{jjm.config} object from data and control lists.
#'
#' @param data List of raw data elements for the JJM model.
#' @param control List of control parameters, including \code{modelName}.
#' @param ... Unused.
#'
#' @return An object of class \code{jjm.config}.
#' @export
.getJjmConfig <- function(data, control, ...) {
  out <- list()
  model_name <- control$modelName
  out[[model_name]] <- list(
    Dat = data,
    Ctl = control
  )
  class(out) <- "jjm.config"
  return(out)
}

#' Print a jjm.config object
#'
#' @param x A \code{jjm.config} object.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns \code{x}.
#' @export
print.jjm.config <- function(x, ...) {
  cat("JJM configuration for model(s):", paste(names(x), collapse = ", "), "\n")
  invisible(x)
}

#' Summarize a jjm.config object
#'
#' @param object A \code{jjm.config} object.
#' @param ... Additional arguments (ignored).
#'
#' @return An object of class \code{summary.jjm.config} containing model names.
#' @export
summary.jjm.config <- function(object, ...) {
  res <- list(models = names(object))
  class(res) <- "summary.jjm.config"
  return(res)
}

#' Print a summary of jjm.config
#'
#' @param x A \code{summary.jjm.config} object.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns \code{x}.
#' @export
print.summary.jjm.config <- function(x, ...) {
  cat("JJM config summary:\n")
  cat(" Models:", paste(x$models, collapse = ", "), "\n")
  invisible(x)
}

#' Run JJM models (default method)
#'
#' @description
#' Executes one or more JJM model runs, optionally in parallel.
#'
#' @param models Character vector of model filenames.
#' @param path Optional directory containing model files.
#' @param output Directory for model results (created if missing).
#' @param input Directory for input files (default = model directory).
#' @param exec Path to JJM executable.
#' @param version Executable version (ignored if \code{exec} provided).
#' @param useGuess Logical; whether to use starting parameter guesses.
#' @param guess Vector of guess filenames or logical.
#' @param piner Additional runner parameters.
#' @param iprint Printing control for JJM output.
#' @param wait Logical; whether to wait for completion before returning.
#' @param parallel Logical; whether to run models in parallel.
#' @param temp Directory for temporary files (default = \code{tempdir()}).
#' @param ... Additional arguments passed to internal runner.
#'
#' @return Invisibly returns the path to \code{temp} used for input files.
#' @export
runJJM.default <- function(models,
                           path = NULL,
                           output = "results",
                           input = NULL,
                           exec = NULL,
                           version = NULL,
                           useGuess = FALSE,
                           guess = NULL,
                           piner = NULL,
                           iprint = 100,
                           wait = TRUE,
                           parallel = FALSE,
                           temp = NULL,
                           ...) {
  oldwd <- getwd()
  on.exit(setwd(oldwd))
  
  if (!file.exists(output)) {
    dir.create(output, recursive = TRUE)
  }
  output <- normalizePath(output, mustWork = TRUE)
  
  guess <- .checkGuess(models, guess, output)
  if (!is.null(path)) {
    models <- file.path(path, models)
  }
  exec   <- .checkExecutable(exec = exec, version = version)
  models <- .checkModels(models)
  
  base  <- getwd()
  start <- proc.time()
  if (is.null(temp)) temp <- tempdir()
  
  if (!isTRUE(parallel)) {
    cat("\nRunning models", paste(models, collapse = ", "), "\n")
    cat("  Starting at", as.character(Sys.time()), "\n")
    res <- NULL
    for (i in seq_along(models)) {
      setwd(base)
      rtime <- .runJJM(model   = models[i],
                       output  = output,
                       input   = input,
                       exec    = exec,
                       useGuess= useGuess,
                       guess   = guess[i],
                       iprint  = iprint,
                       piner   = piner,
                       wait    = wait,
                       temp    = temp,
                       ...)
      res <- c(res, rtime)
    }
  } else {
    cat("\nRunning models", paste(models, collapse = ", "), "in parallel.\n")
    cat("  Starting at", as.character(Sys.time()), "\n")
    res <- foreach::foreach(i = seq_along(models), .combine = c, .packages = 'jjmR') %dopar% {
      setwd(base)
      .runJJM(model   = models[i],
              output  = output,
              input   = input,
              exec    = exec,
              useGuess= useGuess,
              guess   = guess[i],
              iprint  = iprint,
              piner   = piner,
              wait    = wait,
              temp    = temp,
              ...)
    }
  }
  
  setwd(base)
  cat("  Ended at", as.character(Sys.time()), "\n")
  elapsed <- proc.time() - start
  names(res) <- models
  cat("\nModel runs finished.\nTotal run time:\n")
  print(res)
  cat("\nEffective running time:", round(elapsed[3], 1), "s.\n\n")
  cat("Models were run in temp folder:", temp, "\n")
  return(invisible(temp))
}

#' Run JJM models for jjm.output
#'
#' @inheritParams runJJM.default
#' @export
runJJM.jjm.output <- function(models,
                              path = NULL,
                              output = "results",
                              input = NULL,
                              exec = NULL,
                              version = NULL,
                              useGuess = FALSE,
                              guess = NULL,
                              piner = NULL,
                              iprint = 100,
                              wait = TRUE,
                              parallel = FALSE,
                              temp = NULL,
                              ...) {
  modNames <- tolower(names(models))
  if (is.null(temp)) temp <- tempdir()
  writeJJM(models, path = temp)
  runJJM.default(models   = modNames,
                 path     = temp,
                 output   = output,
                 input    = temp,
                 exec     = exec,
                 version  = version,
                 useGuess = useGuess,
                 guess    = guess,
                 iprint   = iprint,
                 piner    = piner,
                 wait     = wait,
                 parallel = parallel,
                 temp     = temp,
                 ...)
  return(invisible())
}

#' Run JJM models for jjm.config
#'
#' @inheritParams runJJM.default
#' @export
runJJM.jjm.config <- function(models,
                              path = NULL,
                              output = "results",
                              input = NULL,
                              exec = NULL,
                              version = NULL,
                              useGuess = FALSE,
                              guess = NULL,
                              piner = NULL,
                              iprint = 100,
                              wait = TRUE,
                              parallel = FALSE,
                              temp = NULL,
                              ...) {
  modNames <- tolower(names(models))
  if (is.null(temp)) temp <- tempdir()
  for (i in seq_along(models)) {
    .writeJJM(object  = models[[i]]$Dat,
              outFile = models[[i]]$Ctl$dataFile,
              path    = temp)
    .writeJJM(object  = models[[i]]$Ctl,
              outFile = paste0(modNames[i], ".ctl"),
              path    = temp,
              transpose = FALSE)
  }
  runJJM.default(models   = modNames,
                 path     = temp,
                 output   = output,
                 input    = temp,
                 exec     = exec,
                 version  = version,
                 useGuess = useGuess,
                 guess    = guess,
                 iprint   = iprint,
                 piner    = piner,
                 wait     = wait,
                 parallel = parallel,
                 temp     = temp,
                 ...)
  return(invisible())
}



