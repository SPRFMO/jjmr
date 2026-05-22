# Annex plotting helpers -----------------------------------------------------

PFA_BLUE <- "#004868"
PFA_ORANGE <- "#FF800A"
PFA_GREEN <- "#00B764"
PFA_BLUE2 <- "#00BADC"
PFA_ORANGE2 <- "#F2A44A"

.PEEL_PAL <- c(PFA_ORANGE, PFA_GREEN, PFA_ORANGE2, PFA_BLUE2, "#808080")
FLEET_PAL <- c(
  PFA_BLUE, PFA_BLUE2, PFA_ORANGE, PFA_GREEN, "#808080", "#666666",
  PFA_ORANGE2, "#6B4C9A"
)
SURV_PAL <- c(
  PFA_ORANGE, PFA_BLUE, PFA_GREEN, PFA_BLUE2, "#808080", "#666666",
  PFA_ORANGE2, "#6B4C9A"
)

.annex_palette <- function(pal, n) {
  if (n <= length(pal)) {
    pal[seq_len(n)]
  } else {
    rep(pal, length.out = n)
  }
}

.annex_default <- function(x, default) {
  if (is.null(x)) default else x
}

.empty_annex_plot <- function(title = NULL, subtitle = NULL) {
  ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::labs(title = title, subtitle = subtitle)
}

#' JJM annex diagnostic palettes
#'
#' @param type Palette to return: `"pfa"`, `"fleet"`, `"survey"`, or `"peel"`.
#'
#' @return A named character vector of hex colours.
#' @export
jjm_pfa_palette <- function(type = c("pfa", "fleet", "survey", "peel")) {
  type <- match.arg(type)
  switch(
    type,
    pfa = c(
      blue = PFA_BLUE,
      orange = PFA_ORANGE,
      green = PFA_GREEN,
      cyan = PFA_BLUE2,
      amber = PFA_ORANGE2
    ),
    fleet = FLEET_PAL,
    survey = SURV_PAL,
    peel = .PEEL_PAL
  )
}

#' Build a publication label from a JJM model name
#'
#' @param nm Model name, such as `"h1_1.14"` or `"h2_1.14"`.
#' @param stock_name Optional stock label to append.
#'
#' @return A character label.
#' @export
jjm_label <- function(nm, stock_name = NULL) {
  hyp <- if (startsWith(nm, "h1")) "Single-stock" else "Two-stock"
  ver <- sub("^h[12]_", "", nm)
  lbl <- paste0(hyp, " - ", ver)
  if (!is.null(stock_name)) {
    lbl <- paste0(lbl, " - ", stock_name)
  }
  lbl
}

.jjm_out <- function(mod, stock = 1) {
  mod[[1]]$output[[stock]]
}

.fnames <- function(out) {
  n <- out$Fshry_names
  if (is.matrix(n)) n[, 1] else as.character(n)
}

.snames <- function(out) {
  n <- out$Index_names
  if (is.matrix(n)) n[, 1] else as.character(n)
}

.fleet_name_from_number <- function(names_vec, number, observed_numbers) {
  if (length(names_vec) >= max(observed_numbers, na.rm = TRUE)) {
    names_vec[number]
  } else {
    names_vec[match(number, observed_numbers)]
  }
}

#' Extract observed catch by fleet
#'
#' @param mod A `jjm.output` object from [readJJM()].
#' @param stock Stock index.
#'
#' @return A tibble with `year`, `catch`, and `fleet`.
#' @export
extr_catch <- function(mod, stock = 1) {
  out <- .jjm_out(mod, stock)
  fn <- .fnames(out)
  idx <- grep("^Obs_catch_", names(out))

  purrr::imap_dfr(idx, function(ci, i) {
    tibble::tibble(year = out$Yr, catch = out[[ci]], fleet = fn[i])
  }) |>
    dplyr::mutate(fleet = factor(fleet, levels = fn))
}

.extract_wtat <- function(mod, stock, prefix, name_col) {
  out <- .jjm_out(mod, stock)
  nms <- if (name_col == "fleet") .fnames(out) else .snames(out)
  cols <- grep(paste0("^", prefix), names(out), value = TRUE)

  purrr::imap_dfr(cols, function(cn, i) {
    mat <- as.matrix(out[[cn]])
    vals <- mat[, -1, drop = FALSE]
    n_age <- ncol(vals)
    df <- as.data.frame(vals) |>
      stats::setNames(paste0("a", seq_len(n_age))) |>
      dplyr::mutate(year = mat[, 1])
    df[[name_col]] <- nms[i]
    tidyr::pivot_longer(
      df,
      dplyr::starts_with("a"),
      names_to = "age",
      values_to = "weight",
      names_transform = list(age = function(x) as.integer(sub("a", "", x)))
    )
  })
}

#' Extract fishery weight-at-age data
#'
#' @inheritParams extr_catch
#' @return A tibble with `year`, `fleet`, `age`, and `weight`.
#' @export
extr_wtat_fsh <- function(mod, stock = 1) {
  .extract_wtat(mod, stock, "wt_fsh_", "fleet")
}

#' Extract survey weight-at-age data
#'
#' @inheritParams extr_catch
#' @return A tibble with `year`, `survey`, `age`, and `weight`.
#' @export
extr_wtat_srv <- function(mod, stock = 1) {
  .extract_wtat(mod, stock, "wt_ind_", "survey")
}

.composition_long <- function(mat, type, value_name = "prop", x_prefix = "a") {
  mat <- as.matrix(mat)
  vals <- mat[, -1, drop = FALSE]
  n_x <- ncol(vals)
  as.data.frame(vals) |>
    stats::setNames(paste0(x_prefix, seq_len(n_x))) |>
    dplyr::mutate(year = mat[, 1], type = type) |>
    tidyr::pivot_longer(
      dplyr::starts_with(x_prefix),
      names_to = value_name,
      values_to = "prop",
      names_transform = stats::setNames(
        list(function(x) as.integer(sub(x_prefix, "", x))),
        value_name
      )
    )
}

#' Extract fishery age-composition fits
#'
#' @inheritParams extr_catch
#' @return A tibble with observed and predicted fishery age compositions.
#' @export
extr_agefits_fsh <- function(mod, stock = 1) {
  out <- .jjm_out(mod, stock)
  fn <- .fnames(out)
  obs_cols <- grep("^pobs_fsh_", names(out), value = TRUE)
  obs_nums <- sort(as.integer(sub("^pobs_fsh_", "", obs_cols)))

  purrr::imap_dfr(obs_cols, function(cn, i) {
    fleet_num <- as.integer(sub("^pobs_fsh_", "", cn))
    hat_cn <- paste0("phat_fsh_", fleet_num)
    if (is.null(out[[hat_cn]])) return(NULL)
    fleet_name <- .fleet_name_from_number(fn, fleet_num, obs_nums)

    dplyr::bind_rows(
      .composition_long(out[[cn]], "observed", "age", "a"),
      .composition_long(out[[hat_cn]], "predicted", "age", "a")
    ) |>
      dplyr::mutate(fleet = fleet_name)
  })
}

#' Extract fishery length-composition fits
#'
#' @inheritParams extr_catch
#' @return A tibble with observed and predicted fishery length compositions.
#' @export
extr_lenfits_fsh <- function(mod, stock = 1) {
  out <- .jjm_out(mod, stock)
  fn <- .fnames(out)
  obs_cols <- grep("^pobs_len_fsh_", names(out), value = TRUE)
  empty <- tibble::tibble(
    year = integer(),
    length_bin = integer(),
    prop = numeric(),
    type = character(),
    fleet = character()
  )
  if (!length(obs_cols)) return(empty)

  obs_nums <- sort(as.integer(sub("^pobs_len_fsh_", "", obs_cols)))
  purrr::imap_dfr(obs_cols, function(cn, i) {
    fleet_num <- as.integer(sub("^pobs_len_fsh_", "", cn))
    hat_cn <- paste0("phat_len_fsh_", fleet_num)
    if (is.null(out[[hat_cn]])) return(NULL)
    fleet_name <- .fleet_name_from_number(fn, fleet_num, obs_nums)

    dplyr::bind_rows(
      .composition_long(out[[cn]], "observed", "length_bin", "l"),
      .composition_long(out[[hat_cn]], "predicted", "length_bin", "l")
    ) |>
      dplyr::mutate(fleet = fleet_name)
  })
}

#' Extract survey age-composition fits
#'
#' @inheritParams extr_catch
#' @return A tibble with observed and predicted survey age compositions.
#' @export
extr_agefits_srv <- function(mod, stock = 1) {
  out <- .jjm_out(mod, stock)
  sn <- .snames(out)
  obs_cols <- grep("^pobs_ind_", names(out), value = TRUE)
  obs_nums <- sort(as.integer(sub("^pobs_ind_", "", obs_cols)))

  purrr::imap_dfr(obs_cols, function(cn, i) {
    survey_num <- as.integer(sub("^pobs_ind_", "", cn))
    hat_cn <- paste0("phat_ind_", survey_num)
    if (is.null(out[[hat_cn]])) return(NULL)
    survey_name <- .fleet_name_from_number(sn, survey_num, obs_nums)

    dplyr::bind_rows(
      .composition_long(out[[cn]], "observed", "age", "a"),
      .composition_long(out[[hat_cn]], "predicted", "age", "a")
    ) |>
      dplyr::mutate(survey = survey_name)
  })
}

#' Extract standardised survey index fits
#'
#' @inheritParams extr_catch
#' @return A tibble with standardised observed and modelled index values.
#' @export
extr_indices <- function(mod, stock = 1) {
  out <- .jjm_out(mod, stock)
  sn <- .snames(out)
  obs_cols <- grep("^Obs_Survey_", names(out), value = TRUE)

  purrr::imap_dfr(obs_cols, function(cn, i) {
    if (i > length(sn) || is.na(sn[i])) return(NULL)
    df <- as.data.frame(out[[cn]])
    if (ncol(df) < 4) return(NULL)
    colnames(df)[1:4] <- c("year", "obs", "model", "sd")
    if (!any(df$obs > 0, na.rm = TRUE)) return(NULL)
    sc <- max(c(df$obs, df$model, df$sd), na.rm = TRUE)
    if (!is.finite(sc) || sc <= 0) return(NULL)
    df |>
      dplyr::mutate(
        obs = obs / sc,
        model = model / sc,
        sd = sd / sc,
        obs = dplyr::if_else(obs <= 0, NA_real_, obs),
        survey = sn[i]
      ) |>
      dplyr::filter(!is.na(model), model > 0) |>
      dplyr::select(year, obs, model, sd, survey)
  })
}

.extract_meanstat <- function(mod, stock, prefix, name_col) {
  out <- .jjm_out(mod, stock)
  nms <- if (name_col == "fleet") .fnames(out) else .snames(out)
  cols <- grep(paste0("^", prefix), names(out), value = TRUE)

  purrr::imap_dfr(cols, function(cn, i) {
    df <- as.data.frame(out[[cn]])
    if (nrow(df) <= 1 || ncol(df) < 8) return(NULL)
    out_df <- df[, c(1, 4, 5, 7, 8)] |>
      stats::setNames(c("year", "obs", "model", "lower", "upper")) |>
      as.data.frame()
    out_df[[name_col]] <- nms[i]
    out_df
  })
}

#' Extract fishery mean age fits
#'
#' @inheritParams extr_catch
#' @return A tibble with fishery mean-age observations and fits.
#' @export
extr_meanage_fsh <- function(mod, stock = 1) {
  .extract_meanstat(mod, stock, "EffN_Fsh_", "fleet")
}

#' Extract survey mean age fits
#'
#' @inheritParams extr_catch
#' @return A tibble with survey mean-age observations and fits.
#' @export
extr_meanage_srv <- function(mod, stock = 1) {
  .extract_meanstat(mod, stock, "EffN_Survey_", "survey")
}

#' Extract fishery mean length fits
#'
#' @inheritParams extr_catch
#' @return A tibble with fishery mean-length observations and fits.
#' @export
extr_meanlen_fsh <- function(mod, stock = 1) {
  empty <- tibble::tibble(
    year = integer(), obs = numeric(), model = numeric(), lower = numeric(),
    upper = numeric(), fleet = character()
  )
  out <- .jjm_out(mod, stock)
  if (!length(grep("^EffN_Length_Fsh_", names(out), value = TRUE))) return(empty)
  .extract_meanstat(mod, stock, "EffN_Length_Fsh_", "fleet")
}

.mat_long <- function(mat, names_vec, type_label, value_col) {
  if (is.null(mat)) return(NULL)
  years <- suppressWarnings(as.integer(rownames(mat)))
  if (all(is.na(years))) {
    years <- seq_len(nrow(mat))
  }
  purrr::imap_dfr(seq_len(min(ncol(mat), length(names_vec))), function(j, ...) {
    out <- tibble::tibble(year = years, name = names_vec[j], type = type_label)
    out[[value_col]] <- mat[, j]
    out
  })
}

#' Extract input composition sample sizes
#'
#' @inheritParams extr_catch
#' @return A tibble with composition input sample sizes.
#' @export
extr_effn <- function(mod, stock = 1) {
  dat <- mod[[1]]$data
  out <- .jjm_out(mod, stock)
  fn <- .fnames(out)
  sn <- .snames(out)

  dplyr::bind_rows(
    .mat_long(dat$Fagesample, fn, "Fishery (age)", "input_n"),
    .mat_long(dat$Flengthsample, fn, "Fishery (length)", "input_n"),
    .mat_long(dat$Iagesample, sn, "Survey (age)", "input_n")
  ) |>
    dplyr::filter(!is.na(input_n), input_n > 0)
}

#' Extract catch observation CVs
#'
#' @inheritParams extr_catch
#' @return A tibble with catch CVs by fleet and year.
#' @export
extr_catch_cv <- function(mod, stock = 1) {
  dat <- mod[[1]]$data
  out <- .jjm_out(mod, stock)
  fn <- .fnames(out)
  if (is.null(dat$Fcatonerr)) {
    return(tibble::tibble(year = integer(), cv = numeric(), fleet = character()))
  }
  .mat_long(dat$Fcatonerr, fn, "catch", "cv") |>
    dplyr::select(year, cv, fleet = name) |>
    dplyr::filter(!is.na(cv), cv > 0)
}

#' Extract fished and unfished total biomass
#'
#' @inheritParams extr_catch
#' @return A tibble with total biomass by scenario.
#' @export
extr_fished_unfished <- function(mod, stock = 1) {
  out <- .jjm_out(mod, stock)
  dplyr::bind_rows(
    data.frame(
      year = out$TotBiom[, 1],
      bio = out$TotBiom[, 2],
      scenario = "Fished"
    ),
    data.frame(
      year = out$TotBiom_NoFish[, 1],
      bio = out$TotBiom_NoFish[, 2],
      scenario = "Unfished"
    )
  )
}

#' Extract retrospective data
#'
#' @param retro A retrospective object loaded from a JJM retrospective RData file.
#' @param var Variable to extract. `"SSB"` and `"R"` use direct retrospective
#'   entries. `"F"` averages the first column of all `F_fsh_*` entries.
#' @param stock Stock index.
#'
#' @return A tibble with `year`, `est`, optional confidence limits, and `peel`.
#' @export
extr_retro <- function(retro, var = "SSB", stock = 1) {
  stk <- retro[[paste0("Stock_", stock)]]
  if (is.null(stk)) {
    warning("No retrospective data for stock ", stock)
    return(tibble::tibble(
      year = integer(), est = numeric(), lower = numeric(),
      upper = numeric(), peel = integer()
    ))
  }

  if (var == "F") {
    fkeys <- grep("^F_fsh_", names(stk), value = TRUE)
    if (!length(fkeys)) {
      warning("No F_fsh_* keys found in retro for stock ", stock)
      return(tibble::tibble(
        year = integer(), est = numeric(), lower = numeric(),
        upper = numeric(), peel = integer()
      ))
    }
    ref <- stk[[fkeys[1]]]
    time <- ref$time
    n_peel <- dim(ref$var)[3]
    return(
      purrr::map_dfr(seq_len(n_peel), function(i) {
        est_mat <- sapply(fkeys, function(k) stk[[k]]$var[, 1, i])
        tibble::tibble(
          year = time,
          est = rowMeans(est_mat),
          lower = NA_real_,
          upper = NA_real_,
          peel = i - 1L
        )
      }) |>
        dplyr::filter(!is.na(est))
    )
  }

  dat <- stk[[var]]
  if (is.null(dat)) {
    warning("No retrospective data for var '", var, "' in stock ", stock)
    return(tibble::tibble(
      year = integer(), est = numeric(), lower = numeric(),
      upper = numeric(), peel = integer()
    ))
  }
  v <- dat$var
  d <- dim(v)
  if (!is.null(d) && length(d) == 3L) {
    n_run <- d[[3]]
    has_ci <- d[[2]] >= 4L
    purrr::map_dfr(seq_len(n_run), function(i) {
      tibble::tibble(
        year = dat$time,
        est = v[, 1, i],
        lower = if (has_ci) v[, 3, i] else NA_real_,
        upper = if (has_ci) v[, 4, i] else NA_real_,
        peel = i - 1L
      )
    })
  } else if (is.list(v)) {
    purrr::imap_dfr(v, function(mat, i) {
      has_ci <- ncol(mat) >= 4L
      tibble::tibble(
        year = dat$time,
        est = mat[, 1],
        lower = if (has_ci) mat[, 3] else NA_real_,
        upper = if (has_ci) mat[, 4] else NA_real_,
        peel = i - 1L
      )
    })
  } else {
    if (is.null(d)) v <- as.matrix(v)
    has_ci <- ncol(v) >= 4L
    tibble::tibble(
      year = dat$time,
      est = v[, 1],
      lower = if (has_ci) v[, 3] else NA_real_,
      upper = if (has_ci) v[, 4] else NA_real_,
      peel = 0L
    )
  } |>
    dplyr::filter(!is.na(est))
}

#' Extract q break years
#'
#' @param mod A `jjm.output` object from [readJJM()].
#'
#' @return A tibble with survey names and q break years.
#' @export
extr_q_breaks <- function(mod) {
  ctl <- mod[[1]]$control
  phases <- ctl$RW_q_phases
  nyrs <- ctl$RW_nyrs_q
  yrs <- ctl$RW_q_yrs
  snames <- .snames(.jjm_out(mod, 1))
  if (is.null(phases) || is.null(nyrs)) {
    return(tibble::tibble(survey = character(), break_year = integer()))
  }

  pos <- 1L
  purrr::imap_dfr(seq_along(phases), function(i, ...) {
    n <- nyrs[i]
    if (n == 0L) return(NULL)
    bk <- yrs[pos:(pos + n - 1L)]
    pos <<- pos + n
    tibble::tibble(survey = snames[i], break_year = as.integer(bk))
  })
}

#' Extract index CVs
#'
#' @inheritParams extr_catch
#' @return A tibble with survey index CVs.
#' @export
extr_cv <- function(mod, stock = 1) {
  out <- .jjm_out(mod, stock)
  sn <- .snames(out)
  idx_c <- grep("^Obs_Survey_", names(out))
  err_c <- grep("^Indexerr_", names(out))
  empty <- tibble::tibble(year = numeric(), cv = numeric(), survey = character())
  if (!length(err_c)) return(empty)

  purrr::imap_dfr(seq_along(idx_c), function(i, ...) {
    if (i > length(err_c)) return(NULL)
    obs_m <- out[[idx_c[i]]]
    err_m <- out[[err_c[i]]]
    if (is.null(obs_m) || is.null(err_m)) return(NULL)
    obs_m <- as.matrix(obs_m)
    err_m <- as.matrix(err_m)
    keep <- obs_m[, 2] > 0
    tibble::tibble(
      year = obs_m[keep, 1],
      cv = err_m[keep, 2] / obs_m[keep, 2],
      survey = sn[i]
    )
  })
}

.extract_sel <- function(mod, stock, prefix, name_col) {
  out <- .jjm_out(mod, stock)
  nms <- if (name_col == "fleet") .fnames(out) else .snames(out)
  cols <- grep(paste0("^", prefix), names(out), value = TRUE)

  purrr::imap_dfr(cols, function(cn, i) {
    mat <- as.matrix(out[[cn]])
    vals <- mat[, -c(1, 2), drop = FALSE]
    df <- as.data.frame(vals) |>
      stats::setNames(paste0("a", seq_len(ncol(vals)))) |>
      dplyr::mutate(year = mat[, 2])
    df[[name_col]] <- nms[min(i, length(nms))]
    tidyr::pivot_longer(
      df,
      dplyr::starts_with("a"),
      names_to = "age",
      values_to = "sel",
      names_transform = list(age = function(x) as.integer(sub("a", "", x)))
    )
  })
}

#' Extract fishery selectivity-at-age
#'
#' @inheritParams extr_catch
#' @return A tibble with fishery selectivity-at-age.
#' @export
extr_sel_fsh <- function(mod, stock = 1) {
  .extract_sel(mod, stock, "sel_fsh_", "fleet")
}

#' Extract survey selectivity-at-age
#'
#' @inheritParams extr_catch
#' @return A tibble with survey selectivity-at-age.
#' @export
extr_sel_srv <- function(mod, stock = 1) {
  .extract_sel(mod, stock, "sel_ind_", "survey")
}

#' Extract selectivity block break years
#'
#' @param mod A `jjm.output` object from [readJJM()].
#' @param type `"fsh"` for fisheries or `"srv"` for surveys.
#'
#' @return A tibble of break years.
#' @export
extr_sel_breaks <- function(mod, type = c("fsh", "srv")) {
  type <- match.arg(type)
  ctl <- mod[[1]]$control
  out <- .jjm_out(mod, 1)
  nms <- if (type == "fsh") .fnames(out) else .snames(out)
  prefix <- if (type == "fsh") "F" else "I"
  col_name <- if (type == "fsh") "fleet" else "survey"

  purrr::imap_dfr(seq_along(nms), function(i, ...) {
    yrs <- ctl[[paste0(prefix, i, "_selchangeYear")]]
    chs <- ctl[[paste0(prefix, i, "_selchange")]]
    if (is.null(yrs) || !length(yrs)) return(NULL)
    if (any(abs(chs - round(chs)) > 1e-9, na.rm = TRUE)) return(NULL)
    out <- tibble::tibble(break_year = as.integer(yrs))
    out[[col_name]] <- nms[i]
    out
  })
}

#' Check whether composition data are available
#'
#' @param df Composition fit tibble.
#' @param grp_var Optional grouping variable name.
#' @param grp_filter Optional group value to filter.
#'
#' @return `TRUE` when observed composition rows exist.
#' @export
has_comp <- function(df, grp_var = NULL, grp_filter = NULL) {
  if (nrow(df) == 0 || !"type" %in% names(df)) return(FALSE)
  if (!is.null(grp_filter) && !is.null(grp_var)) {
    df <- df[df[[grp_var]] == grp_filter, ]
  }
  nrow(df[df$type == "observed" & !is.na(df$prop), ]) > 0
}

#' Calculate figure height for composition fits
#'
#' @param df Composition fit tibble.
#' @param grp_var Optional grouping variable name.
#' @param grp_filter Optional group value to filter.
#' @param ncol Number of panels per row.
#' @param panel_h Height per row of year panels.
#' @param row2_h Height for the bubble residual row.
#' @param row3_h Height for the marginal residual row.
#'
#' @return Numeric figure height.
#' @export
comp_fig_height <- function(df, grp_var = NULL, grp_filter = NULL,
                            ncol = 5, panel_h = 2.2,
                            row2_h = 2.5, row3_h = 2.2) {
  if (!is.null(grp_filter) && !is.null(grp_var)) {
    df <- df[df[[grp_var]] == grp_filter, ]
  }
  n_yr <- length(unique(df$year[df$type == "observed" & !is.na(df$prop)]))
  if (n_yr == 0L) return(4)
  ceiling(n_yr / ncol) * panel_h + row2_h + row3_h
}

#' Calculate figure height for faceted plots
#'
#' @param n_panels Number of panels.
#' @param ncol Number of panels per row.
#' @param panel_h Height per panel row.
#' @param min_h Minimum height.
#'
#' @return Numeric figure height.
#' @export
facet_fig_height <- function(n_panels, ncol = 2, panel_h = 3.5, min_h = 4) {
  max(min_h, ceiling(n_panels / ncol) * panel_h)
}

#' Plot catch by fleet
#'
#' @inheritParams extr_catch
#' @param title Optional plot title.
#'
#' @return A ggplot or patchwork object.
#' @export
plot_catch_fleet <- function(mod, stock = 1, title = NULL) {
  df <- extr_catch(mod, stock)
  nf <- nlevels(df$fleet)
  pal <- stats::setNames(.annex_palette(FLEET_PAL, nf), levels(df$fleet))

  p_catch <- ggplot2::ggplot(df, ggplot2::aes(year, catch / 1e3, fill = fleet)) +
    ggplot2::geom_area(
      position = "stack", colour = "white", linewidth = 0.25, alpha = 0.9
    ) +
    ggplot2::scale_fill_manual(values = pal, name = NULL) +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::labs(x = NULL, y = "Catch (thousand mt)", title = title) +
    theme_jjm() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )

  df_cv <- extr_catch_cv(mod, stock)
  if (nrow(df_cv) == 0) {
    return(p_catch + ggplot2::theme(axis.text.x = ggplot2::element_text()))
  }

  p_cv <- ggplot2::ggplot(df_cv, ggplot2::aes(year, cv, colour = fleet)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::scale_colour_manual(values = pal, name = NULL) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
    ggplot2::labs(x = NULL, y = "Catch CV") +
    theme_jjm() +
    ggplot2::theme(legend.position = "none")

  p_catch / p_cv + patchwork::plot_layout(heights = c(3, 1))
}

#' Calculate Mohn's rho
#'
#' @param df Retrospective tibble from [extr_retro()].
#' @param n_peel Number of peels to include.
#'
#' @return Numeric Mohn's rho.
#' @export
calc_mohn_rho <- function(df, n_peel = 5) {
  if (nrow(df) == 0 || !any(df$peel == 0L)) return(NA_real_)
  max_yr <- max(df$year[df$peel == 0L])
  peels <- seq_len(min(n_peel, max(df$peel)))
  if (!length(peels)) return(NA_real_)
  rhos <- purrr::map_dbl(peels, function(p) {
    yr <- max_yr - p
    est_peel <- df$est[df$peel == p & df$year == yr]
    est_base <- df$est[df$peel == 0L & df$year == yr]
    if (!length(est_peel) || !length(est_base) || est_base == 0) return(NA_real_)
    (est_peel - est_base) / est_base
  })
  mean(rhos, na.rm = TRUE)
}

#' Plot retrospective peels
#'
#' @inheritParams extr_retro
#' @param title Optional plot title.
#'
#' @return A ggplot object.
#' @export
plot_retro <- function(retro, var = "SSB", stock = 1, title = NULL) {
  df <- extr_retro(retro, var, stock)
  if (nrow(df) == 0L) {
    return(.empty_annex_plot(
      title = paste("No retrospective data for stock", stock)
    ))
  }

  n_peel <- max(df$peel)
  max_yr <- max(df$year)
  term_yrs <- stats::setNames(
    as.character(max_yr - seq_len(n_peel)),
    as.character(seq_len(n_peel))
  )
  lbl_map <- c("0" = as.character(max_yr), term_yrs)
  pal <- c(
    "0" = PFA_BLUE,
    stats::setNames(.annex_palette(.PEEL_PAL, n_peel), as.character(seq_len(n_peel)))
  )
  scale_factor <- switch(var, SSB = 1e3, R = 1e6, F = 1, 1)
  ylab <- switch(
    var,
    SSB = "SSB (thousand mt)",
    R = "Recruitment (millions)",
    F = "Fishing mortality",
    var
  )
  rho <- calc_mohn_rho(df, n_peel = 5)
  rho_lbl <- if (is.na(rho)) NULL else paste0("Mohn's rho (5-yr) = ", sprintf("%+.3f", rho))
  df_ci <- dplyr::filter(df, peel == 0L, !is.na(lower), !is.na(upper))

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(year, est / scale_factor, group = peel, colour = factor(peel))
  )

  if (nrow(df_ci) > 0) {
    p <- p + ggplot2::geom_ribbon(
      data = df_ci,
      ggplot2::aes(
        x = year,
        ymin = lower / scale_factor,
        ymax = upper / scale_factor,
        fill = factor(peel)
      ),
      alpha = 0.18,
      colour = NA,
      inherit.aes = FALSE
    )
  }

  p +
    ggplot2::geom_line(ggplot2::aes(linewidth = peel == 0)) +
    ggplot2::scale_colour_manual(values = pal, labels = lbl_map, name = "Terminal year") +
    ggplot2::scale_fill_manual(values = pal, guide = "none") +
    ggplot2::scale_linewidth_manual(
      values = c("TRUE" = 1.6, "FALSE" = 0.9),
      guide = "none"
    ) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(5)) +
    ggplot2::labs(x = NULL, y = ylab, title = title, subtitle = rho_lbl) +
    theme_jjm()
}

#' Plot weight-at-age over time
#'
#' @param df Data from [extr_wtat_fsh()] or [extr_wtat_srv()].
#' @param facet_var Facet variable, usually `"fleet"` or `"survey"`.
#' @param title Optional plot title.
#' @param years Optional vector of years to plot.
#'
#' @return A ggplot object.
#' @export
plot_wtat <- function(df, facet_var, title = NULL, years = NULL) {
  if (is.null(years)) years <- seq(max(df$year, na.rm = TRUE) - 14, max(df$year, na.rm = TRUE))
  df <- dplyr::filter(df, year %in% years)
  ggplot2::ggplot(df, ggplot2::aes(year, weight, group = age, colour = age)) +
    ggplot2::geom_line(linewidth = 0.9, alpha = 0.85) +
    ggplot2::scale_colour_gradient(
      low = PFA_BLUE,
      high = PFA_ORANGE,
      name = "Age",
      breaks = scales::breaks_width(2),
      guide = ggplot2::guide_colourbar(barwidth = 16, barheight = 0.7)
    ) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(5)) +
    ggplot2::facet_wrap(stats::as.formula(paste("~", facet_var)), ncol = 2) +
    ggplot2::labs(x = NULL, y = "Weight (kg)", title = title) +
    theme_jjm()
}

#' Plot weight-at-age by cohort
#'
#' @inheritParams plot_wtat
#' @return A ggplot object.
#' @export
plot_wtat_cohort <- function(df, facet_var, title = NULL, years = NULL) {
  if (is.null(years)) years <- seq(max(df$year, na.rm = TRUE) - 14, max(df$year, na.rm = TRUE))
  df <- df |>
    dplyr::filter(year %in% years) |>
    dplyr::mutate(cohort = year - age) |>
    dplyr::filter(!is.na(weight))

  ggplot2::ggplot(df, ggplot2::aes(year, weight, group = cohort, colour = cohort)) +
    ggplot2::geom_line(linewidth = 0.9, alpha = 0.85) +
    ggplot2::scale_colour_gradient(
      low = PFA_BLUE,
      high = PFA_ORANGE,
      name = "Cohort",
      breaks = scales::breaks_width(10),
      guide = ggplot2::guide_colourbar(barwidth = 16, barheight = 0.7)
    ) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(5)) +
    ggplot2::facet_wrap(stats::as.formula(paste("~", facet_var)), ncol = 2) +
    ggplot2::labs(x = NULL, y = "Weight (kg)", title = title) +
    theme_jjm()
}

#' Plot composition fits and residuals
#'
#' @param df Composition fit data from an `extr_*fits_*()` function.
#' @param x_var X variable, usually `"age"` or `"length_bin"`.
#' @param grp_var Grouping variable, usually `"fleet"` or `"survey"`.
#' @param grp_filter Optional group value.
#' @param x_lab X-axis label.
#' @param title Optional plot title.
#'
#' @return A patchwork object.
#' @export
plot_comp_fits <- function(df, x_var, grp_var, grp_filter = NULL,
                           x_lab = "Age", title = NULL) {
  if (nrow(df) == 0 || !"type" %in% names(df)) {
    return(.empty_annex_plot(title = title, subtitle = "No composition data available"))
  }
  if (!is.null(grp_filter)) {
    df <- df[df[[grp_var]] == grp_filter, ]
  }

  obs <- dplyr::filter(df, type == "observed", !is.na(prop))
  pred <- dplyr::filter(df, type == "predicted", !is.na(prop))
  if (nrow(obs) == 0) {
    return(.empty_annex_plot(title = title, subtitle = "No composition data available"))
  }

  p_fit <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = obs,
      ggplot2::aes(.data[[x_var]], prop),
      shape = 19,
      colour = PFA_BLUE,
      size = 1.1
    ) +
    ggplot2::geom_line(
      data = pred,
      ggplot2::aes(.data[[x_var]], prop),
      colour = PFA_ORANGE,
      linewidth = 0.8
    ) +
    ggplot2::facet_wrap(~year, scales = "fixed") +
    ggplot2::labs(x = x_lab, y = "Proportion", title = title) +
    theme_jjm() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(size = 6),
      axis.text = ggplot2::element_text(size = 6),
      panel.spacing = grid::unit(0.15, "lines")
    )

  dw <- dplyr::left_join(
    obs |>
      dplyr::select(year, dplyr::all_of(x_var), obs_prop = prop),
    pred |>
      dplyr::select(year, dplyr::all_of(x_var), pred_prop = prop),
    by = c("year", x_var)
  ) |>
    dplyr::mutate(
      resid = obs_prop - pred_prop,
      sign = dplyr::if_else(resid >= 0, "Positive", "Negative")
    )

  p_resid <- ggplot2::ggplot(
    dw,
    ggplot2::aes(year, .data[[x_var]], size = abs(resid), colour = sign)
  ) +
    ggplot2::geom_point(alpha = 0.75) +
    ggplot2::scale_colour_manual(
      values = c(Positive = PFA_ORANGE, Negative = PFA_BLUE),
      name = NULL
    ) +
    ggplot2::scale_size_area(
      max_size = 6,
      name = "|residual|",
      guide = ggplot2::guide_legend(override.aes = list(colour = "grey50"))
    ) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(5)) +
    ggplot2::scale_y_continuous(breaks = scales::breaks_width(2)) +
    ggplot2::labs(x = NULL, y = x_lab) +
    theme_jjm()

  p_age <- ggplot2::ggplot(dw, ggplot2::aes(factor(.data[[x_var]]), resid)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40", linetype = "dashed") +
    ggplot2::geom_boxplot(fill = PFA_BLUE, alpha = 0.7, outlier.size = 0.8) +
    ggplot2::labs(x = x_lab, y = "obs - pred") +
    theme_jjm()

  p_yr <- ggplot2::ggplot(dw, ggplot2::aes(factor(year), resid)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40", linetype = "dashed") +
    ggplot2::geom_boxplot(fill = PFA_ORANGE, alpha = 0.7, outlier.size = 0.8) +
    ggplot2::labs(x = "Year", y = NULL) +
    theme_jjm() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, size = 6))

  p_fit / p_resid / (p_age | p_yr) + patchwork::plot_layout(heights = c(4, 1.5, 1.5))
}

#' Plot index fits with input CVs
#'
#' @inheritParams extr_catch
#' @param title Optional plot title.
#'
#' @return A ggplot or patchwork object.
#' @export
plot_indices_v2 <- function(mod, stock = 1, title = NULL) {
  df_fit <- extr_indices(mod, stock)
  df_cv <- extr_cv(mod, stock)
  df_brk <- extr_q_breaks(mod)

  if (nrow(df_fit) == 0) {
    return(.empty_annex_plot(title = title, subtitle = "No index data available"))
  }

  add_breaks <- function(p) {
    if (nrow(df_brk) == 0) return(p)
    df_b <- dplyr::filter(df_brk, survey %in% unique(df_fit$survey))
    if (nrow(df_b) == 0) return(p)
    p +
      ggplot2::geom_vline(
        data = df_b,
        ggplot2::aes(xintercept = break_year),
        colour = PFA_ORANGE,
        linetype = "dashed",
        linewidth = 0.6
      ) +
      ggplot2::geom_text(
        data = df_b,
        ggplot2::aes(x = break_year, y = Inf, label = break_year),
        angle = 90,
        vjust = -0.3,
        hjust = 1.1,
        colour = PFA_ORANGE,
        size = 3,
        inherit.aes = FALSE
      )
  }

  df_obs <- dplyr::filter(df_fit, !is.na(obs), obs > 0)
  p_fit <- ggplot2::ggplot(df_fit, ggplot2::aes(year)) +
    ggplot2::geom_line(ggplot2::aes(y = model), colour = PFA_ORANGE, linewidth = 1.1) +
    ggplot2::geom_errorbar(
      data = df_obs,
      ggplot2::aes(ymin = pmax(obs - 1.96 * sd, 0), ymax = obs + 1.96 * sd),
      colour = PFA_BLUE,
      width = 0.4,
      linewidth = 0.5
    ) +
    ggplot2::geom_point(data = df_obs, ggplot2::aes(y = obs), colour = PFA_BLUE, size = 2) +
    ggplot2::facet_wrap(~survey, scales = "free_y", ncol = 2) +
    ggplot2::labs(x = NULL, y = "Standardised index (relative)") +
    theme_jjm() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )
  p_fit <- add_breaks(p_fit)

  if (nrow(df_cv) == 0) {
    return(p_fit + patchwork::plot_annotation(title = title))
  }

  p_cv <- ggplot2::ggplot(df_cv, ggplot2::aes(year, cv)) +
    ggplot2::geom_area(fill = PFA_BLUE2, alpha = 0.45) +
    ggplot2::geom_line(colour = PFA_BLUE, linewidth = 0.7) +
    ggplot2::facet_wrap(~survey, scales = "free_y", ncol = 2) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
    ggplot2::labs(x = NULL, y = "Input CV") +
    theme_jjm() +
    ggplot2::theme(strip.text = ggplot2::element_blank())
  p_cv <- add_breaks(p_cv)

  p_fit / p_cv +
    patchwork::plot_layout(heights = c(3, 1)) +
    patchwork::plot_annotation(title = title)
}

#' Plot observed composition ridges
#'
#' @param df Composition fit data.
#' @param x_var X variable, usually `"age"` or `"length_bin"`.
#' @param facet_var Facet variable, usually `"fleet"` or `"survey"`.
#' @param x_lab X-axis label.
#' @param title Optional plot title.
#'
#' @return A ggplot object.
#' @export
plot_comp_ridge <- function(df, x_var = "age", facet_var = "fleet",
                            x_lab = "Age", title = NULL) {
  obs <- dplyr::filter(df, type == "observed", !is.na(prop), prop > 0)
  if (nrow(obs) == 0) {
    return(.empty_annex_plot(title = paste0(title, " - no data")))
  }

  obs <- obs |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(facet_var, "year")))) |>
    dplyr::mutate(prop = prop / max(prop, na.rm = TRUE)) |>
    dplyr::ungroup()

  ggplot2::ggplot(
    obs,
    ggplot2::aes(
      x = .data[[x_var]],
      y = year,
      height = prop,
      group = year,
      fill = year
    )
  ) +
    ggridges::geom_ridgeline(scale = 2, alpha = 0.85, colour = PFA_BLUE, linewidth = 0.25) +
    ggplot2::scale_fill_gradient(low = PFA_BLUE, high = PFA_ORANGE) +
    ggplot2::scale_y_reverse(breaks = scales::breaks_width(5)) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(2)) +
    ggplot2::facet_wrap(stats::as.formula(paste("~", facet_var)), nrow = 1) +
    ggplot2::labs(x = x_lab, y = "Year", title = title) +
    ggplot2::guides(fill = "none") +
    theme_jjm()
}

#' Plot selectivity ridges
#'
#' @param df Selectivity data from [extr_sel_fsh()] or [extr_sel_srv()].
#' @param facet_var Facet variable.
#' @param title Optional plot title.
#' @param breaks_df Optional break-year data from [extr_sel_breaks()].
#'
#' @return A ggplot object.
#' @export
plot_sel_ridge <- function(df, facet_var, title = NULL, breaks_df = NULL) {
  if (nrow(df) == 0) {
    return(.empty_annex_plot(title = paste0(title, " - selectivity data not found")))
  }

  df <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(facet_var, "year")))) |>
    dplyr::mutate(sel = sel / max(sel, na.rm = TRUE)) |>
    dplyr::ungroup()

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = age, y = year, height = sel, group = year, fill = year)
  ) +
    ggridges::geom_ridgeline(scale = 2, alpha = 0.85, colour = PFA_BLUE, linewidth = 0.25) +
    ggplot2::scale_fill_gradient(low = PFA_BLUE, high = PFA_ORANGE) +
    ggplot2::scale_y_reverse(breaks = scales::breaks_width(5)) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(2)) +
    ggplot2::facet_wrap(stats::as.formula(paste("~", facet_var)), nrow = 1) +
    ggplot2::labs(x = "Age", y = "Year", title = title) +
    ggplot2::guides(fill = "none") +
    theme_jjm()

  if (!is.null(breaks_df) && nrow(breaks_df) > 0 && facet_var %in% names(breaks_df)) {
    p <- p + ggplot2::geom_hline(
      data = breaks_df,
      ggplot2::aes(yintercept = break_year),
      colour = "white",
      linetype = "dashed",
      linewidth = 0.7,
      inherit.aes = FALSE
    )
  }
  p
}

#' Plot standard index fit diagnostics
#'
#' @param df Index fit data, usually one survey from [extr_indices()].
#' @param title Optional plot title.
#'
#' @return A patchwork object.
#' @export
plot_fit_diagnostics <- function(df, title = NULL) {
  has_sd <- "sd" %in% names(df) && !all(is.na(df$sd))
  df <- df |>
    dplyr::filter(!is.na(obs), !is.na(model), obs > 0, model > 0) |>
    dplyr::mutate(std_resid = if (has_sd) (obs - model) / sd else obs - model)
  if (nrow(df) == 0) {
    return(.empty_annex_plot(title = title, subtitle = "No fitted observations available"))
  }
  resid_lab <- if (has_sd) "Standardised residuals" else "Residuals"

  p_ts <- ggplot2::ggplot(df, ggplot2::aes(year)) +
    ggplot2::geom_line(ggplot2::aes(y = model), colour = PFA_ORANGE, linewidth = 0.8) +
    ggplot2::geom_point(ggplot2::aes(y = model), colour = PFA_ORANGE, shape = 4, size = 1.8) +
    ggplot2::geom_errorbar(
      data = if (has_sd) df else df[0, ],
      ggplot2::aes(ymin = pmax(obs - 1.96 * sd, 1e-6), ymax = obs + 1.96 * sd),
      colour = PFA_BLUE,
      width = 0.4,
      linewidth = 0.5
    ) +
    ggplot2::geom_point(ggplot2::aes(y = obs), colour = PFA_BLUE, shape = 19, size = 1.8) +
    ggplot2::scale_y_log10(labels = scales::label_comma()) +
    ggplot2::labs(x = "Year", y = "Values", title = "a) Observed and fitted") +
    theme_jjm()

  lim <- range(c(df$obs, df$model), na.rm = TRUE)
  p_1to1 <- ggplot2::ggplot(df, ggplot2::aes(model, obs)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linewidth = 0.8) +
    ggplot2::geom_point(colour = PFA_BLUE, size = 1.8) +
    ggplot2::scale_x_log10(limits = lim) +
    ggplot2::scale_y_log10(limits = lim) +
    ggplot2::labs(x = "Fitted", y = "Observed", title = "b) Observed vs fitted") +
    theme_jjm()

  p_res_ts <- ggplot2::ggplot(df, ggplot2::aes(year, std_resid)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50") +
    ggplot2::geom_segment(ggplot2::aes(xend = year, yend = 0), linewidth = 0.6) +
    ggplot2::geom_point(colour = PFA_BLUE, size = 1.8) +
    ggplot2::labs(x = "Year", y = resid_lab, title = "c) Residuals over time") +
    theme_jjm()

  p_ta <- ggplot2::ggplot(df, ggplot2::aes(model, std_resid)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50") +
    ggplot2::geom_point(colour = PFA_BLUE, size = 1.8) +
    ggplot2::scale_x_log10() +
    ggplot2::labs(x = "Fitted (log scale)", y = resid_lab, title = "d) Tukey-Anscombe") +
    theme_jjm()

  p_qq <- ggplot2::ggplot(df, ggplot2::aes(sample = std_resid)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey40") +
    ggplot2::stat_qq_line(colour = PFA_ORANGE, linewidth = 0.9) +
    ggplot2::stat_qq(colour = PFA_BLUE, size = 1.8) +
    ggplot2::labs(x = "Theoretical quantiles", y = resid_lab, title = "e) Normal Q-Q") +
    theme_jjm()

  r <- df$std_resid[!is.na(df$std_resid)]
  if (length(r) >= 4) {
    acf_out <- stats::acf(r, plot = FALSE)
    ci_bound <- stats::qnorm(0.975) / sqrt(length(r))
    acf_df <- tibble::tibble(
      lag = as.integer(acf_out$lag[-1]),
      acf = as.numeric(acf_out$acf[-1])
    )
    p_acf <- ggplot2::ggplot(acf_df, ggplot2::aes(lag, acf)) +
      ggplot2::geom_hline(yintercept = 0, colour = "grey50") +
      ggplot2::geom_hline(
        yintercept = c(-ci_bound, ci_bound),
        linetype = "dashed",
        colour = PFA_ORANGE,
        linewidth = 0.8
      ) +
      ggplot2::geom_segment(
        ggplot2::aes(xend = lag, yend = 0),
        linewidth = 0.9,
        colour = PFA_BLUE
      ) +
      ggplot2::annotate(
        "text", x = Inf, y = ci_bound, label = "95% CI",
        hjust = 1.1, vjust = -0.4, colour = PFA_ORANGE, size = 3.5
      ) +
      ggplot2::labs(x = "Lag (yrs)", y = "ACF", title = "f) Autocorrelation of residuals") +
      theme_jjm()
  } else {
    p_acf <- .empty_annex_plot(title = "f) Autocorrelation (insufficient data)")
  }

  (p_ts | p_1to1) / (p_res_ts | p_ta) / (p_qq | p_acf) +
    patchwork::plot_annotation(title = title)
}

#' Plot input composition sample sizes
#'
#' @inheritParams extr_catch
#' @param title Optional plot title.
#'
#' @return A ggplot object.
#' @export
plot_effn <- function(mod, stock = 1, title = NULL) {
  df <- extr_effn(mod, stock)
  if (nrow(df) == 0) {
    return(.empty_annex_plot(title = title, subtitle = "No sample size data available"))
  }

  ggplot2::ggplot(df, ggplot2::aes(year)) +
    ggplot2::geom_col(ggplot2::aes(y = input_n), fill = PFA_BLUE2, alpha = 0.65, width = 0.85) +
    ggplot2::facet_wrap(~paste0(name, "\n(", type, ")"), scales = "free_y", ncol = 2) +
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_width(5),
      guide = ggplot2::guide_axis(check.overlap = TRUE)
    ) +
    ggplot2::labs(x = NULL, y = "Input sample size (N)", title = title) +
    theme_jjm() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

#' Plot mean age or mean length fits
#'
#' @param df Data from `extr_mean*()` functions.
#' @param facet_var Facet variable.
#' @param y_lab Y-axis label.
#' @param title Optional plot title.
#'
#' @return A ggplot object.
#' @export
plot_meanstat <- function(df, facet_var, y_lab = "Mean age (years)", title = NULL) {
  if (nrow(df) == 0 || !facet_var %in% names(df)) {
    return(.empty_annex_plot(title = title, subtitle = "No data available"))
  }

  ggplot2::ggplot(df, ggplot2::aes(year)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = lower, ymax = upper),
      colour = PFA_BLUE,
      width = 0.4,
      linewidth = 0.5
    ) +
    ggplot2::geom_point(ggplot2::aes(y = obs), colour = PFA_BLUE, size = 2) +
    ggplot2::geom_line(ggplot2::aes(y = model), colour = PFA_ORANGE, linewidth = 1.1) +
    ggplot2::facet_wrap(stats::as.formula(paste("~", facet_var)), scales = "free_y", ncol = 2) +
    ggplot2::labs(x = NULL, y = y_lab, title = title) +
    theme_jjm()
}

#' Plot fished and unfished biomass
#'
#' @inheritParams extr_catch
#' @param title Optional plot title.
#'
#' @return A ggplot object.
#' @export
plot_fished_unfished <- function(mod, stock = 1, title = NULL) {
  df <- extr_fished_unfished(mod, stock)
  ggplot2::ggplot(df, ggplot2::aes(year, bio / 1e3, linetype = scenario, colour = scenario)) +
    ggplot2::geom_line(linewidth = 1.3) +
    ggplot2::scale_linetype_manual(values = c(Fished = "solid", Unfished = "dashed")) +
    ggplot2::scale_colour_manual(values = c(Fished = PFA_BLUE, Unfished = PFA_ORANGE)) +
    ggplot2::labs(
      x = NULL,
      y = "Total biomass (thousand mt)",
      linetype = NULL,
      colour = NULL,
      title = title
    ) +
    theme_jjm()
}

#' Plot a summary diagnostics sheet
#'
#' @inheritParams extr_catch
#' @param title Optional plot title.
#'
#' @return A patchwork object.
#' @export
plot_summary_sheet <- function(mod, stock = 1, title = NULL) {
  out <- .jjm_out(mod, stock)
  yr <- out$Yr
  msy <- out$msy_mt
  avg_bmsy <- mean(rev(msy[, 10])[seq_len(min(10, nrow(msy)))], na.rm = TRUE)

  ssb <- as.data.frame(out$SSB)
  colnames(ssb) <- c("year", "est", "sd", "lower", "upper")
  p_ssb <- ggplot2::ggplot(ssb, ggplot2::aes(year)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lower / 1e3, ymax = upper / 1e3),
      fill = PFA_BLUE,
      alpha = 0.2
    ) +
    ggplot2::geom_line(ggplot2::aes(y = est / 1e3), linewidth = 1.3, colour = PFA_BLUE) +
    ggplot2::geom_line(
      data = data.frame(year = yr, bmsy = msy[, 10] / 1e3),
      ggplot2::aes(y = bmsy),
      colour = PFA_BLUE2,
      linewidth = 1,
      linetype = "dashed"
    ) +
    ggplot2::geom_hline(yintercept = avg_bmsy / 1e3, colour = PFA_ORANGE, linewidth = 1) +
    ggplot2::labs(
      x = NULL,
      y = "SSB (thousand mt)",
      title = "Spawning Biomass",
      subtitle = "Dashed = dynamic BMSY; orange = 10-yr avg"
    ) +
    theme_jjm()

  rec <- as.data.frame(out$R)
  colnames(rec) <- c("year", "est", "sd", "lower", "upper")
  p_rec <- ggplot2::ggplot(rec, ggplot2::aes(year)) +
    ggplot2::geom_col(ggplot2::aes(y = est / 1e6), fill = PFA_BLUE2, width = 0.8, alpha = 0.8) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = lower / 1e6, ymax = upper / 1e6),
      width = 0,
      linewidth = 0.6,
      colour = PFA_BLUE
    ) +
    ggplot2::labs(x = NULL, y = "Recruitment (billions)", title = "Recruitment") +
    theme_jjm()

  f_df <- data.frame(year = yr, f = rowMeans(out$TotF[, -1, drop = FALSE]), fmsy = msy[, 5])
  p_f <- ggplot2::ggplot(f_df, ggplot2::aes(year, f)) +
    ggplot2::geom_line(linewidth = 1.3, colour = PFA_BLUE) +
    ggplot2::geom_line(ggplot2::aes(y = fmsy), colour = PFA_BLUE2, linewidth = 1, linetype = "dashed") +
    ggplot2::labs(
      x = NULL,
      y = "Fishing mortality",
      title = "Fishing Mortality",
      subtitle = "Dashed = dynamic FMSY"
    ) +
    theme_jjm()

  catch_cols <- grep("^Obs_catch_", names(out), value = TRUE)
  tot_catch <- rowSums(sapply(catch_cols, function(cn) out[[cn]]))
  p_catch <- ggplot2::ggplot(data.frame(year = yr, catch = tot_catch / 1e3), ggplot2::aes(year, catch)) +
    ggplot2::geom_col(fill = PFA_ORANGE2, width = 0.8, alpha = 0.85) +
    ggplot2::labs(x = NULL, y = "Catch (thousand mt)", title = "Total Catch") +
    theme_jjm()

  df_ir <- extr_indices(mod, stock) |>
    dplyr::filter(!is.na(obs), obs > 0, model > 0) |>
    dplyr::mutate(log_resid = log(obs / model))

  if (nrow(df_ir) > 0) {
    sn_ir <- unique(df_ir$survey)
    pal_ir <- stats::setNames(.annex_palette(SURV_PAL, length(sn_ir)), sn_ir)
    p_idx <- ggplot2::ggplot(df_ir, ggplot2::aes(year, log_resid, colour = survey)) +
      ggplot2::geom_hline(yintercept = 0, colour = "grey40", linetype = "dashed") +
      ggplot2::geom_point(size = 1.5, alpha = 0.8) +
      ggplot2::geom_line(linewidth = 0.5, alpha = 0.6) +
      ggplot2::scale_colour_manual(values = pal_ir, name = NULL) +
      ggplot2::labs(x = NULL, y = "log(obs/pred)", title = "Index log-residuals") +
      theme_jjm() +
      ggplot2::theme(legend.position = "right", legend.text = ggplot2::element_text(size = 8))
  } else {
    p_idx <- .empty_annex_plot(title = "Index log-residuals", subtitle = "No index residuals")
  }

  (p_ssb | p_f) / (p_rec | p_catch) / p_idx +
    patchwork::plot_layout(heights = c(3, 3, 2)) +
    patchwork::plot_annotation(title = title)
}

#' Extract stock-recruitment pairs and curve
#'
#' @inheritParams extr_catch
#' @return A list with `pairs`, `curve`, and `steepness`.
#' @export
extr_sr <- function(mod, stock = 1) {
  out <- .jjm_out(mod, stock)
  sr <- as.data.frame(out$Stock_Rec)
  curve <- as.data.frame(out$stock_Rec_Curve_1)
  steep <- out$Steep
  h <- if (length(steep) >= 2) steep[2] else NA_real_

  pairs <- tibble::tibble(
    year = as.integer(sr[, 1]),
    ssb = sr[, 3],
    r = sr[, 4]
  ) |>
    dplyr::filter(!is.na(ssb), !is.na(r), ssb > 0, r > 0)

  curve_df <- tibble::tibble(ssb = curve[, 1], r = curve[, 2]) |>
    dplyr::filter(ssb > 0, r > 0)

  list(pairs = pairs, curve = curve_df, steepness = h)
}

#' Plot stock-recruitment relationship
#'
#' @inheritParams extr_catch
#' @param title Optional plot title.
#'
#' @return A ggplot object.
#' @export
plot_sr <- function(mod, stock = 1, title = NULL) {
  obj <- extr_sr(mod, stock)
  df <- obj$pairs
  crv <- obj$curve
  h <- obj$steepness
  n <- nrow(df)
  if (n == 0) return(.empty_annex_plot(title = "No SR data"))
  h_lbl <- if (!is.na(h)) paste0("h = ", round(h, 3)) else NULL

  ggplot2::ggplot(df, ggplot2::aes(ssb, r)) +
    ggplot2::geom_line(
      data = crv,
      ggplot2::aes(ssb, r),
      colour = PFA_ORANGE,
      linewidth = 1.1,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_point(ggplot2::aes(colour = year), size = 2.2, alpha = 0.85) +
    ggplot2::geom_text(
      data = dplyr::slice(df, c(1L, n)),
      ggplot2::aes(label = year),
      vjust = -0.8,
      size = 3,
      colour = PFA_BLUE
    ) +
    ggplot2::scale_colour_gradient(low = PFA_BLUE, high = PFA_ORANGE2, name = "Year") +
    ggplot2::guides(
      colour = ggplot2::guide_colourbar(
        barwidth = grid::unit(8, "cm"),
        barheight = grid::unit(0.4, "cm"),
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    ggplot2::labs(
      x = "SSB",
      y = "Recruitment",
      title = .annex_default(title, "Stock-recruitment"),
      subtitle = h_lbl
    ) +
    theme_jjm()
}

#' Extract Kobe phase-plot data
#'
#' @inheritParams extr_catch
#' @return A tibble with `year`, `bbmsy`, and `ffmsy`.
#' @export
extr_kobe <- function(mod, stock = 1) {
  out <- .jjm_out(mod, stock)
  msy <- out$msy_mt
  tibble::tibble(
    year = as.integer(msy[, 1]),
    bbmsy = msy[, 13],
    ffmsy = msy[, 4]
  )
}

#' Plot Kobe phase plot
#'
#' @inheritParams extr_catch
#' @param title Optional plot title.
#'
#' @return A ggplot object.
#' @export
plot_kobe <- function(mod, stock = 1, title = NULL) {
  df <- extr_kobe(mod, stock)
  n <- nrow(df)
  if (n == 0) return(.empty_annex_plot(title = "No Kobe data"))

  xlim <- c(0, max(3, max(df$bbmsy, na.rm = TRUE) * 1.1))
  ylim <- c(0, max(3, max(df$ffmsy, na.rm = TRUE) * 1.1))

  ggplot2::ggplot(df, ggplot2::aes(bbmsy, ffmsy)) +
    ggplot2::annotate("rect", xmin = 0, xmax = 1, ymin = 1, ymax = ylim[2], fill = "#FF3333", alpha = 0.15) +
    ggplot2::annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = "#FFD700", alpha = 0.15) +
    ggplot2::annotate("rect", xmin = 1, xmax = xlim[2], ymin = 1, ymax = ylim[2], fill = "#FFD700", alpha = 0.15) +
    ggplot2::annotate("rect", xmin = 1, xmax = xlim[2], ymin = 0, ymax = 1, fill = PFA_GREEN, alpha = 0.15) +
    ggplot2::geom_vline(xintercept = 1, colour = "grey50", linewidth = 0.5) +
    ggplot2::geom_hline(yintercept = 1, colour = "grey50", linewidth = 0.5) +
    ggplot2::geom_path(ggplot2::aes(colour = year), linewidth = 0.8) +
    ggplot2::geom_point(ggplot2::aes(colour = year), size = 1.8) +
    ggplot2::geom_point(data = dplyr::slice_tail(df, n = 1), size = 4.5, colour = PFA_BLUE, shape = 18) +
    ggplot2::geom_text(
      data = dplyr::slice(df, c(1L, n)),
      ggplot2::aes(label = year),
      vjust = -0.9,
      size = 3,
      colour = PFA_BLUE
    ) +
    ggplot2::scale_colour_gradient(low = PFA_BLUE, high = PFA_ORANGE, name = "Year") +
    ggplot2::guides(
      colour = ggplot2::guide_colourbar(
        barwidth = grid::unit(8, "cm"),
        barheight = grid::unit(0.4, "cm"),
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim) +
    ggplot2::labs(x = "B / Bmsy", y = "F / Fmsy", title = .annex_default(title, "Kobe phase plot")) +
    theme_jjm()
}

#' Identify strong cohorts
#'
#' @inheritParams extr_catch
#' @param n Number of recruitment years to return.
#'
#' @return Integer vector of cohort years.
#' @export
strong_cohorts <- function(mod, stock = 1, n = 3) {
  out <- .jjm_out(mod, stock)
  rec <- as.data.frame(out$R)
  colnames(rec)[1:2] <- c("year", "est")
  rec |>
    dplyr::arrange(dplyr::desc(est)) |>
    dplyr::slice_head(n = n) |>
    dplyr::pull(year) |>
    as.integer()
}

#' Annotate strong cohort years on a ggplot
#'
#' @param years Cohort years.
#' @param colour Annotation colour.
#' @param label_size Text size.
#'
#' @return A list of ggplot layers.
#' @export
annotate_cohorts <- function(years, colour = PFA_GREEN, label_size = 3) {
  list(
    ggplot2::geom_vline(
      xintercept = years,
      colour = colour,
      linetype = "dashed",
      linewidth = 0.5,
      alpha = 0.8
    ),
    ggplot2::annotate(
      "text",
      x = years,
      y = Inf,
      label = paste0("'", substr(as.character(years), 3, 4)),
      colour = colour,
      size = label_size,
      vjust = -0.3,
      hjust = -0.2
    ),
    ggplot2::labs(caption = paste0(
      "Green dashed lines = strong cohort years (",
      paste(years, collapse = ", "),
      ")"
    ))
  )
}

#' Calculate Francis T1.8 effective sample sizes
#'
#' @param agefits Age-composition fit data.
#' @param effn_df Input sample-size data from [extr_effn()].
#' @param grp_var Group variable, `"fleet"` or `"survey"`.
#'
#' @return A tibble with input and Francis effective sample sizes.
#' @export
extr_francis <- function(agefits, effn_df, grp_var = "fleet") {
  obs <- dplyr::filter(agefits, type == "observed", !is.na(prop))
  pred <- dplyr::filter(agefits, type == "predicted", !is.na(prop))
  if (nrow(obs) == 0) return(tibble::tibble())

  dplyr::inner_join(
    obs |>
      dplyr::select(year, age, dplyr::all_of(grp_var), obs_p = prop),
    pred |>
      dplyr::select(year, age, dplyr::all_of(grp_var), pred_p = prop),
    by = c("year", "age", grp_var)
  ) |>
    dplyr::group_by(.data[[grp_var]], year) |>
    dplyr::summarise(
      num = sum(pred_p * (1 - pred_p), na.rm = TRUE),
      denom = sum((obs_p - pred_p)^2, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      {
        effn_renamed <- dplyr::rename(effn_df, input_n = input_n)
        effn_renamed[[grp_var]] <- effn_renamed$name
        effn_renamed
      },
      by = c(grp_var, "year")
    ) |>
    dplyr::filter(!is.na(input_n), input_n > 0, denom > 0) |>
    dplyr::mutate(francis_n = num / (denom / input_n), ratio = francis_n / input_n) |>
    dplyr::select(source = dplyr::all_of(grp_var), year, input_n, francis_n, ratio)
}

#' Plot Francis T1.8 reweighting diagnostics
#'
#' @inheritParams extr_catch
#' @param title Optional plot title.
#'
#' @return A ggplot object.
#' @export
plot_francis <- function(mod, stock = 1, title = NULL) {
  en <- extr_effn(mod, stock)
  fsh_age <- extr_agefits_fsh(mod, stock)
  srv_age <- extr_agefits_srv(mod, stock)

  df <- dplyr::bind_rows(
    if (nrow(fsh_age) > 0) {
      extr_francis(fsh_age, dplyr::filter(en, type == "Fishery (age)"), "fleet")
    },
    if (nrow(srv_age) > 0) {
      extr_francis(srv_age, dplyr::filter(en, type == "Survey (age)"), "survey")
    }
  )

  if (nrow(df) == 0) {
    return(.empty_annex_plot(title = "No composition data for Francis"))
  }

  ggplot2::ggplot(df, ggplot2::aes(year)) +
    ggplot2::geom_col(ggplot2::aes(y = input_n), fill = PFA_BLUE2, alpha = 0.55, width = 0.85) +
    ggplot2::geom_point(ggplot2::aes(y = francis_n), colour = PFA_ORANGE, size = 1.8) +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = mean(input_n)),
      colour = PFA_BLUE,
      linetype = "dashed",
      linewidth = 0.6
    ) +
    ggplot2::facet_wrap(~source, scales = "free_y", ncol = 2) +
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_width(5),
      guide = ggplot2::guide_axis(check.overlap = TRUE)
    ) +
    ggplot2::labs(
      x = NULL,
      y = "N  (bars = input, dots = Francis)",
      title = .annex_default(title, "Francis reweighting"),
      subtitle = "Dashed = mean input N; dots above/below bars = under/over-weighted"
    ) +
    theme_jjm() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

#' Extract q trajectories
#'
#' @inheritParams extr_catch
#' @return A tibble with catchability by survey and year.
#' @export
extr_q_trajectory <- function(mod, stock = 1) {
  out <- .jjm_out(mod, stock)
  sn <- .snames(out)
  purrr::imap_dfr(seq_along(sn), function(i, ...) {
    qmat <- out[[paste0("q_", i)]]
    if (is.null(qmat) || !is.matrix(qmat) || ncol(qmat) < 2) return(NULL)
    tibble::tibble(year = as.integer(qmat[, 1]), q = qmat[, 2], survey = sn[i])
  })
}

#' Plot q trajectories
#'
#' @inheritParams extr_catch
#' @param title Optional plot title.
#'
#' @return A ggplot object.
#' @export
plot_q_trajectory <- function(mod, stock = 1, title = NULL) {
  df <- extr_q_trajectory(mod, stock) |>
    dplyr::filter(!is.na(q))
  if (nrow(df) == 0) return(.empty_annex_plot(title = "No q data"))

  srvs <- unique(df$survey)
  pal_sv <- stats::setNames(.annex_palette(SURV_PAL, length(srvs)), srvs)

  ggplot2::ggplot(df, ggplot2::aes(year, q, colour = survey)) +
    ggplot2::geom_step(linewidth = 0.9) +
    ggplot2::scale_colour_manual(values = pal_sv, name = NULL) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(5)) +
    ggplot2::facet_wrap(~survey, scales = "free_y", ncol = 2) +
    ggplot2::labs(x = NULL, y = "Catchability (q)", title = .annex_default(title, "Survey catchability")) +
    theme_jjm() +
    ggplot2::theme(legend.position = "none")
}

#' Extract survey skill data
#'
#' @inheritParams extr_catch
#' @return A tibble with survey log-ratios and model SSB direction.
#' @export
extr_survey_skill <- function(mod, stock = 1) {
  out <- .jjm_out(mod, stock)
  msy <- out$msy_mt
  ssb_df <- tibble::tibble(year = as.integer(msy[, 1]), ssb = msy[, 12]) |>
    dplyr::arrange(year) |>
    dplyr::mutate(ssb_dir = as.integer(ssb > dplyr::lag(ssb))) |>
    dplyr::filter(!is.na(ssb_dir))

  ind_df <- extr_indices(mod, stock) |>
    dplyr::filter(!is.na(obs), obs > 0) |>
    dplyr::arrange(survey, year) |>
    dplyr::group_by(survey) |>
    dplyr::mutate(log_ratio = log(obs / dplyr::lag(obs))) |>
    dplyr::filter(!is.na(log_ratio)) |>
    dplyr::ungroup()

  dplyr::inner_join(ind_df, dplyr::select(ssb_df, year, ssb_dir), by = "year")
}

.calc_roc <- function(truth, score) {
  thresholds <- sort(unique(score), decreasing = TRUE)
  roc <- purrr::map_dfr(c(Inf, thresholds), function(thr) {
    pred <- as.integer(score >= thr)
    tp <- sum(pred == 1L & truth == 1L)
    fp <- sum(pred == 1L & truth == 0L)
    fn <- sum(pred == 0L & truth == 1L)
    tn <- sum(pred == 0L & truth == 0L)
    tibble::tibble(
      tpr = if (tp + fn > 0) tp / (tp + fn) else 0,
      fpr = if (fp + tn > 0) fp / (fp + tn) else 0
    )
  })
  dplyr::bind_rows(tibble::tibble(tpr = 0, fpr = 0), roc, tibble::tibble(tpr = 1, fpr = 1)) |>
    dplyr::distinct()
}

.calc_auc <- function(roc_df) {
  r <- dplyr::arrange(roc_df, fpr, tpr)
  sum(diff(r$fpr) * (utils::head(r$tpr, -1) + utils::tail(r$tpr, -1)) / 2)
}

#' Plot survey skill ROC curves
#'
#' @inheritParams extr_catch
#' @param title Optional plot title.
#'
#' @return A ggplot object.
#' @export
plot_survey_skill <- function(mod, stock = 1, title = NULL) {
  df <- extr_survey_skill(mod, stock)
  if (nrow(df) == 0) {
    return(.empty_annex_plot(title = "No survey skill data"))
  }

  srvs <- unique(df$survey)
  pal <- stats::setNames(.annex_palette(SURV_PAL, length(srvs)), srvs)
  roc_df <- purrr::map_dfr(srvs, function(s) {
    d <- dplyr::filter(df, survey == s)
    roc <- .calc_roc(d$ssb_dir, d$log_ratio)
    auc <- .calc_auc(roc)
    dplyr::mutate(
      roc,
      survey = s,
      facet_lbl = sprintf("%s  |  AUC = %.2f  (n = %d)", s, auc, nrow(d))
    )
  })

  ggplot2::ggplot(roc_df, ggplot2::aes(fpr, tpr, colour = survey)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "grey70", linetype = "dashed", linewidth = 0.5) +
    ggplot2::geom_step(linewidth = 0.9, direction = "hv") +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::scale_colour_manual(values = pal, name = NULL) +
    ggplot2::scale_x_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
    ggplot2::facet_wrap(~facet_lbl, ncol = 2) +
    ggplot2::labs(
      x = "False positive rate",
      y = "True positive rate",
      title = .annex_default(title, "Survey skill (ROC)"),
      caption = "Truth = year-over-year direction of model SSB; score = survey log-ratio"
    ) +
    theme_jjm() +
    ggplot2::theme(legend.position = "none", strip.text = ggplot2::element_text(size = 8))
}

.metadata_matrix_long <- function(mat, names_vec, var, series_type = NULL) {
  empty <- tibble::tibble(
    var = character(),
    seriesnr = integer(),
    year = numeric(),
    series = character(),
    seriestype = character()
  )
  if (is.null(mat)) return(empty)
  mat <- as.data.frame(mat)
  max_col <- min(ncol(mat), length(names_vec))
  df <- purrr::map_dfr(seq_len(max_col), function(j) {
    tibble::tibble(
      var = var,
      seriesnr = j,
      year = mat[[j]],
      series = names_vec[j]
    )
  }) |>
    dplyr::filter(!is.na(year), year > 0)
  if (nrow(df) == 0) return(empty)
  if (is.null(series_type)) {
    dplyr::mutate(df, seriestype = ifelse(grepl("cpue", tolower(series)), "CPUE", "SURVEY"))
  } else {
    dplyr::mutate(df, seriestype = as.character(series_type))
  }
}

#' Plot metadata overview
#'
#' @param mod A `jjm.output` object from [readJJM()].
#' @param title Optional plot title.
#'
#' @return A ggplot object showing index, age-composition, and
#'   length-composition years by data source.
#' @export
plot_metadata_overview <- function(mod, title = NULL) {
  dat <- mod[[1]]$data

  sn <- as.character(dat$Inames)
  fn <- as.character(dat$Fnames)
  df <- dplyr::bind_rows(
    .metadata_matrix_long(dat$Iyears, sn, "index"),
    .metadata_matrix_long(dat$Iyearsage, sn, "age"),
    .metadata_matrix_long(dat$Iyearslength, sn, "length"),
    .metadata_matrix_long(dat$Fageyears, fn, "age", "CATCH"),
    .metadata_matrix_long(dat$Flengthyears, fn, "length", "CATCH")
  ) |>
    dplyr::mutate(
      series2 = paste(seriesnr, series, var, sep = " / "),
      var = factor(var, levels = c("index", "age", "length"))
    )

  if (nrow(df) == 0) {
    return(.empty_annex_plot(title = title, subtitle = "No metadata years available"))
  }

  ggplot2::ggplot(df, ggplot2::aes(x = year, y = forcats::fct_rev(factor(series2)))) +
    ggplot2::geom_point(ggplot2::aes(colour = var, shape = var), size = 3) +
    ggplot2::scale_colour_manual(
      values = c(index = PFA_BLUE2, age = PFA_ORANGE, length = PFA_GREEN),
      labels = c(index = "Index", age = "Age comp.", length = "Length comp.")
    ) +
    ggplot2::scale_shape_manual(
      values = c(index = 19, age = 17, length = 15),
      labels = c(index = "Index", age = "Age comp.", length = "Length comp.")
    ) +
    ggplot2::labs(x = NULL, y = NULL, colour = NULL, shape = NULL, title = title) +
    ggplot2::facet_grid(seriestype ~ ., scales = "free_y", space = "free") +
    theme_jjm()
}

.annex_lowcase <- function(df) {
  names(df) <- tolower(names(df))
  names(df) <- gsub("\\?|\\s+|\\.+|_+|\\(|\\)", "", names(df))
  df
}

#' Read historical retrospective CSV data
#'
#' @param file CSV file path.
#' @param exclude_assessment_types Assessment types to remove before plotting.
#'
#' @return A prepared data frame.
#' @export
read_historic_retro <- function(file, exclude_assessment_types = c("benchmark", "mod1.4")) {
  d <- utils::read.csv(file, header = TRUE, stringsAsFactors = FALSE) |>
    .annex_lowcase()

  required <- c("assessmenttype", "assessmentyear", "year")
  missing <- setdiff(required, names(d))
  if (length(missing)) {
    stop("Historical retrospective data are missing columns: ", paste(missing, collapse = ", "))
  }

  if ("favg" %in% names(d) && !"f" %in% names(d)) {
    names(d)[names(d) == "favg"] <- "f"
  }

  d |>
    dplyr::filter(!(assessmenttype %in% exclude_assessment_types)) |>
    dplyr::mutate(
      assessmenttype = ifelse(assessmentyear == max(assessmentyear), "last", "assess"),
      tyear = substr(as.character(assessmentyear), 3, 4)
    )
}

.historic_terminal_labels <- function(d, value) {
  d |>
    dplyr::filter(!is.na(.data[[value]])) |>
    dplyr::group_by(assessmentyear) |>
    dplyr::filter(year == max(year, na.rm = TRUE)) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup()
}

.historic_line_plot <- function(d, value, title, lower = NULL, upper = NULL, refline = NULL) {
  if (!value %in% names(d)) {
    return(.empty_annex_plot(title = title, subtitle = paste("Column", value, "not found")))
  }

  data <- dplyr::filter(d, !is.na(.data[[value]]))
  if (nrow(data) == 0) {
    return(.empty_annex_plot(title = title, subtitle = "No data available"))
  }

  p <- ggplot2::ggplot(data, ggplot2::aes(year, .data[[value]], group = assessmentyear)) +
    ggplot2::geom_line(
      ggplot2::aes(colour = assessmenttype, linewidth = assessmenttype, linetype = assessmenttype)
    ) +
    ggplot2::geom_text(
      data = .historic_terminal_labels(data, value),
      ggplot2::aes(label = tyear, colour = assessmenttype),
      hjust = -0.15,
      size = 3,
      show.legend = FALSE
    ) +
    ggplot2::scale_colour_manual(values = c(last = PFA_BLUE, assess = "grey40", bench = PFA_BLUE2, old = "grey60")) +
    ggplot2::scale_linetype_manual(values = c(last = "solid", assess = "solid", bench = "dashed", old = "dotdash")) +
    ggplot2::scale_linewidth_manual(values = c(last = 1.5, assess = 0.8, bench = 1.2, old = 0.8)) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.1))) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::labs(x = NULL, y = NULL, title = title) +
    theme_jjm() +
    ggplot2::theme(legend.position = "none")

  if (!is.null(lower) && !is.null(upper) && all(c(lower, upper) %in% names(data))) {
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data[[lower]], ymax = .data[[upper]], fill = assessmenttype),
      alpha = 0.2,
      colour = NA
    ) +
      ggplot2::scale_fill_manual(values = c(last = PFA_BLUE, assess = "white", bench = PFA_BLUE2, old = "grey80"))
  }

  if (!is.null(refline)) {
    p <- p + ggplot2::geom_hline(yintercept = refline, colour = PFA_BLUE)
  }

  p
}

#' Plot historical retrospective diagnostics
#'
#' @param data A prepared data frame from [read_historic_retro()] or a CSV file
#'   path.
#' @param quantity Plot group: `"stock"` for SSB/F/recruitment or `"reference"`
#'   for reference points.
#' @param title Optional patchwork title.
#'
#' @return A patchwork object.
#' @export
plot_historic_retro <- function(data, quantity = c("stock", "reference"), title = NULL) {
  quantity <- match.arg(quantity)
  if (is.character(data) && length(data) == 1L) {
    data <- read_historic_retro(data)
  }

  if (quantity == "stock") {
    p <- .historic_line_plot(data, "ssb", "SSB ('000 t)", "ssblow", "ssbupp") /
      .historic_line_plot(data, "f", "F") /
      .historic_line_plot(data, "r", "Recruitment (age 1; millions)", "rlow", "rupp")
  } else {
    p <- (
      .historic_line_plot(data, "bbmsy", "B/Bmsy", refline = 1) |
        .historic_line_plot(data, "ffmsy", "F/Fmsy", refline = 1)
    ) / (
      .historic_line_plot(data, "bmsy", "Bmsy") |
        .historic_line_plot(data, "fmsy", "Fmsy")
    )
  }

  if (!is.null(title)) {
    p <- p + patchwork::plot_annotation(title = title)
  }
  p
}
