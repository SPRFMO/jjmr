% Please edit documentation in R/annex-plots.R
\name{annex_diagnostics}
\alias{annex_diagnostics}
\alias{annotate_cohorts}
\alias{calc_mohn_rho}
\alias{comp_fig_height}
\alias{extr_agefits_fsh}
\alias{extr_agefits_srv}
\alias{extr_catch}
\alias{extr_catch_cv}
\alias{extr_cv}
\alias{extr_effn}
\alias{extr_fished_unfished}
\alias{extr_francis}
\alias{extr_indices}
\alias{extr_kobe}
\alias{extr_lenfits_fsh}
\alias{extr_meanage_fsh}
\alias{extr_meanage_srv}
\alias{extr_meanlen_fsh}
\alias{extr_q_breaks}
\alias{extr_q_trajectory}
\alias{extr_retro}
\alias{extr_sel_breaks}
\alias{extr_sel_fsh}
\alias{extr_sel_srv}
\alias{extr_sr}
\alias{extr_survey_skill}
\alias{extr_wtat_fsh}
\alias{extr_wtat_srv}
\alias{facet_fig_height}
\alias{has_comp}
\alias{jjm_label}
\alias{jjm_pfa_palette}
\alias{plot_catch_fleet}
\alias{plot_comp_fits}
\alias{plot_comp_ridge}
\alias{plot_effn}
\alias{plot_fished_unfished}
\alias{plot_fit_diagnostics}
\alias{plot_francis}
\alias{plot_historic_retro}
\alias{plot_indices_v2}
\alias{plot_kobe}
\alias{plot_meanstat}
\alias{plot_metadata_overview}
\alias{plot_q_trajectory}
\alias{plot_retro}
\alias{plot_sel_ridge}
\alias{plot_sr}
\alias{plot_summary_sheet}
\alias{plot_survey_skill}
\alias{plot_wtat}
\alias{plot_wtat_cohort}
\alias{read_historic_retro}
\alias{strong_cohorts}
\title{Annex diagnostic plotting helpers}
\usage{
jjm_label(nm, stock_name = NULL)
jjm_pfa_palette(type = c("pfa", "fleet", "survey", "peel"))
extr_catch(mod, stock = 1)
extr_wtat_fsh(mod, stock = 1)
extr_wtat_srv(mod, stock = 1)
extr_agefits_fsh(mod, stock = 1)
extr_lenfits_fsh(mod, stock = 1)
extr_agefits_srv(mod, stock = 1)
extr_indices(mod, stock = 1)
extr_meanage_fsh(mod, stock = 1)
extr_meanage_srv(mod, stock = 1)
extr_meanlen_fsh(mod, stock = 1)
extr_effn(mod, stock = 1)
extr_catch_cv(mod, stock = 1)
extr_fished_unfished(mod, stock = 1)
extr_retro(retro, var = "SSB", stock = 1)
extr_q_breaks(mod)
extr_cv(mod, stock = 1)
extr_sel_fsh(mod, stock = 1)
extr_sel_srv(mod, stock = 1)
extr_sel_breaks(mod, type = c("fsh", "srv"))
extr_sr(mod, stock = 1)
extr_kobe(mod, stock = 1)
extr_francis(agefits, effn_df, grp_var = "fleet")
extr_q_trajectory(mod, stock = 1)
extr_survey_skill(mod, stock = 1)
has_comp(df, grp_var = NULL, grp_filter = NULL)
comp_fig_height(df, grp_var = NULL, grp_filter = NULL, ncol = 5,
  panel_h = 2.2, row2_h = 2.5, row3_h = 2.2)
facet_fig_height(n_panels, ncol = 2, panel_h = 3.5, min_h = 4)
plot_catch_fleet(mod, stock = 1, title = NULL)
calc_mohn_rho(df, n_peel = 5)
plot_retro(retro, var = "SSB", stock = 1, title = NULL)
plot_wtat(df, facet_var, title = NULL, years = NULL)
plot_wtat_cohort(df, facet_var, title = NULL, years = NULL)
plot_comp_fits(df, x_var, grp_var, grp_filter = NULL, x_lab = "Age",
  title = NULL)
plot_indices_v2(mod, stock = 1, title = NULL)
plot_comp_ridge(df, x_var = "age", facet_var = "fleet", x_lab = "Age",
  title = NULL)
plot_sel_ridge(df, facet_var, title = NULL, breaks_df = NULL)
plot_fit_diagnostics(df, title = NULL)
plot_effn(mod, stock = 1, title = NULL)
plot_meanstat(df, facet_var, y_lab = "Mean age (years)", title = NULL)
plot_fished_unfished(mod, stock = 1, title = NULL)
plot_summary_sheet(mod, stock = 1, title = NULL)
plot_sr(mod, stock = 1, title = NULL)
plot_kobe(mod, stock = 1, title = NULL)
strong_cohorts(mod, stock = 1, n = 3)
annotate_cohorts(years, colour = PFA_GREEN, label_size = 3)
plot_francis(mod, stock = 1, title = NULL)
plot_q_trajectory(mod, stock = 1, title = NULL)
plot_survey_skill(mod, stock = 1, title = NULL)
plot_metadata_overview(mod, title = NULL)
read_historic_retro(file,
  exclude_assessment_types = c("benchmark", "mod1.4"))
plot_historic_retro(data, quantity = c("stock", "reference"),
  title = NULL)
}
\arguments{
\item{mod}{A \code{jjm.output} object returned by \code{\link{readJJM}}.}
\item{stock}{Stock index.}
\item{retro}{A retrospective object loaded from a JJM retrospective RData file.}
\item{df, agefits, effn_df, data}{Data frames or tibbles returned by the corresponding extractor functions.}
\item{title}{Optional plot title.}
\item{var}{Retrospective variable, such as \code{"SSB"}, \code{"R"}, or \code{"F"}.}
\item{type, quantity}{Character option selecting a palette, selectivity type, or historical retrospective plot group.}
\item{grp_var}{Grouping variable name.}
\item{grp_filter}{Optional group value to filter.}
\item{ncol}{Number of panels per row.}
\item{panel_h}{Height per panel row.}
\item{row2_h}{Height for the bubble residual row.}
\item{row3_h}{Height for the marginal residual row.}
\item{n_panels}{Number of panels.}
\item{min_h}{Minimum figure height.}
\item{n_peel}{Number of retrospective peels to include.}
\item{facet_var}{Facet variable name.}
\item{x_var}{X variable name.}
\item{x_lab}{X-axis label.}
\item{breaks_df}{Optional selectivity break-year data.}
\item{y_lab}{Y-axis label.}
\item{n}{Number of cohort years to return.}
\item{colour}{Annotation colour.}
\item{label_size}{Annotation label size.}
\item{exclude_assessment_types}{Assessment types to remove before plotting historical retrospectives.}
\item{nm, stock_name}{Model name and optional stock label used to construct a display label.}
\item{file}{Path to a historical retrospective CSV file.}
\item{years}{Years to annotate or plot.}
}
\value{
Extractor functions return tibbles or lists. Plot functions return \pkg{ggplot2}
or \pkg{patchwork} objects.
}
\description{
Helpers for building benchmark annex diagnostic figures from JJM model outputs,
including input metadata, composition fits, index diagnostics, retrospective
plots, selectivity, stock status, stock-recruitment, Kobe plots, and historical
retrospectives.
}
\examples{
\dontrun{
mod <- readJJM("h1_1.14", path = "assessment/config",
  input = "assessment/input", output = "assessment/results")

plot_metadata_overview(mod)
plot_catch_fleet(mod, title = jjm_label("h1_1.14"))
plot_indices_v2(mod)
}
}
