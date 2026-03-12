# NormicDoE — Developer Guide

R package for designing and analysing Design of Experiments (DoE) setups.
Follows [r-pkgs.org](https://r-pkgs.org/) best practices. MIT licence.

---

## Package commands

```r
# Regenerate NAMESPACE + man/
roxygen2::roxygenise()

# Run tests
devtools::test()

# Full check (target: 0 errors, 0 warnings, 1 note — timestamp only)
devtools::check()

R 4.5.1 at /c/Program Files/R/R-4.5.1/bin/Rscript

Architecture
R/
  NormicDoE-package.R   # all @importFrom declarations — update here first
  doe_design.R          # S3 class, constructor new_doe_design(), print/summary/as.data.frame
  factorial_design.R    # full_factorial() — public entry point
  model_fit.R           # fit_model(), extract_effects(), model_summary()
  optimization.R        # optimize_response() — grid search + L-BFGS-B
  utils.R               # build_formula(), encode/decode_to_coded(), plot_*(), is_saturated()
  export.R              # export_design()
  app_ui.R              # app_ui(factors) — Shiny UI
  app_server.R          # app_server(init_design) — Shiny server
  app.R                 # launch_app() — public entry point
tests/testthat/
  test-doe_design.R
  test-factorial_design.R
  test-model_fit.R
  test-optimization.R
  test-export.R
  test-utils.R

Key conventions
full_factorial() uses reversed expand.grid() args for Yates order.
2-level designs: coded matrix (-1/+1) via encode_to_coded(); lm fitted on coded values → coefficients = half-effects.
as.data.frame.doe_design must include row.names = NULL, optional = FALSE for S3 generic compatibility.
Use utils::globalVariables(".data") to suppress R CMD check note for the ggplot2 .data pronoun.
LICENSE file must be the short DCF stub (YEAR: / COPYRIGHT HOLDER:), not full MIT text.
Non-ASCII characters in R source must use \uXXXX escapes.
Completed work
v0.1.0
full_factorial() — full factorial design generation (S3 class doe_design)
fit_model(), extract_effects(), model_summary() — linear model on coded or actual values
optimize_response() — grid search + L-BFGS-B optimisation (max / min / target)
export_design() — CSV export
Shiny app (launch_app()) — main effects, interaction effects, Pareto chart, model summary, optimisation panel
60 tests, 0 errors, 0 warnings in R CMD check
v0.2.0
Replicates: full_factorial(replicates = n) repeats each treatment combination n times, adding residual df.
Multi-factor interaction plots: plot_interaction() extended to 3-factor (facet_wrap) and 4-factor (facet_grid) layouts.
Saturated-model guard: plot_pareto() warns gracefully (no NaN crash); model_summary() exposes is_saturated flag; Shiny defaults interactions selector to "two_way" for 2-level unreplicated designs.
CSV upload in Shiny: two modes — full CSV (select factor + response columns, design reconstructed from file) and response-only (append to an existing design).
Interactions selector: "All" / "Two-way only" / "Main effects only" in the sidebar.
Multi-factor checkbox for interaction tab (2–4 factors).
83 tests, 0 errors, 0 warnings in R CMD check
Next steps
1. Normic branding — Shiny theme
Apply the shared Bootstrap theme from NormicBranding.

Approach

Add NormicBranding as a git submodule (or install locally with devtools::install_github()).
Add bslib (>= 0.7.0) to Imports in DESCRIPTION.
In app_ui.R, pass theme = bslib::bs_theme(brand = "_brand.yml") to fluidPage().
Add @importFrom bslib bs_theme to NormicDoE-package.R.
Brand palette (for reference — the _brand.yml encodes these):

Name	Primary
Green	#006f32
Yellow	#ffcd00
Purple	#946aab
Blue	#2e5558
Brown	#3e3d33
Red	#e52519
Font: Libre Franklin (Google Fonts — requires internet, or bundle as static asset).

2. Normic plot colours — ggplot2 scales
Replace default ggplot2 discrete colours with the Normic palette from ggNormic.

Approach

Add ggNormic to Suggests (not Imports — non-CRAN); apply it opportunistically in each plot_*() helper in utils.R:
# At the end of each plot_* that uses discrete colour:
if (requireNamespace("ggNormic", quietly = TRUE)) {
  p <- p + ggNormic::scale_color_normic_d()
}
p

For continuous scales (response surface): ggNormic::scale_color_normic_c().
Replace theme_bw() calls with ggNormic::theme_normic() (same fallback pattern).
Add installation note to README: devtools::install_github("Nordic-Microbes/ggNormic").
ggNormic exports: scale_color_normic_c(), scale_color_normic_d(palette),
theme_normic(base_size, panel_border), normic_colors.

3. Fix optimization — NAs introduced by coercion
Root cause (R/optimization.R:81)

grid_lists <- lapply(ranges, function(r) seq(r[1], r[2], length.out = grid_size))

ranges is built from range(design$level_values[[f]]). When a design is loaded via
CSV upload, level_values stores sort(unique(df[[f]])). If a factor column contains
character data, range() returns characters and seq() produces NAs.

Secondary bug (R/optimization.R:80)

grid_size <- min(11L, 5L)   # always 5 — ignores number of factors

Intended to reduce grid density for high-dimensional designs but min(11, 5) is a constant.

Fix in optimize_response()

# Coerce range bounds to numeric; error clearly on categorical factors
ranges <- lapply(design$factors, function(f) {
  lv <- design$level_values[[f]]
  r  <- suppressWarnings(as.numeric(range(lv)))
  if (anyNA(r))
    stop(sprintf("Factor '%s' has non-numeric levels and cannot be optimised.", f),
         call. = FALSE)
  if (!is.null(constraints[[f]])) constraints[[f]] else r
})

# Grid density: coarser for >3 factors to keep grid_size^n manageable
grid_size <- if (length(design$factors) > 3L) 5L else 11L

Add test: expect_error(optimize_response(d_categorical), regexp = "non-numeric").

4. Better factor effects overview — main effects tab
Current state: single selectInput shows one factor at a time.

Goal: all-factors overview so the user can rank importance at a glance.

Approach

Add plot_all_main_effects(design) in utils.R:
Builds a long-form data frame: columns factor, level, mean_response.
Uses facet_wrap(~ factor, scales = "free_x") so each panel has its own x-axis.
Optionally adds a ranked bar of absolute half-effects (signed, coloured positive/negative).
In app_ui.R, add plotOutput("all_main_effects_plot") above the per-factor selector,
or add a toggle radio ("Overview" / "Single factor").
Keep the existing plot_main_effects(design, factor_name) unchanged.
5. Effect comparison between samples
Goal: load responses from multiple experiments (batches, conditions) and overlay their
effect estimates on one chart.

Approach

New file R/comparison.R:
#' @export
compare_effects <- function(..., labels = NULL) {
  # Accepts multiple fitted doe_design objects
  # Binds their extract_effects() tables with a 'sample' column
}

#' @export
plot_effect_comparison <- function(comparison_df) {
  # Grouped bar chart: x = term, fill = sample, y = effect
  # Error bars from std_error where available
}

In the Shiny sidebar, add a "Compare samples" section:
fileInput for additional response CSVs.
On load, fit the same model formula to the new response and push to rv$designs
(named list; primary design = "primary").
New "Comparison" tab with plotOutput("comparison_plot").
Refactor rv$design → rv$designs[["primary"]] throughout app_server.R.
6. Bayesian optimisation
Implement a GP-based ask-tell optimisation loop modelled on
ProcessOptimizer.

CRAN dependencies (add to Suggests):

DiceKriging (>= 1.6.0) — Gaussian Process surrogate
DiceOptim (>= 2.1.1) — Expected Improvement acquisition
New file R/bayes_opt.R

#' Suggest next experimental run via Bayesian optimisation
#'
#' Fits a Gaussian Process surrogate to observed responses and returns the
#' factor settings that maximise Expected Improvement (or minimise / target).
#'
#' @param design A doe_design with at least one fitted response.
#' @param goal "max" | "min" | "target"
#' @param target Numeric. Required when goal = "target".
#' @param n_suggestions Integer. Points to suggest (default 1).
#' @return A data frame of suggested factor settings.
#' @export
bayes_suggest <- function(design, goal = "max", target = NULL,
                          n_suggestions = 1L) { ... }

#' Update the Bayesian model with new observations
#' @export
bayes_update <- function(design, new_responses) { ... }

Key design decisions

Store the fitted GP object in design$bayes_model (new slot).
Respect the same factor bounds as optimize_response().
Guard with requireNamespace("DiceKriging") and requireNamespace("DiceOptim").
The ask-tell pattern maps directly: bayes_suggest() = ask, bayes_update() = tell.
Shiny integration — new "Bayesian opt." tab:

Contour plot of the GP surrogate surface (2D) or slice plots (>2D).
"Suggest next run" button displays recommended factor settings.
User pastes new observation and clicks "Update model" to call bayes_update().