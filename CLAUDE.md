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
```

R 4.5.1 at `/c/Program Files/R/R-4.5.1/bin/Rscript`

## Architecture

```
R/
  NormicDoE-package.R   # all @importFrom declarations — update here first
  doe_design.R          # S3 class, constructor new_doe_design(), print/summary/as.data.frame
  factorial_design.R    # full_factorial() — public entry point
  model_fit.R           # fit_model(), extract_effects(), model_summary()
  optimization.R        # optimize_response() — categorical enumeration + L-BFGS-B
  utils.R               # build_formula(), encode/decode_to_coded(), plot_*(), is_saturated()
  export.R              # export_design()
  comparison.R          # compare_effects(), plot_effect_comparison()
  bayes_opt.R           # bayes_suggest(), bayes_update()
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
  test-comparison.R
  test-bayes_opt.R
inst/
  brand.yml             # Normic Bootstrap brand colours for bslib
```

## Key conventions

- `full_factorial()` uses reversed `expand.grid()` args for Yates order.
- 2-level designs: coded matrix (-1/+1) via `encode_to_coded()`; lm fitted on coded values → coefficients = half-effects.
- `as.data.frame.doe_design` must include `row.names = NULL, optional = FALSE` for S3 generic compatibility.
- Use `utils::globalVariables(".data")` to suppress R CMD check note for the ggplot2 `.data` pronoun.
- LICENSE file must be the short DCF stub (`YEAR: / COPYRIGHT HOLDER:`), not full MIT text.
- Non-ASCII characters in R source must use `\uXXXX` escapes (e.g. `\u00d7` for ×).
- All `@importFrom` declarations live in `R/NormicDoE-package.R` — update this file first.
- **File writes**: Edit/Write tools fail with EEXIST in worktrees. Use `python3 - << 'PYEOF'` via Bash for all file modifications. Write ASCII-only Python string literals; non-ASCII in file content must be passed as unicode escapes in the Python string (e.g. `\u2014`), not as literals in the heredoc.
- **NAMESPACE** is gitignored; do not `git add NAMESPACE`.
- **Branch creation**: `main` is checked out in the parent repo. Use `git checkout -b <branch> origin/main`.

---

## Completed work

### v0.1.0
- `full_factorial()` — full factorial design generation (S3 class `doe_design`)
- `fit_model()`, `extract_effects()`, `model_summary()` — linear model on coded or actual values
- `optimize_response()` — grid search + L-BFGS-B optimisation (max / min / target)
- `export_design()` — CSV export
- Shiny app (`launch_app()`) — main effects, interaction effects, Pareto chart, model summary, optimisation panel
- 60 tests, 0 errors, 0 warnings in R CMD check

### v0.2.0
- Replicates: `full_factorial(replicates = n)` repeats each treatment combination n times.
- Multi-factor interaction plots: `plot_interaction()` extended to 3-factor (`facet_wrap`) and 4-factor (`facet_grid`) layouts.
- Saturated-model guard: `plot_pareto()` warns gracefully; `model_summary()` exposes `is_saturated` flag.
- CSV upload in Shiny: full CSV mode (select factor + response columns) and response-only mode.
- Interactions selector: "All" / "Two-way only" / "Main effects only" in the sidebar.
- 83 tests, 0 errors, 0 warnings in R CMD check

### v0.3.0 — open PRs (merge in this order)

| PR | Branch | Title |
|----|--------|-------|
| #6 | `feat/unit1-fix-optimization` | fix: optimization — categorical enumeration + zero-range guard |
| #1 | `feat/unit2-ggnormic-colors` | feat: ggNormic opportunistic theme and color scales |
| #2 | `feat/unit3-normic-branding` | feat: Normic Bootstrap branding via bslib |
| #3 | `feat/unit4-all-main-effects` | feat: all-factors main effects overview plot |
| #4 | `feat/unit5-effect-comparison` | feat: effect comparison between samples |
| #5 | `feat/unit6-bayesian-opt` | feat: Bayesian optimisation via DiceKriging/DiceOptim |

**Merge order:** #6 first (no conflicts), then #1+#2 in either order, then #3→#4→#5 sequentially.

After all PRs are merged, bump `Version` in DESCRIPTION to `0.3.0` and tag `v0.3.0`.

#### v0.3.0 feature summary

1. **Optimization fix** (`R/optimization.R`)
   - Categorical factors (character levels) are enumerated; best combination returned.
   - Numeric factors with zero range (single level, e.g. a factor tested at only one
     temperature) are treated as fixed — prevents L-BFGS-B "non-finite finite-difference
     value" error.
   - Adaptive grid size: 11 points/factor for ≤3 factors, 5 for >3.
   - Shiny output prints categorical values with `%s` instead of `%.4f`.

2. **ggNormic colours** (`R/utils.R`, DESCRIPTION Suggests)
   - `ggNormic::theme_normic()` and `ggNormic::scale_color_normic_d()` applied
     opportunistically with `requireNamespace()` + `tryCatch()` fallback to `theme_bw()`.
   - Install: `devtools::install_github("Nordic-Microbes/ggNormic")`.

3. **Normic branding** (`inst/brand.yml`, `R/app_ui.R`, `R/NormicDoE-package.R`)
   - `bslib::bs_theme(brand = brand_yml)` passed to `fluidPage()`.
   - `bslib (>= 0.7.0)` added to Imports.

4. **All-factors main effects overview** (`R/utils.R`, `R/app_ui.R`, `R/app_server.R`)
   - New `plot_all_main_effects(design)`: faceted plot, one panel per factor.
   - Main Effects tab has radio toggle "Overview (all factors)" / "Single factor".

5. **Effect comparison** (`R/comparison.R`, Shiny Comparison tab)
   - `compare_effects(..., labels)` binds `extract_effects()` tables with a `sample` column.
   - `plot_effect_comparison(df)`: grouped bar chart with error bars.
   - Shiny: upload additional response CSVs; stored in `rv$comparison_designs`.

6. **Bayesian optimisation** (`R/bayes_opt.R`, Shiny Bayesian Opt tab)
   - `bayes_suggest(design, goal, target)`: GP surrogate (DiceKriging) + EI (DiceOptim).
   - `bayes_update(design, new_settings, new_response)`: appends run, re-fits model.
   - Guarded with `requireNamespace()`. DiceKriging + DiceOptim in Suggests.

---

## Next steps (post v0.3.0)

_None defined yet. Add here after v0.3.0 is merged._
