# NormicDoE

An R package for designing and analysing Design of Experiments (DoE) setups.

## Installation

```r
# Install from source
devtools::install_local(".")
```

## Quick Start

```r
library(NormicDoE)

# 1. Create a 2^3 full factorial design
d <- full_factorial(
  factors      = c("Temperature", "pH", "Catalyst"),
  levels       = c(2, 2, 2),
  level_values = list(
    Temperature = c(60, 80),
    pH          = c(5, 7),
    Catalyst    = c(0.1, 0.5)
  )
)
print(d)
# Design of Experiments: Full Factorial
#   Type   : full_factorial_2level
#   Factors: 3
#   Runs   : 8

# 2. Export design template to CSV
export_design(d, file = "my_design.csv")

# 3. After running experiments, add responses and fit model
responses <- c(42, 55, 48, 61, 45, 57, 51, 68)
d <- fit_model(d, response = responses, response_name = "Yield")

# 4. Extract effects (half-effects for 2-level coded designs)
extract_effects(d, type = "half")

# 5. Model summary (R², RMSE, F-statistic, effects table)
model_summary(d)

# 6. Optimization
optimize_response(d, goal = "max")
optimize_response(d, goal = "target", target = 55)

# 7. Interactive Shiny visualization
launch_app(d)
```

## Shiny App

`launch_app(d)` opens a browser-based app with:

- **Main Effects** — mean response at each factor level
- **Interactions** — interaction plots for any two factors
- **Pareto Diagram** — ranked standardized effects with significance reference line
- **Model Summary** — R², RMSE, coefficients, p-values

## Functions

| Function | Description |
|---|---|
| `full_factorial()` | Create a full factorial design matrix |
| `export_design()` | Export design to `.csv` |
| `fit_model()` | Fit a linear model to experimental results |
| `extract_effects()` | Extract half-effects or standardized effects |
| `model_summary()` | R², RMSE, F-statistic, effects table |
| `optimize_response()` | Find optimal factor settings (max/min/target) |
| `launch_app()` | Open interactive Shiny visualization |
