#' NormicDoE: Design of Experiments for Process Optimization
#'
#' Provides tools for creating and analyzing full factorial experimental
#' designs. Supports design matrix generation, linear model fitting,
#' effect extraction, response optimization, and an interactive Shiny app
#' for visualizing main effects, interaction plots, and Pareto diagrams.
#'
#' ## Main workflow
#'
#' 1. **Create a design** with [full_factorial()].
#' 2. **Export** the design template with [export_design()].
#' 3. **Fit a model** after collecting responses with [fit_model()].
#' 4. **Inspect effects** with [extract_effects()] and [model_summary()].
#' 5. **Optimize** the response with [optimize_response()].
#' 6. **Visualize** everything interactively with [launch_app()].
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom bslib bs_theme
#' @importFrom ggplot2 ggplot aes geom_bar geom_line geom_point geom_vline
#'   geom_hline geom_text coord_flip labs theme_bw theme element_blank element_text
#'   margin scale_color_discrete scale_fill_manual scale_x_continuous scale_y_continuous
#'   facet_wrap facet_grid position_dodge position_jitter geom_errorbar
#' @importFrom grid unit
#' @importFrom gt gt tab_header cols_label fmt_number tab_style cell_text
#'   cells_body sub_missing gt_output render_gt
#' @importFrom plotly ggplotly renderPlotly plotlyOutput
#' @importFrom shiny shinyApp fluidPage titlePanel sidebarLayout sidebarPanel
#'   mainPanel tabsetPanel tabPanel selectInput selectizeInput sliderInput
#'   numericInput textAreaInput textInput actionButton uiOutput observeEvent
#'   observe reactive reactiveValues req outputOptions conditionalPanel
#'   showNotification fluidRow column h4 hr br fileInput checkboxInput
#'   radioButtons updateSelectInput updateSelectizeInput
#' @importFrom stats lm as.formula coef qt optim pf predict aggregate setNames
#' @importFrom utils write.csv head combn read.csv
## usethis namespace: end
NULL
