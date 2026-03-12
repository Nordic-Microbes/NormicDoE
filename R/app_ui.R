# Internal Shiny UI definition

#' Build the Shiny UI
#' @param factors Character vector of factor names (used to populate selectors).
#' @noRd
app_ui <- function(factors = character(0)) {
  shiny::fluidPage(
    shiny::titlePanel("NormicDoE \u2014 Design of Experiments Analysis"),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,

        # --- Design Info ---
        shiny::h4("Design Info"),
        shiny::verbatimTextOutput("design_info"),
        shiny::hr(),

        # --- Load Data ---
        shiny::h4("Load Data"),
        shiny::radioButtons(
          inputId  = "upload_mode",
          label    = "Upload mode:",
          choices  = c(
            "Full CSV (factors + response)"    = "full",
            "Response only (append to design)" = "response_only"
          ),
          selected = "full"
        ),
        shiny::fileInput(
          inputId     = "csv_file",
          label       = "Upload CSV:",
          accept      = ".csv",
          placeholder = "No file selected"
        ),
        shiny::conditionalPanel(
          condition = "input.upload_mode == 'full'",
          shiny::checkboxGroupInput(
            inputId  = "csv_factor_cols",
            label    = "Factor columns:",
            choices  = character(0)
          ),
          shiny::checkboxGroupInput(
            inputId  = "csv_response_cols",
            label    = "Response column(s):",
            choices  = character(0)
          )
        ),
        shiny::actionButton("load_csv_btn", "Load from CSV", class = "btn-info"),
        shiny::hr(),

        # --- Paste Response ---
        shiny::h4("Paste Response"),
        shiny::textAreaInput(
          inputId     = "response_input",
          label       = "Response values (comma- or newline-separated):",
          placeholder = "e.g. 42, 55, 48, 61, ...",
          rows        = 4
        ),
        shiny::textInput(
          inputId = "response_name",
          label   = "Response name",
          value   = "y"
        ),
        shiny::selectInput(
          inputId  = "interactions_level",
          label    = "Model interactions:",
          choices  = c(
            "All"                = "all",
            "Two-way only"       = "two_way",
            "Main effects only"  = "none"
          ),
          selected = "two_way"
        ),
        shiny::actionButton("fit_btn", "Fit Model", class = "btn-primary"),
        shiny::hr(),

        # --- Optimization (shown only after model is fitted) ---
        shiny::conditionalPanel(
          condition = "output.model_fitted",
          shiny::h4("Optimization"),
          shiny::selectInput(
            inputId = "opt_goal",
            label   = "Goal",
            choices = c("Maximize" = "max", "Minimize" = "min", "Target" = "target")
          ),
          shiny::conditionalPanel(
            condition = "input.opt_goal == 'target'",
            shiny::numericInput("opt_target", "Target value", value = 0)
          ),
          shiny::actionButton("opt_btn", "Optimize", class = "btn-success"),
          shiny::verbatimTextOutput("opt_result")
        )
      ),

      shiny::mainPanel(
        width = 9,
        shiny::tabsetPanel(
          id = "tabs",
          shiny::tabPanel(
            "Main Effects",
            shiny::br(),
            shiny::radioButtons(
              inputId  = "main_view",
              label    = NULL,
              choices  = c("Overview (all factors)" = "overview",
                           "Single factor"          = "single"),
              inline   = TRUE
            ),
            shiny::conditionalPanel(
              condition = "input.main_view == 'overview'",
              shiny::plotOutput("all_main_effects_plot", height = "400px")
            ),
            shiny::conditionalPanel(
              condition = "input.main_view == 'single'",
              shiny::selectInput(
                inputId = "main_factor",
                label   = "Select factor:",
                choices = factors
              ),
              shiny::plotOutput("main_effects_plot", height = "400px")
            )
          ),
          shiny::tabPanel(
            "Interactions",
            shiny::br(),
            shiny::checkboxGroupInput(
              inputId  = "int_factors",
              label    = "Select 2\u20134 factors (1st = x-axis, 2nd = lines, 3rd = facet, 4th = facet grid):",
              choices  = factors,
              selected = factors[seq_len(min(2L, length(factors)))]
            ),
            shiny::plotOutput("interaction_plot", height = "450px")
          ),
          shiny::tabPanel(
            "Pareto Diagram",
            shiny::br(),
            shiny::sliderInput(
              inputId = "pareto_alpha",
              label   = "Significance level (\u03b1):",
              min = 0.01, max = 0.20, value = 0.05, step = 0.01
            ),
            shiny::plotOutput("pareto_plot", height = "450px")
          ),
          shiny::tabPanel(
            "Model Summary",
            shiny::br(),
            shiny::verbatimTextOutput("model_summary_text")
          )
        )
      )
    )
  )
}
