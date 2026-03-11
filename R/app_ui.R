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
        shiny::h4("Design Info"),
        shiny::verbatimTextOutput("design_info"),
        shiny::hr(),
        shiny::h4("Response Data"),
        shiny::textAreaInput(
          inputId     = "response_input",
          label       = "Paste response values (comma- or newline-separated):",
          placeholder = "e.g. 42, 55, 48, 61, ...",
          rows        = 5
        ),
        shiny::textInput(
          inputId = "response_name",
          label   = "Response name",
          value   = "y"
        ),
        shiny::actionButton("fit_btn", "Fit Model", class = "btn-primary"),
        shiny::hr(),
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
            shiny::selectInput(
              inputId = "main_factor",
              label   = "Select factor:",
              choices = factors
            ),
            shiny::plotOutput("main_effects_plot", height = "400px")
          ),
          shiny::tabPanel(
            "Interactions",
            shiny::br(),
            shiny::fluidRow(
              shiny::column(6,
                shiny::selectInput("int_factor1", "Factor 1 (x-axis):", choices = factors)
              ),
              shiny::column(6,
                shiny::selectInput("int_factor2", "Factor 2 (lines):", choices = factors)
              )
            ),
            shiny::plotOutput("interaction_plot", height = "400px")
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
