# Internal Shiny server function

#' Build the Shiny server
#' @param init_design A `doe_design` object to pre-load, or `NULL`.
#' @noRd
app_server <- function(init_design = NULL) {
  function(input, output, session) {

    # Reactive: current doe_design (possibly pre-loaded)
    rv <- shiny::reactiveValues(design = init_design)

    # Design info panel
    output$design_info <- shiny::renderText({
      d <- rv$design
      if (is.null(d)) return("No design loaded.")
      paste0(
        "Factors: ", paste(d$factors, collapse = ", "), "\n",
        "Levels : ", paste(d$levels, collapse = ", "), "\n",
        "Runs   : ", d$n_runs
      )
    })

    # Flag: is a model currently fitted?
    output$model_fitted <- shiny::reactive({
      !is.null(rv$design) && !is.null(rv$design$model)
    })
    shiny::outputOptions(output, "model_fitted", suspendWhenHidden = FALSE)

    # Fit model on button click
    shiny::observeEvent(input$fit_btn, {
      d <- rv$design
      if (is.null(d)) {
        shiny::showNotification("No design loaded.", type = "error")
        return()
      }

      # Parse response values
      raw <- trimws(input$response_input)
      if (nchar(raw) == 0) {
        shiny::showNotification("Please paste response values.", type = "warning")
        return()
      }
      vals <- tryCatch(
        as.numeric(strsplit(raw, "[,\n]+")[[1]]),
        warning = function(w) NULL
      )
      if (is.null(vals) || any(is.na(vals))) {
        shiny::showNotification(
          "Could not parse response values. Use comma- or newline-separated numbers.",
          type = "error"
        )
        return()
      }
      if (length(vals) != d$n_runs) {
        shiny::showNotification(
          paste0("Expected ", d$n_runs, " values, got ", length(vals), "."),
          type = "error"
        )
        return()
      }

      rv$design <- fit_model(d, response = vals,
                             response_name = trimws(input$response_name))
      shiny::showNotification("Model fitted successfully.", type = "message")
    })

    # Shared reactive: fitted design (after model is fitted)
    fitted_design <- shiny::reactive({
      shiny::req(!is.null(rv$design$model))
      rv$design
    })

    # Main effects plot
    output$main_effects_plot <- shiny::renderPlot({
      d <- fitted_design()
      shiny::req(input$main_factor %in% d$factors)
      plot_main_effects(d, input$main_factor)
    })

    # Interaction plot
    output$interaction_plot <- shiny::renderPlot({
      d <- fitted_design()
      f1 <- input$int_factor1
      f2 <- input$int_factor2
      shiny::req(f1 %in% d$factors, f2 %in% d$factors, f1 != f2)
      plot_interaction(d, f1, f2)
    })

    # Pareto plot
    output$pareto_plot <- shiny::renderPlot({
      d <- fitted_design()
      plot_pareto(d, alpha = input$pareto_alpha)
    })

    # Model summary text
    output$model_summary_text <- shiny::renderPrint({
      d <- fitted_design()
      ms <- model_summary(d)
      cat(sprintf("R\u00b2            : %.4f\n", ms$r_squared))
      cat(sprintf("Adj. R\u00b2      : %.4f\n", ms$adj_r_squared))
      cat(sprintf("RMSE          : %.4f\n", ms$rmse))
      cat(sprintf("F-statistic   : %.4f (p = %.4g)\n", ms$f_statistic, ms$f_p_value))
      cat(sprintf("Runs / df_res : %d / %d\n\n", ms$n_runs, ms$degrees_of_freedom))
      cat("Effects table:\n")
      print(ms$effects_table, digits = 4, row.names = FALSE)
    })

    # Optimization
    shiny::observeEvent(input$opt_btn, {
      d <- rv$design
      shiny::req(!is.null(d$model))
      tgt <- if (input$opt_goal == "target") input$opt_target else NULL
      res <- tryCatch(
        optimize_response(d, goal = input$opt_goal, target = tgt),
        error = function(e) { shiny::showNotification(e$message, type = "error"); NULL }
      )
      if (!is.null(res)) {
        output$opt_result <- shiny::renderPrint({
          cat("Goal:", res$goal, "\n")
          if (!is.null(res$target)) cat("Target:", res$target, "\n")
          cat("Predicted response:", round(res$predicted_response, 4), "\n\n")
          cat("Optimal settings:\n")
          for (nm in names(res$optimal_settings)) {
            cat(sprintf("  %-20s %.4f\n", nm, res$optimal_settings[[nm]]))
          }
        })
      }
    })
  }
}
