# Internal Shiny server function

#' Build the Shiny server
#' @param init_design A `doe_design` object to pre-load, or `NULL`.
#' @noRd
app_server <- function(init_design = NULL) {
  function(input, output, session) {

    # Reactive state: current doe_design (possibly pre-loaded)
    rv <- shiny::reactiveValues(design = init_design)

    # -------------------------------------------------------------------------
    # Design info panel
    # -------------------------------------------------------------------------
    output$design_info <- shiny::renderText({
      d <- rv$design
      if (is.null(d)) return("No design loaded.")
      paste0(
        "Factors: ", paste(d$factors, collapse = ", "), "\n",
        "Levels : ", paste(d$levels, collapse = ", "), "\n",
        "Runs   : ", d$n_runs,
        if (!is.null(d$n_replicates) && d$n_replicates > 1L)
          paste0("\nReplicates: ", d$n_replicates) else ""
      )
    })

    # Flag: is a model currently fitted?
    output$model_fitted <- shiny::reactive({
      !is.null(rv$design) && !is.null(rv$design$model)
    })
    shiny::outputOptions(output, "model_fitted", suspendWhenHidden = FALSE)

    # -------------------------------------------------------------------------
    # Smart default for interactions selector
    # -------------------------------------------------------------------------
    shiny::observe({
      d <- rv$design
      if (is.null(d)) return()
      smart <- if (isTRUE(d$coded) &&
                   (is.null(d$n_replicates) || d$n_replicates == 1L))
                 "two_way" else "all"
      shiny::updateSelectInput(session, "interactions_level", selected = smart)
    })

    # -------------------------------------------------------------------------
    # CSV file upload: populate column selectors
    # -------------------------------------------------------------------------
    shiny::observeEvent(input$csv_file, {
      shiny::req(input$csv_file)
      df <- tryCatch(
        utils::read.csv(input$csv_file$datapath, stringsAsFactors = FALSE),
        error = function(e) {
          shiny::showNotification(paste("CSV read error:", e$message), type = "error")
          NULL
        }
      )
      if (is.null(df)) return()
      cols <- names(df)
      shiny::updateSelectizeInput(session, "csv_factor_cols",
                                  choices = cols, selected = NULL)
      shiny::updateSelectizeInput(session, "csv_response_cols",
                                  choices = cols, selected = NULL)
    })

    # -------------------------------------------------------------------------
    # Load from CSV button
    # -------------------------------------------------------------------------
    shiny::observeEvent(input$load_csv_btn, {
      shiny::req(input$csv_file)
      df <- tryCatch(
        utils::read.csv(input$csv_file$datapath, stringsAsFactors = FALSE),
        error = function(e) {
          shiny::showNotification(paste("CSV read error:", e$message), type = "error")
          NULL
        }
      )
      if (is.null(df)) return()

      mode <- input$upload_mode

      if (mode == "full") {
        # --- Full CSV mode: define design + response from CSV columns ---
        factor_cols   <- input$csv_factor_cols
        response_cols <- input$csv_response_cols

        if (length(factor_cols) == 0L) {
          shiny::showNotification("Select at least one factor column.", type = "warning")
          return()
        }
        if (length(response_cols) == 0L) {
          shiny::showNotification("Select at least one response column.", type = "warning")
          return()
        }

        # Build level_values from unique values in each factor column
        levels_vec <- vapply(factor_cols,
                             function(f) length(unique(df[[f]])), integer(1L))
        level_values_list <- lapply(factor_cols,
                                    function(f) sort(unique(df[[f]])))
        names(level_values_list) <- factor_cols

        # Build design matrix directly from CSV (preserves actual run order)
        dm <- df[, factor_cols, drop = FALSE]

        # Coded matrix for 2-level designs
        all_two_level <- all(levels_vec == 2L)
        cm <- if (all_two_level) {
          cm_df <- dm
          for (f in factor_cols) {
            lo <- min(level_values_list[[f]])
            hi <- max(level_values_list[[f]])
            cm_df[[f]] <- encode_to_coded(cm_df[[f]], lo, hi)
          }
          cm_df
        } else NULL

        dtype <- if (all_two_level) "full_factorial_2level" else "full_factorial_multilevel"

        new_design <- new_doe_design(
          factors       = factor_cols,
          levels        = levels_vec,
          level_values  = level_values_list,
          design_matrix = dm,
          coded_matrix  = cm,
          coded         = all_two_level,
          design_type   = dtype,
          n_replicates  = 1L   # unknown from CSV; conservative default
        )

        # Fit on first response column (notify if multiple selected)
        resp_col <- response_cols[1]
        if (length(response_cols) > 1L) {
          shiny::showNotification(
            paste("Multiple response columns detected. Fitting model on:", resp_col),
            type = "message"
          )
        }

        new_design <- tryCatch(
          fit_model(new_design,
                    response      = df[[resp_col]],
                    response_name = resp_col,
                    interactions  = input$interactions_level),
          error = function(e) {
            shiny::showNotification(paste("Model error:", e$message), type = "error")
            NULL
          }
        )
        if (is.null(new_design)) return()

        rv$design <- new_design

        # Update factor selectors in visualization tabs
        shiny::updateSelectInput(session, "main_factor",
                                 choices  = factor_cols,
                                 selected = factor_cols[1])
        shiny::updateCheckboxGroupInput(
          session, "int_factors",
          choices  = factor_cols,
          selected = factor_cols[seq_len(min(2L, length(factor_cols)))]
        )
        shiny::showNotification("CSV loaded and model fitted.", type = "message")

      } else {
        # --- Response-only mode: append response to existing design ---
        d <- rv$design
        if (is.null(d)) {
          shiny::showNotification(
            "No design loaded. Create or load a design first, then upload responses.",
            type = "error"
          )
          return()
        }

        # Find the first numeric column in the CSV
        numeric_cols <- names(df)[vapply(df, is.numeric, logical(1L))]
        if (length(numeric_cols) == 0L) {
          shiny::showNotification("No numeric columns found in CSV.", type = "error")
          return()
        }
        resp_col  <- numeric_cols[1]
        resp_vals <- df[[resp_col]]

        if (length(resp_vals) != d$n_runs) {
          shiny::showNotification(
            paste0("CSV has ", length(resp_vals), " rows but design has ",
                   d$n_runs, " runs."),
            type = "error"
          )
          return()
        }

        new_design <- tryCatch(
          fit_model(d,
                    response      = resp_vals,
                    response_name = resp_col,
                    interactions  = input$interactions_level),
          error = function(e) {
            shiny::showNotification(paste("Model error:", e$message), type = "error")
            NULL
          }
        )
        if (is.null(new_design)) return()

        rv$design <- new_design
        shiny::showNotification("Response loaded and model fitted.", type = "message")
      }
    })

    # -------------------------------------------------------------------------
    # Fit model from pasted response
    # -------------------------------------------------------------------------
    shiny::observeEvent(input$fit_btn, {
      d <- rv$design
      if (is.null(d)) {
        shiny::showNotification("No design loaded.", type = "error")
        return()
      }

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

      new_design <- tryCatch(
        fit_model(d,
                  response      = vals,
                  response_name = trimws(input$response_name),
                  interactions  = input$interactions_level),
        error = function(e) {
          shiny::showNotification(paste("Model error:", e$message), type = "error")
          NULL
        }
      )
      if (is.null(new_design)) return()
      rv$design <- new_design
      shiny::showNotification("Model fitted successfully.", type = "message")
    })

    # -------------------------------------------------------------------------
    # Shared reactive: fitted design
    # -------------------------------------------------------------------------
    fitted_design <- shiny::reactive({
      shiny::req(!is.null(rv$design$model))
      rv$design
    })

    # -------------------------------------------------------------------------
    # Visualizations
    # -------------------------------------------------------------------------

    output$all_main_effects_plot <- shiny::renderPlot({
      d <- fitted_design()
      plot_all_main_effects(d)
    })

    output$main_effects_plot <- shiny::renderPlot({
      d <- fitted_design()
      shiny::req(input$main_factor %in% d$factors)
      plot_main_effects(d, input$main_factor)
    })

    output$interaction_plot <- shiny::renderPlot({
      d    <- fitted_design()
      fsel <- input$int_factors
      shiny::req(
        length(fsel) >= 2L,
        length(fsel) <= 4L,
        all(fsel %in% d$factors),
        length(unique(fsel)) == length(fsel)
      )
      plot_interaction(
        d,
        factor1 = fsel[1],
        factor2 = fsel[2],
        factor3 = if (length(fsel) >= 3L) fsel[3] else NULL,
        factor4 = if (length(fsel) >= 4L) fsel[4] else NULL
      )
    })

    output$pareto_plot <- shiny::renderPlot({
      d <- fitted_design()
      withCallingHandlers(
        plot_pareto(d, alpha = input$pareto_alpha),
        warning = function(w) {
          shiny::showNotification(conditionMessage(w), type = "warning", duration = 8)
          invokeRestart("muffleWarning")
        }
      )
    })

    output$model_summary_text <- shiny::renderPrint({
      d  <- fitted_design()
      ms <- model_summary(d)

      if (isTRUE(ms$is_saturated)) {
        cat("WARNING: Model is saturated (df_residual = 0).\n")
        cat("Standard errors and p-values are undefined.\n")
        cat("Fix: use 'Two-way only' or 'Main effects only' interactions,\n")
        cat("     or add replicates to the design.\n\n")
      }

      cat(sprintf("R\u00b2            : %.4f\n", ms$r_squared))
      cat(sprintf("Adj. R\u00b2      : %.4f\n", ms$adj_r_squared))
      cat(sprintf("RMSE          : %.4f\n", ms$rmse))
      cat(sprintf("F-statistic   : %.4f (p = %.4g)\n", ms$f_statistic, ms$f_p_value))
      cat(sprintf("Runs / df_res : %d / %d\n\n", ms$n_runs, ms$degrees_of_freedom))
      cat("Effects table:\n")
      print(ms$effects_table, digits = 4, row.names = FALSE)
    })

    # -------------------------------------------------------------------------
    # Optimization
    # -------------------------------------------------------------------------
    shiny::observeEvent(input$opt_btn, {
      d <- rv$design
      shiny::req(!is.null(d$model))
      tgt <- if (input$opt_goal == "target") input$opt_target else NULL
      res <- tryCatch(
        optimize_response(d, goal = input$opt_goal, target = tgt),
        error = function(e) {
          shiny::showNotification(e$message, type = "error")
          NULL
        }
      )
      if (!is.null(res)) {
        output$opt_result <- shiny::renderPrint({
          cat("Goal:", res$goal, "\n")
          if (!is.null(res$target)) cat("Target:", res$target, "\n")
          cat("Predicted response:", format(res$predicted_response, digits = 3), "\n\n")
          cat("Optimal settings:\n")
          for (nm in names(res$optimal_settings)) {
            cat(sprintf("  %-20s %s\n", nm, res$optimal_settings[[nm]]))
          }
        })
      }
    })
  }
}
