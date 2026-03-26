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
        shiny::updateSelectizeInput(
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

    output$all_main_effects_plot <- plotly::renderPlotly({
      p <- plot_all_main_effects(fitted_design())
      plotly::ggplotly(p, tooltip = "text")
    })

    output$main_effects_plot <- plotly::renderPlotly({
      d <- fitted_design()
      shiny::req(input$main_factor %in% d$factors)
      p <- plot_main_effects(d, input$main_factor)
      plotly::ggplotly(p, tooltip = "text")
    })

    # -------------------------------------------------------------------------
    # Interactions: role-selector UI
    # -------------------------------------------------------------------------
    output$int_role_selectors <- shiny::renderUI({
      fsel <- input$int_factors
      shiny::req(length(fsel) >= 2L)
      row1 <- shiny::fluidRow(
        shiny::column(6, shiny::selectInput("int_role_x",    "X-axis:",       choices = fsel, selected = fsel[1])),
        shiny::column(6, shiny::selectInput("int_role_line", "Line/color:",   choices = fsel, selected = fsel[2]))
      )
      if (length(fsel) <= 2L) return(row1)
      row2 <- shiny::fluidRow(
        shiny::column(6, shiny::selectInput("int_role_facet", "Facet (rows):", choices = fsel, selected = fsel[3])),
        if (length(fsel) >= 4L)
          shiny::column(6, shiny::selectInput("int_role_grid", "Facet (cols):", choices = fsel, selected = fsel[4]))
      )
      shiny::tagList(row1, row2)
    })

    output$interaction_plot <- plotly::renderPlotly({
      d  <- fitted_design()
      f1 <- input$int_role_x
      f2 <- input$int_role_line
      shiny::req(
        length(input$int_factors) >= 2L,
        !is.null(f1), !is.null(f2),
        f1 %in% d$factors, f2 %in% d$factors,
        f1 != f2
      )
      f3 <- if (!is.null(input$int_role_facet) &&
                  input$int_role_facet %in% d$factors &&
                  input$int_role_facet != f1 &&
                  input$int_role_facet != f2)
              input$int_role_facet else NULL
      f4 <- if (!is.null(f3) &&
                  !is.null(input$int_role_grid) &&
                  input$int_role_grid %in% d$factors &&
                  input$int_role_grid != f1 &&
                  input$int_role_grid != f2 &&
                  input$int_role_grid != f3)
              input$int_role_grid else NULL
      p    <- plot_interaction(d, factor1 = f1, factor2 = f2, factor3 = f3, factor4 = f4)
      p_pl <- plotly::ggplotly(p, tooltip = "text")

      # Deduplicate legend entries across facet panels.
      # ggplotly names faceted traces "(color_val, facet_idx)" — extract just
      # the color part (before the first comma) as the dedup key.  Set a shared
      # legendgroup per color level so clicking the legend item toggles all
      # related traces (lines + points).  Hide pure-line traces so the legend
      # symbol is always the point marker.
      seen_keys <- character(0)
      for (i in seq_along(p_pl$x$data)) {
        tr   <- p_pl$x$data[[i]]
        nm   <- if (is.null(tr$name)) "" else tr$name
        key  <- trimws(sub(",.*", "", gsub("[()]", "", nm)))
        mode <- if (is.null(tr$mode)) "" else tr$mode

        if (nzchar(key)) p_pl$x$data[[i]]$legendgroup <- key

        is_line_only <- nzchar(mode) && grepl("lines", mode) && !grepl("markers", mode)
        if (is_line_only || key == "") {
          p_pl$x$data[[i]]$showlegend <- FALSE
        } else if (key %in% seen_keys) {
          p_pl$x$data[[i]]$showlegend <- FALSE
        } else {
          seen_keys <- c(seen_keys, key)
          p_pl$x$data[[i]]$name <- key
        }
      }
      p_pl
    })

    output$pareto_plot <- plotly::renderPlotly({
      d <- fitted_design()
      p <- withCallingHandlers(
        plot_pareto(d, alpha = input$pareto_alpha),
        warning = function(w) {
          shiny::showNotification(conditionMessage(w), type = "warning", duration = 8)
          invokeRestart("muffleWarning")
        }
      )
      plotly::ggplotly(p, tooltip = "text")
    })

    # -------------------------------------------------------------------------
    # Model Summary — gt table
    # -------------------------------------------------------------------------
    output$model_summary_stats <- shiny::renderUI({
      ms <- model_summary(fitted_design())
      shiny::tagList(
        if (isTRUE(ms$is_saturated))
          shiny::div(
            class = "alert alert-warning",
            "Model is saturated (df_residual = 0). Standard errors and p-values are undefined. ",
            "Use 'Two-way only' or 'Main effects only' interactions, or add replicates."
          ),
        shiny::p(sprintf(
          "R\u00b2 = %.4f  |  Adj R\u00b2 = %.4f  |  RMSE = %.4f  |  F = %.4f (p = %.4g)  |  Runs / df_res = %d / %d",
          ms$r_squared, ms$adj_r_squared, ms$rmse,
          ms$f_statistic, ms$f_p_value,
          ms$n_runs, ms$degrees_of_freedom
        ))
      )
    })

    output$model_summary_gt <- gt::render_gt({
      ms  <- model_summary(fitted_design())
      tbl <- ms$effects_table
      gt::gt(tbl) |>
        gt::tab_header(title = "Effects Table") |>
        gt::cols_label(
          term      = "Term",
          effect    = "Effect",
          std_error = "Std Error",
          t_value   = "t-value",
          p_value   = "p-value",
          significant = "Sig."
        ) |>
        gt::fmt_number(columns = c("effect", "std_error", "t_value"), decimals = 4) |>
        gt::fmt_number(columns = "p_value", decimals = 4) |>
        gt::sub_missing(missing_text = "\u2014") |>
        gt::tab_style(
          style     = gt::cell_text(weight = "bold"),
          locations = gt::cells_body(rows = significant == TRUE)
        )
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

    # -------------------------------------------------------------------------
    # Comparison — fold change relative to reference
    # -------------------------------------------------------------------------
    output$reference_inputs <- shiny::renderUI({
      d <- rv$design
      shiny::req(!is.null(d$model))
      n      <- length(d$factors)
      n_cols <- max(1L, ceiling(sqrt(n)))
      col_w  <- 12L %/% n_cols

      rows_list <- split(d$factors, ceiling(seq_along(d$factors) / n_cols))
      lapply(rows_list, function(row_factors) {
        shiny::fluidRow(
          lapply(row_factors, function(f) {
            shiny::column(col_w,
              shiny::selectInput(
                inputId  = paste0("ref_factor_", f),
                label    = f,
                choices  = as.character(d$level_values[[f]]),
                selected = as.character(d$level_values[[f]][[1L]])
              )
            )
          })
        )
      })
    })

    output$fold_change_plot <- plotly::renderPlotly({
      d <- rv$design
      shiny::req(!is.null(d$model))
      ref_parts <- vapply(d$factors, function(f) {
        val <- input[[paste0("ref_factor_", f)]]
        shiny::req(!is.null(val))
        paste0(f, "=", val)
      }, character(1L))
      reference_label <- paste(ref_parts, collapse = ", ")
      fc <- tryCatch(
        compute_fold_changes(d, reference_label),
        error = function(e) {
          shiny::showNotification(e$message, type = "error")
          NULL
        }
      )
      shiny::req(!is.null(fc))
      show_y <- if (is.null(input$fc_show_y_labels)) TRUE else input$fc_show_y_labels
      p <- plot_fold_changes(fc, show_y_labels = show_y)
      plotly::ggplotly(p, tooltip = "text")
    })
  }
}
