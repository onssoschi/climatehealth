#' Code for calculating Diarrhea disease cases attributable to extreme 
#' precipitation and extreme temperature

#' Read in Relative Risk plot at country, Region, and District level
#'
#' @description Plots the relative risk of diarrhea cases by the maximum
#' temperature and cumulative rainfall at country, Region and District level
#'
#' @param data Data list from combine_health_climate_data() function.
#' @param model The fitted model from run_inla_models() function.
#' @param param_term A character vector or list containing parameter terms such
#' as "tmax" (maximum temperature) and "rainfall" (precipitation).
#' Default to "tmax".
#' @param level A character vector specifying the spatial disaggregation level.
#' Can take one of the following values: "country", "region", or "district".
#' Default to "country".
#' @param filter_year Integer. The year to filter to data to. Defaults to NULL.
#' @param output_dir Character. The path where the PDF file will be saved. Default to NULL.
#' @param save_csv Boolean. If TRUE, saves the RR data to the specified directory.
#' Defaults to FALSE.
#' @param save_fig Boolean. If TRUE, saves the plot to the specified directory.
#' Defaults to FALSE.
#'
#' @return Relative risk plot at country, region, and district levels.
#'
#' @export
plot_relative_risk <- function(data,
                               model,
                               param_term,
                               level = "country",
                               filter_year = NULL,
                               output_dir = NULL,
                               save_csv = FALSE,
                               save_fig = FALSE) {
  if (!"year" %in% names(data)) stop("'year' column not found in data.")
  if (is.null(filter_year)) filter_year <- sort(unique(data$year))

  level <- tolower(level)
  if (save_fig) {
    if (is.null(output_dir)) stop("output_dir must be provided if save_fig = TRUE")
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  }
  output_pdf <- if (save_fig) {
    file.path(output_dir, paste0("RR_", param_term, "_", level, "_all_plots.pdf"))
  } else {
    NULL
  }
  csv_output_path <- if (save_csv) {
    file.path(output_dir, paste0("RR_", param_term, "_", level, "_all_plots.csv"))
  } else {
    NULL
  }
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is required but not installed.")
  }
  build_plot <- function(pred, title) {
    if (anyNA(pred$allRRfit)) return(NULL)
    ggplot2::ggplot(
      dplyr::tibble(
        x = pred$predvar,
        y = pred$allRRfit,
        ymin = pred$allRRlow,
        ymax = pred$allRRhigh
      ),
      ggplot2::aes(x, y)
    ) +
      ggplot2::geom_line(color = "red", linewidth = 1) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = ymin, ymax = ymax), fill = "red", alpha = 0.3) +
      ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "gray", linewidth = 0.5) +
      ggplot2::labs(title = title, x = param_term, y = "Relative Risk") +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 9))
  }

  all_predictions <- list()

  if (level == "country") {
    if (is.null(filter_year)) {
      data_all <- data
      pred <- get_predictions(data_all, param_term, model, level)
      if (is.list(pred) && !is.null(names(pred)) && length(pred) == 1) {
        pred <- pred[[1]]
      }
      all_predictions[["All Years"]] <- pred

      x_breaks <- pretty(range(pred$predvar, na.rm = TRUE), n = 6)
      x_limits <- range(x_breaks)
      param_sym <- rlang::sym(param_term)

      # Get range of predvar where RR >= 1
      rr_above_1 <- pred$predvar[which(pred$allRRfit >= 1)]
      rr_range <- range(rr_above_1, na.rm = TRUE)

      rr_plot <- ggplot2::ggplot() +
        ggplot2::geom_line(
          data = dplyr::tibble(x = pred$predvar, y = pred$allRRfit),
          ggplot2::aes(x = x, y = y), color = "red", linewidth = 1) +
        ggplot2::geom_ribbon(
          data = dplyr::tibble(x = pred$predvar, ymin = pred$allRRlow, ymax = pred$allRRhigh),
          ggplot2::aes(x = x, ymin = ymin, ymax = ymax), fill = "red", alpha = 0.3) +
        ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "gray", linewidth = 0.5) +
        ggplot2::geom_vline(xintercept = rr_range, linetype = "dotted", color = "blue", linewidth = 0.8) +
        ggplot2::scale_x_continuous(limits = x_limits, breaks = x_breaks) +
        ggplot2::labs(title = "Relative Risk Curve", y = "Relative Risk") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_text(color = "gray"),
          plot.title = ggplot2::element_text(size = 11)
        )

      hist_counts <- ggplot2::ggplot_build(
        ggplot2::ggplot(data_all, ggplot2::aes(x = !!param_sym)) +
          ggplot2::geom_histogram(binwidth = 1, boundary = 0)
      )$data[[1]]
      max_count <- max(hist_counts$count, na.rm = TRUE)
      y_breaks <- pretty(c(0, max_count), n = 3)
      y_limits <- range(y_breaks)

      hist_plot <- ggplot2::ggplot(data_all, ggplot2::aes(x = !!param_sym)) +
        ggplot2::geom_histogram(binwidth = 1, boundary = 0, fill = "skyblue",
                                color = "black", alpha = 0.6) +
        ggplot2::scale_x_continuous(limits = x_limits, breaks = x_breaks) +
        ggplot2::scale_y_continuous(name = "Frequency", limits = y_limits,
                                    breaks = y_breaks, position = "right") +
        ggplot2::labs(x = param_term) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.title.y = ggplot2::element_text(color = "skyblue"))

      plot_all_years <- rr_plot / hist_plot +
        patchwork::plot_layout(heights = c(2, 1))

      if (save_fig && !is.null(output_pdf)) {
        grDevices::pdf(output_pdf, width = 10, height = 6)
        print(plot_all_years)
        grDevices::dev.off()
      }

      if (save_csv && !is.null(csv_output_path)) {
        utils::write.csv(
          dplyr::tibble(
            predvar = pred$predvar,
            allRRfit = pred$allRRfit,
            allRRlow = pred$allRRlow,
            allRRhigh = pred$allRRhigh
          ),
          csv_output_path, row.names = FALSE
        )
      }

      return(list(plots = plot_all_years, RR = all_predictions))
    }

    filter_year <- sort(unique(filter_year))
    plots <- lapply(filter_year, function(yr) {
      pred <- get_predictions(dplyr::filter(data, year == yr), param_term, model, level)
      all_predictions[[as.character(yr)]] <- pred
      build_plot(pred, as.character(yr))
    }) %>% purrr::keep(~ !is.null(.))

    if (save_fig && !is.null(output_pdf)) {
      grDevices::pdf(output_pdf, width = 14, height = 10)
      purrr::walk(plots, print)
      grDevices::dev.off()
    }

    if (save_csv && !is.null(csv_output_path)) {
      flat_df <- dplyr::bind_rows(lapply(names(all_predictions), function(yr) {
        df <- all_predictions[[yr]]
        dplyr::tibble(
          year = as.numeric(yr),
          predvar = df$predvar,
          allRRfit = df$allRRfit,
          allRRlow = df$allRRlow,
          allRRhigh = df$allRRhigh
        )
      }))
      utils::write.csv(flat_df, csv_output_path, row.names = FALSE)
    }
    return(list(
      plots = patchwork::wrap_plots(plots) +
        patchwork::plot_annotation(
          title = "Exposure-Response Curves by Country",
          subtitle = paste(param_term, "Years:", paste(filter_year, collapse = ", "))
        ),
      RR = all_predictions
    ))
  }
  if (level %in% c("region", "district")) {
    group_plots <- list()

    if (is.null(filter_year)) {
      preds <- get_predictions(data, param_term, model, level)
      all_predictions[["All Years"]] <- preds
      for (grp in names(preds)) {
        p <- build_plot(preds[[grp]], grp)
        if (!is.null(p)) {
          group_plots[[grp]] <- list(p)
        }
      }
    } else {
      for (yr in filter_year) {
        preds <- get_predictions(dplyr::filter(data, year == yr), param_term, model, level)
        all_predictions[[as.character(yr)]] <- preds
        for (grp in names(preds)) {
          p <- build_plot(preds[[grp]], paste0(grp, " (", yr, ")"))
          if (!is.null(p)) {
            group_plots[[grp]] <- c(group_plots[[grp]], list(p))
          }
        }
      }
    }
    if (save_fig && !is.null(output_pdf)) {
      grDevices::pdf(output_pdf, width = 12, height = 9)
      all_plots <- unlist(group_plots, recursive = FALSE)
      if (length(all_plots) > 0) {
        plot_pages <- split(all_plots, ceiling(seq_along(all_plots) / 6))
        for (page in plot_pages) {
          print(
            patchwork::wrap_plots(page, ncol = 2, nrow = 3) +
              patchwork::plot_annotation(
                title = paste("Exposure-Response Curves by", tools::toTitleCase(level)),
                subtitle = if (is.null(filter_year)) "All Years Combined" else paste(param_term, "Years:", paste(filter_year, collapse = ", "))
              )
          )
        }
      }
      grDevices::dev.off()
    }
    if (save_csv && !is.null(csv_output_path)) {
      flat_df <- dplyr::bind_rows(lapply(names(all_predictions), function(yr) {
        preds <- all_predictions[[yr]]
        dplyr::bind_rows(lapply(names(preds), function(grp) {
          df <- preds[[grp]]
          dplyr::tibble(
            year = yr,
            group = grp,
            predvar = df$predvar,
            allRRfit = df$allRRfit,
            allRRlow = df$allRRlow,
            allRRhigh = df$allRRhigh
          )
        }))
      }))
      utils::write.csv(flat_df, csv_output_path, row.names = FALSE)
    }

    return(list(plots = group_plots, RR = all_predictions))
  }
}


#' Attribution calculation for maximum temperature
#'
#' @description The attribution calculation uses the attrdl function from
#' Gasparini and DNLM package
#'
#' @param data Data list from combine_health_climate_data() function.
#' @param param_term A character vector or list containing parameter terms such
#' as "tmax" (maximum temperature) and "rainfall" (precipitation).
#' Default to "tmax"
#' @param model The fitted model from run_inla_models() function.
#' @param level Character. The spatial disaggregation level.
#' Can take one of the following values: "country", "region", or "district".
#' @param param_threshold Numeric. Threshold above which exposure is considered,
#' "attributable". Can take floats. Defaults to 1.
#' @param filter_year Integer. The year to filter to data to. Defaults to NULL.
#' @param output_dir Character. Directory path to save the output metrics if
#' save_csv = TRUE. Defaults to NULL.
#' @param save_csv Logical. Whether to save the generated attribution metrics to file.
#' Defaults to FALSE.
#'
#' @return Results containing the attributable number and fraction at the chosen
#' dissagregation level.
#'
#' @export
attribution_calculation <- function(data,
                                    param_term,
                                    model,
                                    level,
                                    param_threshold = 1,
                                    filter_year = NULL,
                                    output_dir = NULL,
                                    save_csv = FALSE) {

  level <- tolower(level)

  # Get model coefficients
  coef_mean <- model$summary.fixed$mean
  vcov_full <- model$misc$lincomb.derived.covariance.matrix
  indt <- grep(paste0("basis_", param_term), model$names.fixed)
  if (length(indt) == 0) stop("No terms for 'basis_", param_term, "' found in model.")

  # Filter years if needed
  if (!is.null(filter_year)) {
    stopifnot("year" %in% names(data), all(filter_year %in% unique(data$year)))
    data <- dplyr::filter(data, year %in% filter_year)
  }

  # Create INLA indices and basis matrices
  data <- create_inla_indices(data)
  basis_matrices <- set_cross_basis(data)

  # Function to compute metrics given prediction object
  compute_metrics_from_pred <- function(df, pred) {
    df <- df[!is.na(df[[param_term]]) & !is.na(df$diarrhea) & !is.na(df$tot_pop), ]
    if (nrow(df) == 0) return(NULL)

    ref_temp <- pred$predvar[which.min(abs(pred$allRRfit - 1))]

    # Safe interpolation
    rr_obs_fit  <- approx(pred$predvar, pred$allRRfit,  xout = df[[param_term]], rule = 2)$y
    rr_obs_low  <- approx(pred$predvar, pred$allRRlow,  xout = df[[param_term]], rule = 2)$y
    rr_obs_high <- approx(pred$predvar, pred$allRRhigh, xout = df[[param_term]], rule = 2)$y

    total_cases <- df$diarrhea
    tot_pop <- sum(df$tot_pop, na.rm = TRUE)

    get_metrics <- function(rr_obs) {
      valid <- which(rr_obs > param_threshold & !is.na(rr_obs))
      if (length(valid) == 0 || tot_pop == 0 || is.na(tot_pop)) return(c(0, 0, 0))  # Changed NA to 0
      af <- 1 - 1 / mean(rr_obs[valid])
      an <- af * sum(total_cases[valid], na.rm = TRUE)
      ar <- (an / tot_pop) * 1e5
      return(c(af, an, ar))
    }

    res_fit <- get_metrics(rr_obs_fit)
    res_low <- get_metrics(rr_obs_low)
    res_high <- get_metrics(rr_obs_high)

    return(list(
      MRT = round(ref_temp, 2),
      AR_Number = round(res_fit[2], 0),
      AR_Number_LCI = round(res_low[2], 0),
      AR_Number_UCI = round(res_high[2], 0),
      AR_Fraction = round(res_fit[1] * 100, 2),
      AR_Fraction_LCI = round(res_low[1] * 100, 2),
      AR_Fraction_UCI = round(res_high[1] * 100, 2),
      AR_per_100k = ceiling(res_fit[3]),
      AR_per_100k_LCI = ceiling(res_low[3]),
      AR_per_100k_UCI = ceiling(res_high[3])
    ))
  }

  # Grouping variables
  grp_vars <- switch(level,
                     "country" = c("year", "month"),
                     "region"  = c("region", "year", "month"),
                     "district"= c("region", "district", "year", "month"),
                     stop("Invalid level. Choose 'country', 'region', or 'district'."))

  # Compute predictions and metrics
  res <- data %>%
    dplyr::group_by(across(all_of(grp_vars))) %>%
    dplyr::group_split() %>%
    purrr::map_dfr(~{
      df_group <- .x

      mean_param <- switch(level,
                           "country"  = round(mean(data[[param_term]], na.rm = TRUE), 0),
                           "region"   = round(mean(df_group[[param_term]], na.rm = TRUE), 0),
                           "district" = round(mean(df_group[[param_term]], na.rm = TRUE), 0))

      pred <- tryCatch({
        dlnm::crosspred(basis_matrices[[param_term]],
                        coef = coef_mean[indt],
                        vcov = vcov_full[indt, indt],
                        model.link = "log",
                        bylag = 0.25,
                        cen = mean_param)
      }, error = function(e) NULL)

      if (is.null(pred)) return(NULL)

      r <- compute_metrics_from_pred(df_group, pred)
      if (is.null(r)) return(NULL)

      tibble::tibble(!!!df_group[1, grp_vars],
                     MRT = r$MRT,
                     AR_Number = r$AR_Number,
                     AR_Number_LCI = r$AR_Number_LCI,
                     AR_Number_UCI = r$AR_Number_UCI,
                     AR_Fraction = r$AR_Fraction,
                     AR_Fraction_LCI = r$AR_Fraction_LCI,
                     AR_Fraction_UCI = r$AR_Fraction_UCI,
                     AR_per_100k = r$AR_per_100k,
                     AR_per_100k_LCI = r$AR_per_100k_LCI,
                     AR_per_100k_UCI = r$AR_per_100k_UCI)
    })

  # Save if needed
  if (save_csv && !is.null(output_dir)) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    readr::write_csv(res, file.path(output_dir,
                                    paste0("attribution_", level, "_", param_term, ".csv")))
  }

  return(res)
}


#' Plot Attributable Metrics
#'
#' @description
#' This function generates plots of climate-attributable health metrics such as
#' the attributable number, fraction, and rate of diarrhea cases at different spatial
#' levels (country, region, or district). It supports filtering by year and can
#' optionally save the plots as PDF files.
#'
#' The function produces time series plots (if `filter_year` is specified) or aggregate
#' bar charts (if `filter_year` is `NULL`), depending on the level and user input.
#'
#' @param attr_data A data frame containing climate-health attribution results,
#' including columns such as `year`, `AR_Number`, `AR_Fraction`, `AR_per_100k`,
#' and their respective confidence intervals.
#' @param level Character. The spatial level at which to plot the results.
#' Must be one of `"country"`, `"region"`, or `"district"`. Defaults to "district".
#' @param metrics Character vector. Specifies which metrics to plot.
#' Options include `"AR_Number"` (attributable number),
#' `"AR_Fraction"` (attributable fraction), and
#' `"AR_per_100k"` (attributable rate per 100,000 population).
#' Multiple values can be passed. Defaults to c("AR_Number", "AR_Fraction",
#' "AR_per_100k").
#' @param filter_year Optional. Integer or vector of integers specifying the year(s)
#' to filter the dataset. If `NULL`, all years are aggregated. Defaults to FALSE.
#' @param param_term Character. The climate variable term used in the attribution
#' analysis (e.g., "tmax", "rainfall"). This is used for labeling the plot titles.
#' @param save_fig Logical. Whether to save the generated plots to file.
#' Defaults to FALSE.
#' @param output_dir Optional. Directory path to save the output plots if
#' `save_fig = TRUE`. Defaults to NULL.
#'
#' @return
#' A list of ggplot objects (or nested lists if the level is `"region"` or
#' `"district"` with no `filter_year`). If `save_fig = TRUE`,
#' the plots are also saved as PDF files in the specified `output_dir`.
#'
#' @details
#' - When `level = "country"` and `filter_year = NULL`, data is aggregated by
#' year and plotted as a time series.
#' - When `level = "region"` or `"district"` and `filter_year = NULL`,
#' the function returns a list of faceted bar plots showing top-level units,
#' split across multiple pages if necessary.
#' - When `filter_year` is provided, trends over time are shown for the specified
#' regions or districts.
#'
#' @export
plot_attribution_metric <- function(attr_data,
                                    level = c("country", "region", "district"),
                                    metrics = c("AR_Number", "AR_Fraction", "AR_per_100k"),
                                    filter_year = NULL,
                                    param_term,
                                    save_fig = FALSE,
                                    output_dir = NULL) {
  # Normalise and validate country+filter_year selection
  level <- tolower(level)
  if (level=="country" && !is.null(filter_year)) {
    warning("If level==country, filter_year must be NULL.")
    return(NULL)
  }
  metrics <- match.arg(metrics, several.ok = TRUE)

  if (is.null(param_term)) stop("'param_term' must be provided.")

  param_label <- switch(tolower(param_term),
                        tmax = "Extreme Temperature",
                        rainfall = "Extreme Rainfall",
                        param_term)

  if (!is.null(filter_year)) {
    if (!"year" %in% names(attr_data)) stop("'year' column not found in data.")
    attr_data <- dplyr::filter(attr_data, year %in% filter_year)
  }

  y_title_lookup <- c(
    AR_per_100k = "AR (per 100,000 population)",
    AR_Fraction = "Attributable Fraction (%)",
    AR_Number = "Attributable Number"
  )

  title_lookup <- c(
    AR_per_100k = paste0("Diarrhea cases per 100,000 attributable to ", param_label, " (95% CI)"),
    AR_Fraction = paste0("Diarrhea  Attributable Fraction (%) due to ", param_label, " (95% CI)"),
    AR_Number = paste0("Number of Diarrhea  cases attributable to ", param_label, " (95% CI)")
  )

  formatter_lookup <- list(
    AR_per_100k = scales::label_comma(),
    AR_Fraction = scales::label_comma(),
    AR_Number = scales::label_comma()
  )

  aggregate_attr_data <- function(data, group_var) {
    dplyr::group_by(data, .data[[group_var]]) %>%
      dplyr::summarise(
        across(matches("^AR_Number(_LCI|_UCI)?$"), ~ sum(.x, na.rm = TRUE)),
        across(matches("^AR_(Fraction|per_100k)(_LCI|_UCI)?$"), ~ mean(.x, na.rm = TRUE)),
        .groups = "drop"
      )
  }

  if (is.null(filter_year)) {
    if (level == "country") {
      attr_data <- aggregate_attr_data(attr_data, "year")
    } else if (level %in% c("region", "district")) {
      if (!(level %in% names(attr_data))) stop(paste0("'", level, "' column not found in data."))
      attr_data <- aggregate_attr_data(attr_data, level)
    }
  }

  plots <- purrr::map(metrics, function(metric) {
    lci_col <- paste0(metric, "_LCI")
    uci_col <- paste0(metric, "_UCI")
    x_var <- if (level %in% c("region", "district") && is.null(filter_year)) level else "year"

    required_cols <- c(x_var, metric, lci_col, uci_col)
    if (level %in% c("region", "district") && !is.null(filter_year)) {
      required_cols <- c(required_cols, level)
    }

    missing_cols <- setdiff(required_cols, names(attr_data))
    if (length(missing_cols) > 0) {
      warning("Skipping '", metric, "' due to missing columns: ", paste(missing_cols, collapse = ", "))
      return(NULL)
    }

    attr_data_plot <- attr_data %>%
      dplyr::filter(!is.na(.data[[metric]]),
                    !is.na(.data[[lci_col]]),
                    !is.na(.data[[uci_col]]))

    title <- title_lookup[[metric]]
    y_formatter <- formatter_lookup[[metric]]
    y_label <- y_title_lookup[[metric]]

    # Country-level time series when filter_year is NULL
    if (level == "country" && is.null(filter_year)) {
      attr_data_plot$year <- factor(attr_data_plot$year)
      p <- ggplot2::ggplot(attr_data_plot, ggplot2::aes(x = year, y = .data[[metric]], group = 1)) +
        ggplot2::geom_line(color = "steelblue", linewidth = 1) +
        ggplot2::geom_point(color = "steelblue", size = 2) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[[lci_col]], ymax = .data[[uci_col]]),
                    alpha = 0.2, fill = "steelblue") +
        ggplot2::labs(title = title, y = y_label, x = "Year") +
        ggplot2::scale_y_continuous(labels = y_formatter) +
        ggplot2::theme_minimal(base_size = 10) +
        ggplot2:: theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 12, face = "bold"))

      if (save_fig && !is.null(output_dir)) {
        if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
        ggsave(filename = file.path(output_dir, paste0("plot_", metric, "_", param_term, "_country.pdf")),
               plot = p, width = 8, height = 5)
      }
      return(p)
    }

    # region/district bar plot logic
    if (level %in% c("region", "district") && is.null(filter_year)) {
      attr_data_plot <- attr_data_plot %>%
        dplyr::arrange(dplyr::desc(.data[[metric]])) %>%
        dplyr::mutate(!!level := factor(.data[[level]], levels = unique(.data[[level]])))

      max_y <- max(attr_data_plot[[uci_col]], na.rm = TRUE)

      district_plots <- attr_data_plot %>%
        split(ceiling(seq_along(attr_data_plot[[level]]) / 30)) %>%
        purrr::map(~ {
          ggplot2::ggplot(.x, ggplot2::aes(x = .data[[level]], y = .data[[metric]])) +
            ggplot2::geom_col(fill = "steelblue", width= 0.6) +
            ggplot2::geom_errorbar(ggplot2::aes(ymin = .data[[lci_col]], ymax = .data[[uci_col]], color = "95% CI"),
                          width = 0.2) +
            ggplot2::coord_flip() +
            ggplot2::labs(x = tools::toTitleCase(level), y = y_label) +
            ggplot2::scale_y_continuous(labels = y_formatter, limits = c(0, max_y)) +
            ggplot2::scale_color_manual(name = "", values = c("95% CI" = "black")) +
            ggplot2::theme_minimal(base_size = 10) +
            ggplot2::theme(axis.text.y = ggplot2::element_text(size = 7),
                  plot.title = ggplot2::element_text(hjust = 0.5, size = 9))
        })

      if (save_fig && !is.null(output_dir)) {
        if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
        pdf_file <- file.path(output_dir, paste0("plot_", metric, "_", param_term, "_", level, ".pdf"))
        pdf(pdf_file, width = 11, height = 8)
        for (i in seq_along(district_plots)) {
          merged_plot <- patchwork::wrap_plots(district_plots[i], ncol = 1) +
            patchwork::plot_annotation(
              title = paste(title, "by", tools::toTitleCase(level)),
              theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 10, face = "bold", hjust = 0.5))
            )
          print(merged_plot)
        }
        dev.off()
      }
      return(district_plots)
    }

    # Region/district with multi-year grouped bar plot logic
    if (!is.null(filter_year) && length(filter_year) > 2 && level %in% c("region", "district")) {
      attr_data_plot <- attr_data_plot %>%
        group_by(.data[[level]], year) %>%
        summarise(
          across(matches("^AR_Number(_LCI|_UCI)?$"), ~ sum(.x, na.rm = TRUE)),
          across(matches("^AR_(Fraction|per_100k)(_LCI|_UCI)?$"), ~ mean(.x, na.rm = TRUE)),
          .groups = "drop")

      level_vals <- attr_data_plot %>%
        group_by(.data[[level]]) %>%
        summarise(avg = mean(.data[[metric]], na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(avg)) %>%
        pull(.data[[level]])

      attr_data_plot[[level]] <- factor(attr_data_plot[[level]], levels = level_vals)
      y_min <- min(attr_data_plot[[paste0(metric, "_LCI")]], na.rm = TRUE)
      y_max <- max(attr_data_plot[[paste0(metric, "_UCI")]], na.rm = TRUE)
      split_levels <- split(level_vals, ceiling(seq_along(level_vals) / 30))

      group_plots <- purrr::map(split_levels, function(subset_levels) {
        df <- filter(attr_data_plot, .data[[level]] %in% subset_levels)
        ggplot2::ggplot(df, ggplot2::aes(x = .data[[level]], y = .data[[metric]], fill = factor(year))) +
          ggplot2::geom_col(position = position_dodge(width = 0.8)) +
          ggplot2::geom_errorbar(
            ggplot2::aes(ymin = .data[[paste0(metric, "_LCI")]], ymax = .data[[paste0(metric, "_UCI")]],
                color = "95% CI"), position = position_dodge(0.8), width = 0.25) +
          ggplot2::scale_color_manual(name = "", values = c("95% CI" = "black")) +
          ggplot2::labs( x = tools::toTitleCase(level), y = y_label, fill = "Year") +
          ggplot2::theme_minimal(base_size = 8) +
          ggplot2::theme(axis.text.x = element_text(angle = 70, hjust = 1, size = 8),
                axis.text.y = ggplot2::element_text(size = 8),
                plot.margin = ggplot2::margin(t = 5, r = 5, b = 50, l = 5)) +
          ggplot2::coord_cartesian(ylim = c(y_min, y_max))
      })

      if (save_fig && !is.null(output_dir)) {
        if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
        pdf_file <- file.path(output_dir, paste0("plot_", metric, "_", param_term, "_", "Year_",level, ".pdf"))
        pdf(pdf_file, width = 11, height = 8)
        for (i in seq_along(group_plots)) {
          merged <- patchwork::wrap_plots(group_plots[i], ncol = 1) +
            patchwork::plot_annotation(
              title = paste(title, "by Year and", tools::toTitleCase(level)),
              theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 10, face = "bold", hjust = 0.5))
            )
          print(merged)
        }
        dev.off()
      }
      return(group_plots)
    }
  })

  return(plots)
}



#' Run Full Diarrhea-Climate Analysis Pipeline
#'
#' @description
#' The `diarrhea_do_analysis` function runs the complete analysis workflow
#' by combining multiple functions to analyze the association between diarrhea
#' cases and climate variables. It processes health, climate, and spatial data,
#' fits models, generates plots, and calculates attributable risk.
#'
#' @param health_data_path Data frame containing the processed health data.
#' @param climate_data_path Data frame containing the processed climate data.
#' @param map_path Data frame containing the spatial map data (shapefile or equivalent).
#' @param region_col Character. Name of the column containing region names.
#' @param district_col Character. Name of the column containing district names.
#' @param date_col Character. Name of the column containing the date. Defaults to NULL.
#' @param year_col Character. Name of the column containing the year.
#' @param month_col Character. Name of the column containing the month.
#' @param diarrhea_case_col Character. Name of the column containing diarrhea case counts.
#' @param tot_pop_col Character. Name of the column containing total population.
#' @param tmin_col Character. Name of the column containing minimum temperature.
#' @param tmean_col Character. Name of the column containing mean temperature.
#' @param tmax_col Character. Name of the column containing maximum temperature.
#' @param rainfall_col Character. Name of the column containing cumulative
#' monthly rainfall.
#' @param r_humidity_col Character. Name of the column containing relative humidity.
#' @param runoff_col Character. Name of the column containing monthly runoff
#' data.
#' @param geometry_col Character. Name of the geometry column in the shapefile
#' (usually "geometry").
#' @param spi_col Character. Name of the column containing the Standardized
#' Precipitation Index. Defaults to NULL.
#' @param max_lag Numeric. Maximum lag to consider in the model
#' (typically 2 to 4). Defaults to 2.
#' @param inla_param A character vector specifying the confounding exposures to
#' be included in the model. Possible values are "tmax","tmin", "rainfall",
#' "r_humidity", and "runoff".
#' @param basis_matrices_choices Character vector specifying basis matrix
#' parameters to include in the model (e.g., "tmax", "tmin", "rainfall",
#' "r_humidity", "spi").
#' @param param_term Character vector specifying the exposure variables of interest
#' (e.g., "tmax", "rainfall").
#' @param level Character. Spatial disaggregation level: "country", "region", or "district".
#' @param param_threshold Numeric. Threshold above which exposure is considered,
#' "attributable". Can take floats. Defaults to 1.
#' @param filter_year Integer. The year to filter to data to. Defaults to NULL.
#' @param family Character. The probability distribution for the response
#' variable. The user may also have thepossibility to choose "nbinomial" for a
#' negative binomial distribution. Defaults to "poisson".
#' @param config Boolean. Enable additional model configurations. Defaults to FALSE.
#' @param save_csv Boolean. If TRUE, saves the resultant datasets. Defaults to FALSE.
#' @param save_fig Boolean. If TRUE, saves the generated plots. Defaults to FALSE.
#' @param output_dir Character. The path to the directory where outputs
#' (e.g., plots, maps, datasets) should be saved.
#'
#' @return A list containing:
#' \itemize{
#'   \item Model output from INLA
#'   \item Monthly random effects plot
#'   \item Yearly random effects plot
#'   \item Contour plot
#'   \item Relative risk map
#'   \item Relative risk plot
#'   \item Attributable fraction and number summary
#' }
#'
#' @export
diarrhea_do_analysis <- function(health_data_path,
                                 climate_data_path,
                                 map_path,
                                 region_col,
                                 district_col,
                                 date_col= NULL,
                                 year_col,
                                 month_col,
                                 diarrhea_case_col,
                                 tot_pop_col,
                                 tmin_col,
                                 tmean_col,
                                 tmax_col,
                                 rainfall_col,
                                 r_humidity_col,
                                 runoff_col,
                                 geometry_col,
                                 spi_col = NULL,
                                 max_lag = 2,
                                 inla_param,
                                 basis_matrices_choices,
                                 param_term,
                                 level,
                                 param_threshold = 1,
                                 filter_year = NULL,
                                 family = "poisson",
                                 config = FALSE,
                                 save_csv = FALSE,
                                 save_fig = FALSE,
                                 output_dir = NULL){

  # Simple output validation
  if (is.null(output_dir) & (save_fig | save_csv)) {
    stop("'output_dir' must be provided if 'save_fig' or save_csv' are TRUE.")
  }
  check_file_exists(output_dir, TRUE)
  # level validation
  level <- tolower(level)
  acceptable_levels = c("country", "region", "district")
  if (!(level %in% acceptable_levels)) {
    stop(paste0("Level must be one of ", paste0(acceptable_levels, collapse=", ")))
  }

  # Input validation (IF makes API exception)
  if (is.character(health_data_path)) {
    check_file_exists(health_data_path, TRUE)
  }
  if (is.character(climate_data_path)) {
    check_file_exists(climate_data_path, TRUE)
  }
  check_file_exists(map_path, TRUE)
  # get combined data
  combined_data <- combine_health_climate_data(health_data_path,
                                               climate_data_path,
                                               map_path,
                                               region_col,
                                               district_col,
                                               date_col,
                                               year_col,
                                               month_col,
                                               diarrhea_case_col,
                                               tot_pop_col,
                                               tmin_col,
                                               tmean_col,
                                               tmax_col,
                                               rainfall_col,
                                               r_humidity_col,
                                               geometry_col,
                                               runoff_col,
                                               spi_col,
                                               max_lag,
                                               output_dir)

  #plot time series
  plot_diarrhea<-plot_health_climate_timeseries(
    combined_data$data,
    param_term= level,
    level = "country",
    filter_year = filter_year,
    save_fig = save_fig,
    output_dir = output_dir
  )
  plot_tmax<-plot_health_climate_timeseries(
    combined_data$data,
    param_term= "tmax",
    level = level,
    filter_year = filter_year,
    save_fig = save_fig,
    output_dir = output_dir
  )
  plot_rainfall<-plot_health_climate_timeseries(
    combined_data$data,
    param_term= "rainfall",
    level = level,
    filter_year = filter_year,
    save_fig = save_fig,
    output_dir = output_dir
  )

  # create base matrice
  basis <- set_cross_basis(combined_data$data)

  #check for multicolinearity
  VIF <- check_diarrhea_vif(
    data=combined_data$data,
    inla_param=inla_param,
    basis_matrices_choices=basis_matrices_choices
  )
  VIF$vif <- rbind(
    data.frame(VIF$vif),
    data.frame(
      row.names=c("condition_number", "interpretation"),
      VIF.vif=c(VIF$condition_number, VIF$interpretation)
    )
  )
  if (save_csv) {
    # Create FPATH
    fpath <- file.path(output_dir, "VIF_results.csv")
    file_connection <- file(fpath)
    # Create file contents and write to file
    write.csv(file=fpath, VIF$vif)
  }

  # fitting the model
  inla_result <- run_inla_models(combined_data,
                                 basis_matrices_choices,
                                 inla_param=inla_param,
                                 output_dir=output_dir,
                                 save_csv=save_csv,
                                 family=family,
                                 config=config)

  # seasonality plot
  reff_plot_monthly <- plot_monthly_random_effects(combined_data,
                                                   model=inla_result$model,
                                                   output_dir=output_dir,
                                                   save_fig=save_fig )

  # spatial random effect
  reff_plot_yearly <- plot_yearly_spatial_random_effect(combined_data,
                                                        model=inla_result$model,
                                                        output_dir=output_dir,
                                                        save_fig=save_fig)
  # contour plots
  contour_plot <- contour_plot(combined_data$data,
                               param_term,
                               model=inla_result$model,
                               level=level,
                               output_dir=output_dir,
                               save_fig=save_fig,
                               filter_year=filter_year)

  # rr map plots
  rr_map_plot <- plot_rr_map(combined_data,
                             param_term,
                             model=inla_result$model,
                             level="district",
                             filter_year=filter_year,
                             output_dir=output_dir,
                             save_fig=save_fig)

  # relative rist plot
  rr_data <- plot_relative_risk( combined_data$data,
                                 param_term=param_term,
                                 model=inla_result$model,
                                 level=level,
                                 filter_year=filter_year,
                                 output_dir=output_dir,
                                 save_csv=save_csv,
                                 save_fig=save_fig)
  rr_plot <- rr_data[["plots"]]
  rr_df <- rr_data[["RR"]]

  # attribution fraction and number
  attr_frac_num <- attribution_calculation(combined_data$data,
                                           param_term=param_term,
                                           model=inla_result$model,
                                           param_threshold=param_threshold,
                                           level= level,
                                           filter_year=filter_year,
                                           save_csv=save_csv,
                                           output_dir=output_dir)

  #AN, AF, and AR plot
  plot_AR_Num <-plot_attribution_metric(attr_data = attr_frac_num,
                                        level= level,
                                        metrics = "AR_Number",
                                        filter_year = filter_year,
                                        param_term=param_term,
                                        save_fig =save_fig,
                                        output_dir = output_dir)

  plot_AR_Fr <-plot_attribution_metric(attr_data = attr_frac_num,
                                       level= level,
                                       metrics = "AR_Fraction",
                                       filter_year = filter_year,
                                       param_term=param_term,
                                       save_fig =save_fig,
                                       output_dir = output_dir)

  plot_AR_per_100k <-plot_attribution_metric(attr_data = attr_frac_num,
                                             level= level,
                                             filter_year = filter_year,
                                             param_term=param_term,
                                             metrics = "AR_per_100k",
                                             save_fig =save_fig,
                                             output_dir = output_dir)

  res <- list(plot_diarhea, plot_tmax, plot_rainfall, VIF, inla_result,
              reff_plot_monthly, reff_plot_yearly, contour_plot, rr_map_plot,
              rr_plot, rr_df, attr_frac_num, plot_AR_Num,plot_AR_Fr,
              plot_AR_per_100k)

  return(res)
}
