# Shared functions across the temperature and mental health indicators

#' Create population totals
#'
#' @description Creates a list of population totals by year and region for use
#' in the attributable rate calculations.
#'
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param country Character. Name of country for national level estimates.
#' @param meta_analysis Boolean. Whether to perform a meta-analysis.
#'
#' @returns List of population totals by year and region
#'
#' @keywords internal
dlnm_pop_totals <- function(
    df_list,
    country = "National",
    meta_analysis = FALSE) {
  # create list of population aggregates
  pop_list <- lapply(
    df_list, function(x) aggregate(population ~ year, data = x, mean)
  )
  if (meta_analysis == TRUE) {
    tot_pop <- do.call(rbind, pop_list)
    tot_pop <- aggregate(population ~ year, data = tot_pop, sum)
    pop_list[[country]] <- tot_pop
  }
  return(pop_list)
}

#' Produce variance inflation factor
#'
#' @description Produces variance inflation factor for the independent variables.
#'
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param independent_cols Additional independent variables to test in model validation.
#'
#' @returns A list. Variance inflation factors for each independent variables by region.
#'
#' @keywords internal
dlnm_vif <- function(
    df_list,
    independent_cols = NULL) {
  # determine formula var
  col_name <- ifelse(suicides %in% colnames(df_list[[1]]), "suicides", "dependent")
  # get all combinations
  all_combos <- unlist(lapply(1:length(independent_cols), function(i) {
    combn(independent_cols, i, simplify = FALSE)
  }), recursive = FALSE)
  # calculate VIF for each region
  vif_list <- list()
  for (reg in names(df_list)) {
    region_data <- df_list[[reg]]
    reg_vif <- list()
    for (combo in all_combos) {
      # define model formula and calculate VIF
      formula_str <- paste(paste(col_name, "~ temp"), paste(combo, collapse = " + "), sep = " + ")
      vif_model <- glm(as.formula(formula_str), data = region_data, family = quasipoisson())
      vif_values <- car::vif(vif_model)
      var_combo <- paste0("temp_", paste(combo, collapse = "_"))
      vif_df <- data.frame(
        region = reg,
        variable_combo = var_combo,
        variable = names(vif_values),
        vif = as.numeric(vif_values),
        stringsAsFactors = FALSE
      )
      reg_vif[[var_combo]] <- vif_df
    }
    vif_list[[reg]] <- do.call(rbind, reg_vif)
  }
  return(vif_list)
}

#' Reduce to overall cumulative
#'
#' @description Reduce model to the overall cumulative association
#'
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param var_per Vector. Internal knot positions for argvar
#' (see dlnm::crossbasis). Defaults to c(25, 50, 75).
#' @param var_degree Integer. Degree of the piecewise polynomial for argvar
#' (see dlnm::crossbasis). Defaults to 2 (quadratic).
#' @param cenper Integer. Value for the percentile in calculating the centering
#' value 0-100. Defaults to 50.
#' @param cb_list List of cross_basis matrices from create_crossbasis function.
#' @param model_list List of models produced from case-crossover and DLNM
#' analysis.
#'
#' @returns
#'  \itemize{
#'   \item `coef_` A matrix of coefficients for the reduced model.
#'   \item `vcov_` A list. Covariance matrices for each region for the reduced model.
#'   }
#'
#' @keywords internal
dlnm_reduce_cumulative <- function(
    df_list,
    var_per = c(25, 50, 75),
    var_degree = 2,
    cenper = NULL,
    cb_list,
    model_list) {
  coef_ <- matrix(
    data = NA,
    nrow = length(names(df_list)),
    ncol = length(var_per) + var_degree,
    dimnames = list(names(df_list))
  )
  vcov_ <- vector("list", length(names(df_list)))
  names(vcov_) <- names(df_list)
  for (reg in names(df_list)) {
    region_data <- df_list[[reg]]
    cb <- cb_list[[reg]]
    cenper <- ifelse(
      is.null(cenper),
      mean(region_data$temp, na.rm = TRUE),
      quantile(region_data$temp, cenper / 100, na.rm = T)
    )
    red <- dlnm::crossreduce(cb, model_list[[reg]], cen = cenper)
    coef_[reg, ] <- coef(red)
    vcov_[[reg]] <- vcov(red)
  }
  return(list(coef_, vcov_))
}

#' Meta-analysis and BLUPs
#'
#' @description Run meta-analysis using temperature average and range as meta
#' predictors. Then create the best linear unbiased predictions (BLUPs).
#'
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param coef_ A matrix of coefficients for the reduced model.
#' @param vcov_ A list. Covariance matrices for each region for the reduced model.
#' @param save_csv Boolean. Whether to save the results as a CSV. Defaults to
#' FALSE.
#' @param output_folder_path Path to folder where results should be saved.
#' Defaults to NULL.
#'
#' @returns
#' \itemize{
#'   \item `mm` A model object. A multivariate meta-analysis model.
#'   \item `blup` A list. BLUP (best linear unbiased predictions) from the
#'   meta-analysis model for each region.
#'   \item `meta_test_res` A dataframe of results from statistical tests on the meta model.
#'   }
#'
#' @keywords internal
dlnm_meta_analysis <- function(df_list,
                               coef_,
                               vcov_,
                               save_csv = FALSE,
                               output_folder_path = NULL) {
  # Assert that df_list is a list of dataframes
  is_list_of_dfs(list_ = df_list)
  # Assert that coef is a numeric matrix
  if (!is.matrix(coef_) || !is.numeric(coef_)) {
    stop("Argument 'coef_' must be a numeric matrix")
  }
  # Assert that vcov is a list of matrices.
  if (is.list(vcov_)) {
    for (matr in vcov_) {
      if (!is.matrix(matr)) {
        stop(paste(
          "'vcov_' expected a list of matrices. List contains item of",
          "type", toString(typeof(matr))
        ))
      }
    }
  } else {
    stop(paste("'vcov_' expected a list.", toString(typeof(vcov_))))
  }
  # Create average temperature and range as meta-predictors
  temp_avg <- sapply(
    df_list,
    function(x) {
      mean(x$temp, na.rm = TRUE)
    }
  )
  temp_range <- sapply(
    df_list,
    function(x) {
      diff(range(x$temp, na.rm = TRUE))
    }
  )
  # Meta-analysis
  mm <- mixmeta::mixmeta(
    formula = coef_ ~ temp_avg + temp_range,
    S = vcov_,
    data = as.data.frame(names(df_list)),
    method = "reml"
  )
  # Obtain BLUPs
  blup <- mixmeta::blup(mm, vcov = TRUE)
  names(blup) <- names(df_list)
  # Wald test
  temp_avg_wald <- fwald(mm, "temp_avg")
  temp_range_wald <- fwald(mm, "temp_range")
  # Cochran's Q-test
  qstat <- mixmeta::qtest(mm)
  # I^2 statistic
  i2stat <- ((qstat$Q - qstat$df) / qstat$Q)[1] * 100
  meta_test_res <- data.frame(
    test = c(
      "temp_avg Wald p-value",
      "temp_range Wald p-value",
      "Cochran's Q test p-value",
      "I2 (%)",
      "AIC"
    ),
    result = round(c(
      temp_avg_wald,
      temp_range_wald,
      qstat[["pvalue"]][1],
      i2stat,
      summary(mm)$AIC
    ), 3)
  )

  if (save_csv == TRUE) {
    if (!is.null(output_folder_path)) {
      check_file_exists(file.path(output_folder_path))
      write.csv(meta_test_res, file = file.path(
        output_folder_path, "meta_model_stat_test_results.csv"
      ), row.names = FALSE)
    } else {
      stop("Output path not specified")
    }
  }
  return(list(mm, blup, meta_test_res))
}

#' Define minimum mortality percentiles and temperatures
#'
#' @description Calculate the temperature at which there is minimum mortality risk
#' using the product of the basis matrix and BLUPs.
#'
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param var_fun Character. Exposure function for argvar
#' (see dlnm::crossbasis). Defaults to 'bs'.
#' @param var_per Vector. Internal knot positions for argvar
#' (see dlnm::crossbasis). Defaults to c(25,50,75).
#' @param var_degree Integer. Degree of the piecewise polynomial for argvar
#' (see dlnm::crossbasis). Defaults to 2 (quadratic).
#' @param blup A list. BLUP (best linear unbiased predictions) from the
#' meta-analysis model for each region.
#' @param coef_ A matrix of coefficients for the reduced model.
#' @param meta_analysis Boolean. Whether to perform a meta-analysis.
#'
#' @returns Percentiles and corresponding temperatures for each geography.
#'
#' @keywords internal
dlnm_min_mortality_temp <- function(
    df_list,
    var_fun = "bs",
    var_per = c(10, 75, 90),
    var_degree = 2,
    blup = NULL,
    coef_,
    meta_analysis = FALSE,
    outcome_type = c("temperature", "suicide")) {
  outcome_type <- match.arg(outcome_type)
  is_list_of_dfs(list_ = df_list)

  if (!is.null(blup) && !is.list(blup)) {
    stop("Argument 'blup' must be a list")
  }

  if (!is.matrix(coef_) || !is.numeric(coef_)) {
    stop("Argument 'coef_' must be a numeric matrix")
  }
  # --- Coefficient source ---
  coef_list <- if (meta_analysis) {
    lapply(blup, function(x) x$blup)
  } else {
    split(coef_, rownames(coef_))
  }

  # create return lists
  minperc <- rep(NA, length(df_list))
  names(minperc) <- names(df_list)
  mintemp <- rep(NA, length(df_list))
  names(mintemp) <- names(df_list)

  for (nm in names(df_list)) {
    dat <- df_list[[nm]]

    predvar <- quantile(dat$temp, 1:99 / 100, na.rm = TRUE)

    argvar <- list(
      x = predvar,
      fun = var_fun,
      knots = quantile(dat$temp, var_per / 100, na.rm = TRUE),
      degree = var_degree
    )

    # Boundary argument differs
    if (outcome_type == "temperature") {
      argvar$Bound <- range(dat$temp, na.rm = TRUE)
    } else {
      argvar$Boundary.knots <- range(dat$temp, na.rm = TRUE)
    }

    bvar <- do.call(dlnm::onebasis, argvar)

    if (outcome_type == "temperature") {
      minperc[nm] <- (1:99)[which.min(bvar %*% coef_list[[nm]])]
      mintemp[nm] <- quantile(dat$temp, minperc[nm] / 100, na.rm = TRUE)
    } else {
      minperc[nm] <- (1:50)[which.min((bvar %*% coef_list[[nm]])[1:50, ])]
      mintemp[nm] <- quantile(dat$temp, minperc[nm] / 100, na.rm = TRUE)
    }
  }

  if (outcome_type == "temperature") {
    return(list(minperc = minperc, mintemp = mintemp))
  } else {
    return(minperc)
  }
}

#' Run national predictions from meta analysis
#'
#' @description Use the meta analysis to create national level predictions
#'
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param var_fun Character. Exposure function for argvar
#' (see dlnm::crossbasis). Defaults to 'bs'.
#' @param var_per Vector. Internal knot positions for argvar
#' (see dlnm::crossbasis). Defaults to c(25,50,75).
#' @param var_degree Integer. Degree of the piecewise polynomial for argvar
#' (see dlnm::crossbasis). Defaults to 2 (quadratic).
#' @param minpercreg Vector. Percentile of maximum suicide temperature for each region.
#' @param mmpredall List of national coefficients and covariance matrices for the crosspred.
#' @param pred_list A list containing predictions from the model by region.
#' @param country Character. Name of country for national level estimates.
#'
#' @returns A list containing predictions by region.
#'
#' @keywords internal
dlnm_predict_nat <- function(
    df_list,
    var_fun = "bs",
    var_per = c(25, 50, 75),
    var_degree = 2,
    minpercreg,
    mmpredall,
    pred_list,
    country = "National") {
  national_data <- df_list[[country]]

  argvar <- list(
    x = national_data$temp,
    fun = var_fun,
    knots = quantile(national_data$temp, var_per / 100, na.rm = TRUE),
    degree = var_degree
  )

  bvar <- do.call(dlnm::onebasis, argvar)

  cen <- quantile(national_data$temp, minpercreg[country] / 100, na.rm = T)

  pred_nat <- dlnm::crosspred(bvar,
    coef = mmpredall$fit,
    vcov = mmpredall$vcov,
    cen = cen,
    model.link = "log",
    by = 0.1,
    from = min(round(national_data$temp, 1), na.rm = TRUE),
    to = max(round(national_data$temp, 1), na.rm = TRUE)
  )

  pred_list[[country]] <- pred_nat

  return(pred_list)
}

#' Power calculation
#'
#' @description Produce a power statistic by area for the attributable threshold
#' and above as a reference.
#'
#' @param df_list A list of dataframes containing daily timeseries data for a health outcome
#' and climate variables which may be disaggregated by a particular region.
#' @param pred_list A list containing predictions from the model by region.
#' @param minpercreg Vector. Percentile of maximum outcome temperature for each region.
#' @param attr_thr Integer. Percentile at which to define the temperature threshold for
#' calculating attributable risk. Defaults to 97.5.
#'
#' @returns A list containing power information by area.
#'
#' @keywords internal
dlnm_power_list <- function(df_list,
                            pred_list,
                            minperc,
                            attr_thr_high = 97.5,
                            attr_thr_low = 2.5,
                            compute_low = TRUE) {
  alpha <- 0.05
  power_list_high <- list()
  power_list_low <- list()

  for (nm in names(df_list)) {
    dat <- df_list[[nm]]
    pred <- pred_list[[nm]]
    mmt <- round(quantile(dat$temp, minperc[nm] / 100, na.rm = TRUE), 1)

    coef_effect_with_se <- data.frame(
      temperature = round(pred$predvar, 1),
      log_rr = pred$allfit,
      se = pred$allse
    )

    ## calculate high threshold
    thresh_temp_high <- round(quantile(dat$temp, attr_thr_high / 100, na.rm = TRUE), 1)
    coef_high <- coef_effect_with_se %>% dplyr::filter(temperature >= thresh_temp_high)

    power_df_high <- data.frame(
      region = nm,
      temperature = coef_high$temperature,
      cen = mmt,
      log_rr = coef_high$log_rr,
      se = coef_high$se,
      z_alpha = stats::qnorm(1 - alpha / 2)
    ) %>%
      dplyr::mutate(power = stats::pnorm(log_rr / se - z_alpha) +
        (1 - stats::pnorm(log_rr / se + z_alpha))) %>%
      dplyr::select(-z_alpha) %>%
      dplyr::mutate(
        log_rr = round(log_rr, 2),
        se = round(se, 2),
        power = round(power * 100, 1)
      )

    power_list_high[[nm]] <- power_df_high

    # Calculate low threshold conditionally
    if (compute_low) {
      thresh_temp_low <- round(quantile(dat$temp, attr_thr_low / 100, na.rm = TRUE), 1)
      coef_low <- coef_effect_with_se %>% dplyr::filter(temperature <= thresh_temp_low)

      power_df_low <- data.frame(
        region = nm,
        temperature = coef_low$temperature,
        cen = mmt,
        log_rr = coef_low$log_rr,
        se = coef_low$se,
        z_alpha = stats::qnorm(1 - alpha / 2)
      ) %>%
        dplyr::mutate(power = stats::pnorm(log_rr / se - z_alpha) +
          (1 - stats::pnorm(log_rr / se + z_alpha))) %>%
        dplyr::select(-z_alpha) %>%
        dplyr::mutate(
          log_rr = round(log_rr, 2),
          se = round(se, 2),
          power = round(power * 100, 1)
        )

      power_list_low[[nm]] <- power_df_low
      return(list(high = power_list_high, low = power_list_low))
    }
  }
  return(power_list_high)
}
