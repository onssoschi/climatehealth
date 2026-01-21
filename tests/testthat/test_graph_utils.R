# Test for graph_utils.R

# Tests for create_grid
patrick::with_parameters_test_that(
    "Create grid works as intended",
    {
        grid <- create_grid(plot_count)
        expect_equal(grid, expected_grid)
    },
    plot_count = c(9, 6, 14),
    expected_grid = list(
        c(3, 3),
        c(3, 2),
        c(4, 4)
    ),
    .test_name = c("Perfect square", "base<0.5", "base>=0.5")
)

# Tests for plot_correlation_matrix
input_matrix <- matrix(1:9, nrow = 3, ncol = 3)
patrick::with_parameters_test_that(
    "Test that plot_correlation_matrix works as intended.",
    {
        # Create output path
        temp_dir <- withr::local_tempdir()
        full_path <- file.path(temp_dir, output_path)
        # Plot corr matrix
        plot_correlation_matrix(input_matrix, "test", full_path)
        # Assert it was saved
        expect_true(file.exists(file.path(temp_dir, exp_out_path)))
    },
    output_path = c("test_corr.png", "test_corr2"),
    exp_out_path = c("test_corr.png", "test_corr2.png"),
    .test_name = c("with .png ext", "without .png ext")
)

# Tests for plot_distributions
# Sample data
set.seed(123)
sample_df <- data.frame(
  A = rnorm(100),
  B = runif(100),
  C = rpois(100, lambda = 5)
)


with_parameters_test_that(
    "plot_distributions saves PDF files correctly",
    {
        # define output path
        tmpdir <- local_tempdir()
        outfile <- file.path(tmpdir, output_path)
        # plot and save dists
        plot_distributions(
            df = sample_df,
            columns = c("A", "B", "C"),
            title = "test",
            save_hists = TRUE,
            output_path = outfile
        )
        # validate existence
        expect_true(file.exists(file.path(tmpdir, exp_output_path)))
    },
    output_path = c("test_dist_1.pdf", "test_dist_2"),
    exp_output_path = c("test_dist_1.pdf", "test_dist_2.pdf"),
    .test_name = c("with .png extension", "without .png extension")
)

test_that(
    "Function runs silently without displaying plot", 
    {
        df <- data.frame(
            date = seq.Date(from = as.Date("2023-01-01"), by = "month", length.out = 12),
            outcome1 = rnorm(12)
        )

        # Redirect graphics to a null device to suppress display
        pdf(NULL)
        on.exit(dev.off())

        expect_silent(
            plot_seasonal_trends(
                df,
                date_col = "date",
                outcome_cols = c("outcome1"),
                save_plot = FALSE
            )
        )
    }
)

# Tests for get_alpha_colour
test_that(
    "get_alpha_colour works as expected",
    {
        rgb <- get_alpha_colour("#F54927", 0.6)
        expect_equal(rgb, "#F5492799")
    }
)

# Tests for plot_moving_average
sample_ma_df <- data.frame(
  date = seq.Date(from = as.Date("2023-01-01"), by = "day", length.out = 100),
  value = rnorm(100, mean = 50, sd = 10)
)

with_parameters_test_that(
  "plot_moving_average saves PDF files correctly",
  {
    # Define output path
    tmpdir <- local_tempdir()
    outfile <- file.path(tmpdir, output_path)
    # plot MA
    plot_moving_average(
      df = sample_ma_df,
      time_col = "date",
      value_col = "value",
      ma_days = 5,
      ma_sides = 2,
      title = "Test MA Plot",
      save_plot = TRUE,
      output_path = outfile
    )
    # validate outputs
    expect_true(file.exists(file.path(tmpdir, exp_output_path)))
  },
  output_path = c("ma_plot_1.pdf", "ma_plot_2.txt"),
  exp_output_path = c("ma_plot_1.pdf", "ma_plot_2.pdf"),
  .test_name = c("with .pdf extension", "without .pdf extension")
)

test_that(
    "plot_moving_average handles missing values",
    {
        # Create DF with NULLs
        df_na <- sample_ma_df
        df_na$value <- NA
        # Validate NAs handled
        expect_warning(
            result <- plot_moving_average(
                df = df_na,
                time_col = "date",
                value_col = "value",
                ma_days = 3,
                ma_sides = 1,
                title = "Missing Data Test",
                save_plot = FALSE
            ),
            regexp = "No valid data to plot"
        )
        expect_null(result)
    }
)

test_that(
    "plot_moving_average runs silently without saving", 
    {
        pdf(NULL)
        on.exit(dev.off())
        expect_silent(
            plot_moving_average(
            df = sample_ma_df,
            time_col = "date",
            value_col = "value",
            ma_days = 7,
            ma_sides = 1,
            title = "No Save Test",
            save_plot = FALSE
            )
        )
    }
)

test_that(
  "plot_moving_average closes PDF when no valid values and save_plot is TRUE", 
  {
    df <- data.frame(
      date = seq.Date(from = as.Date("2023-01-01"), by = "day", length.out = 10),
      value = rep(NA, 10)  # all invalid
    )
    tmpdir <- withr::local_tempdir()
    outfile <- file.path(tmpdir, "empty_plot.pdf")

    expect_warning(
      result <- plot_moving_average(
        df = df,
        time_col = "date",
        value_col = "value",
        ma_days = 3,
        ma_sides = 1,
        title = "Empty Plot Test",
        save_plot = TRUE,
        output_path = outfile
      ),
      regexp = "No valid data to plot for value"
    )

    expect_true(file.exists(outfile))  # PDF was created
    expect_null(result)                # Function returned NULL
  }
)


# Tests for plot_scatter_grid
scatter_df <- data.frame(
  X = rnorm(100),
  Y = runif(100),
  Z = rpois(100, lambda = 3)
)

with_parameters_test_that(
  "plot_scatter_grid saves PDF files correctly",
  {
    tmpdir <- local_tempdir()
    outfile <- file.path(tmpdir, output_path)

    plot_scatter_grid(
      df = scatter_df,
      main_col = "X",
      comparison_cols = c("Y", "Z"),
      title = "Scatter Grid Test",
      save_scatters = TRUE,
      output_path = outfile
    )

    expect_true(file.exists(file.path(tmpdir, exp_output_path)))
  },
  output_path = c("scatter_1.pdf", "scatter_2.txt"),
  exp_output_path = c("scatter_1.pdf", "scatter_2.pdf"),
  .test_name = c("with .pdf extension", "without .pdf extension")
)

test_that(
    "plot_scatter_grid runs silently without saving", 
    {
        pdf(NULL)
        on.exit(dev.off())
        expect_silent(
            plot_scatter_grid(
                df = scatter_df,
                main_col = "X",
                comparison_cols = c("Y", "Z"),
                title = "No Save Test",
                save_scatters = FALSE
            )
        )
    }
)

test_that(
    "plot_scatter_grid returns early if fewer than 2 columns",
    {
        expect_silent(
            result <- plot_scatter_grid(
                df = scatter_df,
                main_col = "X",
                comparison_cols = character(0),
                title = "Too Few Columns",
                save_scatters = FALSE
            )
        )
    }
)


# Tests for plot_boxplots()

test_that(
    "plot_boxplots creates plots as expected", 
    {
        df <- data.frame(a = rnorm(10), b = rnorm(10))
        tmpdir <- local_tempdir()
        outfile <- file.path(tmpdir, "boxplot_output.pdf")

        plot_boxplots(
            df,
            columns = c("a", "b"),
            save_plot = TRUE,
            output_path = outfile
        )

        expect_true(file.exists(outfile))
    }
)

test_that(
    "Plot is created when using select_numeric = TRUE", 
    {
        df <- data.frame(x = rnorm(10), y = rnorm(10), z = letters[1:10])
        tmpdir <- local_tempdir()
        outfile <- file.path(tmpdir, "numeric_only.pdf")

        plot_boxplots(
            df,
            select_numeric = TRUE,
            save_plot = TRUE,
            output_path = outfile
        )

        expect_true(file.exists(outfile))
    }
)

test_that(
    "Plot is created with custom y-axis labels", 
    {
        df <- data.frame(a = rnorm(10), b = rnorm(10))
        tmpdir <- local_tempdir()
        outfile <- file.path(tmpdir, "custom_labels.pdf")

        plot_boxplots(
            df,
            columns = c("a", "b"),
            ylabs = c("Height (cm)", "Weight (kg)"),
            save_plot = TRUE,
            output_path = outfile
        )

        expect_true(file.exists(outfile))
    }
)

test_that(
    "Error is thrown when neither columns nor select_numeric is provided", 
    {
        df <- data.frame(a = 1:10, b = 11:20)
        expect_error(
            plot_boxplots(df),
            "Please specify columns or 'select_numeric' to TRUE"
        )
    }
)

# Tests for plot_seasonal_trends

test_that(
    "plot_seasonal_trends behaves as expected", 
    {
        df <- data.frame(
            date = seq.Date(from = as.Date("2023-01-01"), by = "month", length.out = 12),
            temp = rnorm(12),
            humidity = rnorm(12)
        )
        tmpdir <- withr::local_tempdir()
        outfile <- file.path(tmpdir, "seasonal_plot.pdf")

        plot_seasonal_trends(
            df,
            date_col = "date",
            outcome_cols = c("temp", "humidity"),
            ylabs = c("Temperature (°C)", "Humidity (%)"),
            save_plot = TRUE,
            output_path = outfile
        )

        expect_true(file.exists(outfile))
    }
)

test_that(
    "plot_seasonal_trends runs silently without saving plot", 
    {
        df <- data.frame(
            date = seq.Date(from = as.Date("2023-01-01"), by = "month", length.out = 12),
            outcome1 = rnorm(12)
        )

        pdf(NULL)  # suppress actual plot rendering
        on.exit(dev.off())

        expect_silent(
            plot_seasonal_trends(
                df,
                date_col = "date",
                outcome_cols = c("outcome1"),
                save_plot = FALSE
            )
        )
    }
)

# Test for plot_regional_trends

test_that(
    "plot_regional_trends outputs a PDF as expected", 
    {
        df <- data.frame(
            region = rep(c("North", "South", "East", "West"), each = 5),
            temp = rnorm(20),
            humidity = rnorm(20)
        )
        tmpdir <- local_tempdir()
        outfile <- file.path(tmpdir, "regional_plot.pdf")

        plot_regional_trends(
            df,
            region_col = "region",
            outcome_cols = c("temp", "humidity"),
            ylabs = c("Temperature (°C)", "Humidity (%)"),
            save_plot = TRUE,
            output_path = outfile
        )

        expect_true(file.exists(outfile))
    }
)

test_that(
    "Function runs silently without saving plot", 
    {
        df <- data.frame(
            region = rep(c("North", "South"), each = 6),
            temp = rnorm(12)
        )

        pdf(NULL)
        on.exit(dev.off())

        expect_silent(
            plot_regional_trends(
                df,
                region_col = "region",
                outcome_cols = c("temp"),
                save_plot = FALSE
            )
        )
    }
)

test_that(
    "Function skips NA-only outcome column with warning", 
    {
        df <- data.frame(
            region = rep(c("North", "South"), each = 6),
            temp = rep(NA, 12),
            humidity = rnorm(12)
        )

        pdf(NULL)
        on.exit(dev.off())

        expect_warning(
            plot_regional_trends(
                df,
                region_col = "region",
                outcome_cols = c("temp", "humidity"),
                save_plot = FALSE
            ),
            regexp = "No valid data to plot for temp"
        )
    }
)

# Tests for plot_rate_overall

test_that(
    "plot_rate_overall saves plot as expected", 
    {
        df <- data.frame(
            date = seq.Date(from = as.Date("2020-01-01"), by = "year", length.out = 3),
            cases = c(100, 150, 200),
            population = c(100000, 120000, 130000)
        )
        tmpdir <- local_tempdir()
        outfile <- file.path(tmpdir, "rate_plot.pdf")

        plot_rate_overall(
            df = df,
            dependent_col = "cases",
            population_col = "population",
            date_col = "date",
            save_rate = TRUE,
            output_path = outfile
        )

        expect_true(file.exists(outfile))
    }
)

test_that(
    "plot_rate_overall returns a ggplot object when save_rate is FALSE", 
    {
        df <- data.frame(
            date = seq.Date(from = as.Date("2020-01-01"), by = "year", length.out = 3),
            cases = c(100, 150, 200),
            population = c(100000, 120000, 130000)
        )

        result <- plot_rate_overall(
            df = df,
            dependent_col = "cases",
            population_col = "population",
            date_col = "date",
            save_rate = FALSE
        )

        expect_s3_class(result, "ggplot")
    }
)

test_that(
    "plot_rate_overall runs silently without displaying plot", 
    {
        df <- data.frame(
            date = seq.Date(from = as.Date("2020-01-01"), by = "year", length.out = 3),
            cases = c(100, 150, 200),
            population = c(100000, 120000, 130000)
        )

        pdf(NULL)
        on.exit(dev.off())

        expect_silent(
            plot_rate_overall(
                df = df,
                dependent_col = "cases",
                population_col = "population",
                date_col = "date",
                save_rate = FALSE
            )
        )
    }
)

test_that(
    "plot_rate_overall handles population column with commas", 
    {
        df <- data.frame(
            date = seq.Date(from = as.Date("2020-01-01"), by = "year", length.out = 3),
            cases = c(100, 150, 200),
            population = c("100,000", "120,000", "130,000")
        )

        result <- plot_rate_overall(
            df = df,
            dependent_col = "cases",
            population_col = "population",
            date_col = "date",
            save_rate = FALSE
        )

        expect_s3_class(result, "ggplot")
    }
)

# Tests for plot_total_variables_by_year

test_that(
    "plot_total_variables_by_year saves plots as expected", 
    {
        df <- data.frame(
            date = seq.Date(from = as.Date("2020-01-01"), by = "year", length.out = 3),
            cases = c(100, 150, 200),
            deaths = c(10, 15, 20)
        )
        tmpdir <- local_tempdir()
        outfile <- file.path(tmpdir, "totals_plot.pdf")

        plot_total_variables_by_year(
            df = df,
            date_col = "date",
            variables = c("cases", "deaths"),
            save_total = TRUE,
            output_path = outfile
        )

        expect_true(file.exists(outfile))
    }
)

test_that(
    "plot_total_variables_by_year runs silently without saving plot", 
    {
        df <- data.frame(
            date = seq.Date(from = as.Date("2020-01-01"), by = "year", length.out = 3),
            cases = c(100, 150, 200)
        )

        pdf(NULL)
        on.exit(dev.off())

        expect_silent(
            plot_total_variables_by_year(
                df = df,
                date_col = "date",
                variables = c("cases"),
                save_total = FALSE
            )
        )
    }
)

test_that(
    "plot_total_variables_by_year handles NA values in variables", 
    {
        df <- data.frame(
            date = seq.Date(from = as.Date("2020-01-01"), by = "year", length.out = 3),
            cases = c(100, NA, 200),
            deaths = c(NA, NA, NA)
        )

        pdf(NULL)
        on.exit(dev.off())

        expect_silent(
            plot_total_variables_by_year(
                df = df,
                date_col = "date",
                variables = c("cases", "deaths"),
                save_total = FALSE
            )
        )
    }
)