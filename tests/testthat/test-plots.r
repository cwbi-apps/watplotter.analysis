test_that("metric over time plots work", {
  # sample data
  test_data = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101), 5),
    period = "winter",
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
      as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"), each = 101),
    value = rep(seq(0, 100), 5)
  ))

  result_data = suppressWarnings(analysis_metrics(test_data))
  result_plot = plot_metrics(result_data,
    alt_colors = generate_colors(result_data))

  expect_s3_class(result_plot, "ggplot")
  expect_doppelganger("metric over time plot", result_plot)

})


test_that("metrics over time by water year class plots work", {
  # sample data
  test_dry = data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(1L:2L, each = 365),
    period = "annual",
    wycategory = factor("dry", levels = c("dry", "wet")),
    date = seq(as.POSIXct("2001-10-01 00:00:00", tz = "UTC"),
      as.POSIXct("2003-09-30 00:00:00", tz = "UTC"), by = "1 day"),
    value = rep(c(10, 20), each = 365)
  )
  test_wet = data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(3L:4L, each = 365),
    period = "annual",
    wycategory = factor("wet", levels = c("dry", "wet")),
    date = seq(as.POSIXct("2005-10-01 00:00:00", tz = "UTC"),
      as.POSIXct("2007-09-30 00:00:00", tz = "UTC"), by = "1 day"),
    value = rep(c(100, 200), each = 365)
  )
  test_data = watplotter.core::standardize_dataset(rbind(test_dry, test_wet))

  result_data = analysis_metrics(test_data, "min", by_wycategory = TRUE)
  result_plot = plot_metrics(result_data,
    alt_colors = generate_colors(result_data))

  expect_s3_class(result_plot, "ggplot")
  expect_doppelganger("metric by wycategory plot", result_plot)
})


test_that("metrics over time by base year plots work", {
  # sample data
  test_2007 = data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(1L:2L, each = 365),
    period = "annual",
    baseyear = factor(2007L, levels = c(1993L, 2007L)),
    date = seq(as.POSIXct("2001-10-01 00:00:00", tz = "UTC"),
      as.POSIXct("2003-09-30 00:00:00", tz = "UTC"), by = "1 day"),
    value = rep(c(10, 20), each = 365)
  )
  test_1993 = data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(3L:4L, each = 365),
    period = "annual",
    baseyear = factor(1993L, levels = c(1993L, 2007L)),
    date = seq(as.POSIXct("2005-10-01 00:00:00", tz = "UTC"),
      as.POSIXct("2007-09-30 00:00:00", tz = "UTC"), by = "1 day"),
    value = rep(c(100, 200), each = 365)
  )
  test_data = watplotter.core::standardize_dataset(rbind(test_1993, test_2007))

  result_data = analysis_metrics(test_data, "min", by_baseyear = TRUE)
  result_plot = plot_metrics(result_data,
    alt_colors = generate_colors(result_data))

  expect_s3_class(result_plot, "ggplot")
  expect_doppelganger("metric by baseyear plot", result_plot)
})


test_that("metric by frequency plots work", {

  test_data = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), 5L),
    period = "jan",
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
      as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"),
      each = 101L),
    value = rep(seq(0, 100), 5L) + rep(seq_len(5L), 101L)
  ))

  result_data = analysis_frequency(test_data, c("mean", "min"))
  result_plot = plot_frequency(result_data,
    alt_colors = generate_colors(result_data))

  expect_s3_class(result_plot, "ggplot")
  suppressWarnings(
    expect_doppelganger("metric by frequency plot", result_plot)
  )
})


test_that("metric by frequency by water year class plots work", {

  test_dry = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), 5L),
    period = "jan",
    wycategory = factor("dry", levels = c("dry", "wet")),
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
        as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"),
      each = 101L),
    value = rep(seq(0, 100), 5L) + rep(seq_len(5L), 101L)
  ))

  test_wet = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), 5L),
    period = "jan",
    wycategory = factor("wet", levels = c("dry", "wet")),
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
        as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"),
      each = 101L),
    value = rep(seq(0, 100), 5L) + rep(seq_len(5L), 101L) + 50
  ))
  test_data = rbind(test_dry, test_wet)

  result_data = analysis_frequency(test_data, "mean",
    by_wycategory = TRUE)
  result_plot = plot_frequency(result_data,
    alt_colors = generate_colors(result_data))

  expect_s3_class(result_plot, "ggplot")
  suppressWarnings(
    expect_doppelganger("metric by frequency by wycategory plot",
      result_plot)
  )
})


test_that("metric by frequency by base year plots work", {

  test_2007 = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), 5L),
    period = "jan",
    baseyear = factor(2007L, levels = c(1993L, 2007L)),
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
        as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"),
      each = 101L),
    value = rep(seq(0, 100), 5L) + rep(seq_len(5L), 101L)
  ))

  test_1993 = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), 5L),
    period = "jan",
    baseyear = factor(1993L, levels = c(1993L, 2007L)),
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
        as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"),
      each = 101L),
    value = rep(seq(0, 100), 5L) + rep(seq_len(5L), 101L) + 50
  ))
  test_data = rbind(test_1993, test_2007)

  result_data = analysis_frequency(test_data, "mean",
    by_baseyear = TRUE)
  result_plot = plot_frequency(result_data,
    alt_colors = generate_colors(result_data))

  expect_s3_class(result_plot, "ggplot")
  suppressWarnings(
    expect_doppelganger("metric by frequency by baseyear plot",
      result_plot)
  )
})


test_that("duration plots work", {
  test_data = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), 5L),
    period = "winter",
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
        as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"),
      each = 101L),
    value = rep(seq(0, 100), 5L)
  ))

  result_data = analysis_duration(test_data)
  result_plot = plot_duration(result_data,
    alt_colors = generate_colors(result_data))

  expect_s3_class(result_plot, "ggplot")
  suppressWarnings(
    expect_doppelganger("duration plot", result_plot)
  )
})


test_that("duration by water year class plots work", {
  test_data_dry = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), 5L),
    period = "winter",
    wycategory = factor("dry", c("dry", "average", "wet")),
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
        as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"),
      each = 101L),
    value = rep(seq(0, 100), 5L)
  ))
  test_data_wet = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), 5L),
    period = "winter",
    wycategory = factor("wet", c("dry", "average", "wet")),
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
        as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"),
      each = 101L),
    value = rep(seq(0, 100), 5L) + 50
  ))
  test_data = rbind(test_data_dry, test_data_wet)

  result_data = analysis_duration(test_data, by_wycategory = TRUE)
  result_plot = plot_duration(result_data,
    alt_colors = generate_colors(result_data))

  expect_s3_class(result_plot, "ggplot")
  suppressWarnings(
    expect_doppelganger("duration by wycategory plot", result_plot)
  )

})


test_that("duration by base year plots work", {
  test_data_2007 = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), 5L),
    period = "winter",
    baseyear = factor(2007L, levels = c(1993L, 2007L)),
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
        as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"),
      each = 101L),
    value = rep(seq(0, 100), 5L)
  ))
  test_data_1993 = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), 5L),
    period = "winter",
    baseyear = factor(1993L, levels = c(1993L, 2007L)),
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
        as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"),
      each = 101L),
    value = rep(seq(0, 100), 5L) + 50
  ))
  test_data = rbind(test_data_1993, test_data_2007)

  result_data = analysis_duration(test_data, by_baseyear = TRUE)
  result_plot = plot_duration(result_data,
    alt_colors = generate_colors(result_data))

  expect_s3_class(result_plot, "ggplot")
  suppressWarnings(
    expect_doppelganger("duration by baseyear plot", result_plot)
  )

})


test_that("metric vs metric plots work", {
  #sample data
  test_data = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), each = 5L),
    period = "winter",
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
        as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"),
      101L),
    value = rep(seq_len(5), 101L) + 10 * rep(seq_len(101L), each = 5L)
  ))

  result_data = analysis_metric_vs_metric(test_data, test_data, "max", "min")
  result_plot = plot_metric_vs_metric(result_data,
    alt_colors = generate_colors(result_data))

  expect_s3_class(result_plot, "ggplot")
  suppressWarnings(
    expect_doppelganger("metric vs metric plot", result_plot)
  )
})


test_that("metric vs metric by water year class plots work", {
  #sample data
  test_dry = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), each = 5L),
    period = "winter",
    wycategory = factor("dry", levels = c("dry", "wet")),
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
        as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"),
      101L),
    value = rep(seq_len(5), 101L) + 10 * rep(seq_len(101L), each = 5L)
  ))
  test_wet = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), each = 5L),
    period = "winter",
    wycategory = factor("wet", levels = c("dry", "wet")),
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
        as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"),
      101L),
    value = rep(seq_len(5), 101L) + 10 * rep(seq_len(101L), each = 5L) +
      rep(c(0, 0, 0, 0, 100L), 101L)
  ))
  test_data = rbind(test_dry, test_wet)

  result_data = analysis_metric_vs_metric(test_data, test_data, "max", "min",
    by_wycategory = TRUE)
  result_plot = plot_metric_vs_metric(result_data,
    alt_colors = generate_colors(result_data))

  expect_s3_class(result_plot, "ggplot")
  suppressWarnings(
    expect_doppelganger("metric vs metric by wycategory plot", result_plot)
  )
})


test_that("metric vs metric by base year plots work", {
  #sample data
  test_2007 = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), each = 5L),
    period = "winter",
    baseyear = factor(2007L, levels = c(1993L, 2007L)),
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
        as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"),
      101L),
    value = rep(seq_len(5), 101L) + 10 * rep(seq_len(101L), each = 5L)
  ))
  test_1993 = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), each = 5L),
    period = "winter",
    baseyear = factor(1993L, levels = c(1993L, 2007L)),
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
        as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"),
      101L),
    value = rep(seq_len(5), 101L) + 10 * rep(seq_len(101L), each = 5L) +
      rep(c(0, 0, 0, 0, 100L), 101L)
  ))
  test_data = rbind(test_1993, test_2007)

  result_data = analysis_metric_vs_metric(test_data, test_data, "max", "min",
    by_baseyear = TRUE)
  result_plot = plot_metric_vs_metric(result_data,
    alt_colors = generate_colors(result_data))

  expect_s3_class(result_plot, "ggplot")
  suppressWarnings(
    expect_doppelganger("metric vs metric by baseyear plot", result_plot)
  )
})


test_that("metric vs forecast plots work", {
  #sample data
  test_data = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    period = "winter",
    event = rep(seq_len(101L), each = 5L),
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
      as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"), 101L),
    value = seq_len(505L)
  ))

  test_forecast_data = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "/FORECAST/XXX/XXX/STOR//IR-CENTURY/XXX/",
    event = seq_len(101L),
    period = "31jan",
    date = rep(as.POSIXct("2002-01-31 00:00:00", tz = "UTC"), 101L),
    value = rep(seq(10, 110, by = 10), each = 10, length.out = 101)
  ))

  result_data = analysis_metric_vs_forecast(test_data,
    test_forecast_data, "min")
  result_plot = plot_metric_vs_forecast(result_data,
    alt_colors = generate_colors(result_data))

  expect_s3_class(result_plot, "ggplot")
  expect_doppelganger("metric vs forecast plot", result_plot)

})


test_that("metric vs forecast by water year class plots work", {
  #sample data
  test_dry = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), each = 5L),
    period = "winter",
    wycategory = factor("dry", levels = c("dry", "average", "wet")),
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
      as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"), 101L),
    value = seq_len(505L)
  ))
  test_wet = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), each = 5L),
    period = "winter",
    wycategory = factor("wet", levels = c("dry", "average", "wet")),
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
      as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"), 101L),
    value = seq_len(505L) + 200
  ))
  test_data = rbind(test_dry, test_wet)

  test_forecast_data = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "/FORECAST/XXX/XXX/STOR//IR-CENTURY/XXX/",
    event = seq_len(101L),
    period = "31jan",
    date = rep(as.POSIXct("2002-01-31 00:00:00", tz = "UTC"), 101L),
    value = rep(seq(10, 110, by = 10), each = 10, length.out = 101)
  ))

  result_data = analysis_metric_vs_forecast(test_data,
    test_forecast_data, "min", by_wycategory = TRUE)
  result_plot = plot_metric_vs_forecast(result_data,
    alt_colors = generate_colors(result_data))

  expect_s3_class(result_plot, "ggplot")
  expect_doppelganger("metric vs forecast by wycategory plot", result_plot)

})


test_that("metric vs forecast by base year plots work", {
  #sample data
  test_2007 = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), each = 5L),
    period = "winter",
    baseyear = factor(2007L, levels = c(1993L, 2007L)),
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
      as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"), 101L),
    value = seq_len(505L)
  ))
  test_1993 = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), each = 5L),
    period = "winter",
    baseyear = factor(1993L, levels = c(1993L, 2007L)),
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
      as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"), 101L),
    value = seq_len(505L) + 200
  ))
  test_data = rbind(test_1993, test_2007)

  test_forecast_data = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "/FORECAST/XXX/XXX/STOR//IR-CENTURY/XXX/",
    event = seq_len(101L),
    period = "31jan",
    date = rep(as.POSIXct("2002-01-31 00:00:00", tz = "UTC"), 101L),
    value = rep(seq(10, 110, by = 10), each = 10, length.out = 101)
  ))

  result_data = analysis_metric_vs_forecast(test_data,
    test_forecast_data, "min", by_baseyear = TRUE)
  result_plot = plot_metric_vs_forecast(result_data,
    alt_colors = generate_colors(result_data))

  expect_s3_class(result_plot, "ggplot")
  expect_doppelganger("metric vs forecast by baseyear plot", result_plot)

})


test_that("trace plots work", {
  test_data = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), 5L),
    period = "winter",
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
        as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"),
      each = 101L),
    value = rep(seq(0, 100), 5L) + rep(seq_len(101L), each = 5L)
  ))

  result_data = analysis_trace(test_data)
  result_plot = plot_trace(result_data,
    alt_colors = generate_colors(result_data))

  expect_s3_class(result_plot, "ggplot")
  suppressWarnings(
    expect_doppelganger("trace plot", result_plot)
  )
})


test_that("trace by water year class plots work", {
  test_data_dry = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), 5L),
    period = "winter",
    wycategory = factor("dry", c("dry", "average", "wet")),
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
        as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"),
      each = 101L),
    value = rep(seq(0, 100), 5L) + rep(seq_len(101L), each = 5L)
  ))
  test_data_wet = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), 5L),
    period = "winter",
    wycategory = factor("wet", c("dry", "average", "wet")),
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
        as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"),
      each = 101L),
    value = rep(seq(0, 100), 5L) + 50 + rep(seq_len(101L), each = 5L)
  ))
  test_data = rbind(test_data_dry, test_data_wet)

  result_data = analysis_trace(test_data, by_wycategory = TRUE)
  result_plot = plot_trace(result_data,
    alt_colors = generate_colors(result_data))

  expect_s3_class(result_plot, "ggplot")
  suppressWarnings(
    expect_doppelganger("trace by wycategory plot", result_plot)
  )

})


test_that("trace by base year plots work", {
  test_data_2007 = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), 5L),
    period = "winter",
    baseyear = factor(2007L, levels = c(1993L, 2007L)),
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
        as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"),
      each = 101L),
    value = rep(seq(0, 100), 5L) + rep(seq_len(101L), each = 5L)
  ))
  test_data_1993 = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), 5L),
    period = "winter",
    baseyear = factor(1993L, levels = c(1993L, 2007L)),
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
        as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"),
      each = 101L),
    value = rep(seq(0, 100), 5L) + 50 +  + rep(seq_len(101L), each = 5L)
  ))
  test_data = rbind(test_data_1993, test_data_2007)

  result_data = analysis_trace(test_data, by_baseyear = TRUE)
  result_plot = plot_trace(result_data,
    alt_colors = generate_colors(result_data))

  expect_s3_class(result_plot, "ggplot")
  suppressWarnings(
    expect_doppelganger("trace by baseyear plot", result_plot)
  )

})
