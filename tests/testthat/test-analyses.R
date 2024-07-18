test_that("available analyses works", {
  # throw error if analysis types change
  expect_setequal(
    available_analyses(),
    c("metric over time", "metric by frequency", "duration",
      "metric vs forecast", "metric vs metric", "trace")
  )
})


test_that("metrics analysis works", {
  # sample data
  test_data = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), 5L),
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
        as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"),
      each = 101L),
    value = rep(seq(0, 100), 5L)
  ))

  # preserve groups
  expect_setequal(names(suppressWarnings(analysis_metrics(test_data))),
    fields("metric", "alt", "path", "refdate", "value"))
  # fail on unexpected names
  expect_error(analysis_metrics(test_data, grouping = c("foo")))

  # fail if all available metrics are not returned in dataset
  check_metrics = setdiff(available_metrics(), "median")
  expect_setequal(analysis_metrics(test_data, check_metrics)$metric,
    check_metrics)
  # fail on changes to calculations
  expect_equal(analysis_metrics(test_data, "1% exceedance")$value,
    rep(99.49, 5), ignore_attr = c("class", "names"))
  expect_equal(analysis_metrics(test_data, "10% exceedance")$value,
    rep(90.4, 5), ignore_attr = c("class", "names"))
  expect_equal(analysis_metrics(test_data, "25% exceedance")$value,
    rep(75.25, 5), ignore_attr = c("class", "names"))
  expect_equal(analysis_metrics(test_data, "50% exceedance")$value,
    rep(50, 5), ignore_attr = c("class", "names"))
  expect_equal(analysis_metrics(test_data, "75% exceedance")$value,
    rep(24.75, 5), ignore_attr = c("class", "names"))
  expect_equal(analysis_metrics(test_data, "90% exceedance")$value,
    rep(9.6, 5), ignore_attr = c("class", "names"))
  expect_equal(analysis_metrics(test_data, "99% exceedance")$value,
    rep(0.51, 5), ignore_attr = c("class", "names"))
  expect_equal(analysis_metrics(test_data, "min")$value,
    rep(0, 5), ignore_attr = "class")
  expect_equal(analysis_metrics(test_data, "max")$value,
    rep(100, 5), ignore_attr = "class")
  expect_equal(analysis_metrics(test_data, "mean")$value,
    rep(50, 5), ignore_attr = "class")
  # extra checks for median
  expect_setequal(analysis_metrics(test_data, "median")$metric,
    "median")
  expect_equal(analysis_metrics(test_data, "50% exceedance")$value,
    analysis_metrics(test_data, "median")$value)

  # test that run grouping works
  test_data2 = rbind(
    watplotter.core::standardize_dataset(data.frame(
      alt = "PA",
      run = "r2022",
      path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
      event = rep(seq_len(101L), 5L),
      date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
        as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"),
        each = 101L),
      value = rep(seq(0, 100), 5L)
    )),
    watplotter.core::standardize_dataset(data.frame(
      alt = "PA",
      run = "r2023a",
      path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
      event = rep(seq_len(101L), 5L),
      date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
          as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"),
        each = 101L),
      value = rep(seq(0, 100), 5L) + 20
    ))
  )
  expect_setequal(
    names(suppressWarnings(analysis_metrics(test_data2))),
    fields("metric", "alt", "run", "path", "refdate", "value")
  )
  expect_equal(
    analysis_metrics(test_data2, "1% exceedance")$value,
    rep(c(99.49, 119.49), 5L),
    ignore_attr = c("class", "names")
  )
  expect_setequal(
    analysis_metrics(test_data2, "1% exceedance")$run,
    c("r2022", "r2023a")
  )

})


test_that("metrics analysis by water year class works", {
  # sample data
  test_dry = data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(2L), each = 365L),
    wycategory = factor("dry", levels = c("dry", "wet")),
    date = seq(as.POSIXct("2001-10-01 00:00:00", tz = "UTC"),
      as.POSIXct("2003-09-30 00:00:00", tz = "UTC"), by = "1 day"),
    value = rep(c(10, 20), each = 365L)
  )
  test_wet = data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq(3L, 4L), each = 365L),
    wycategory = factor("wet", levels = c("dry", "wet")),
    date = seq(as.POSIXct("2005-10-01 00:00:00", tz = "UTC"),
      as.POSIXct("2007-09-30 00:00:00", tz = "UTC"), by = "1 day"),
    value = rep(c(100, 200), each = 365L)
  )
  test_data = watplotter.core::standardize_dataset(rbind(test_dry, test_wet))

  result_data = analysis_metrics(test_data, "min", by_wycategory = TRUE)

  expect_setequal(names(result_data), fields("wycategory", "metric",
    "alt", "refdate", "path", "value"))
  expect_setequal(result_data[[fields("wycategory")]], c("dry", "wet"))
  expect_setequal(result_data$metric, "min")
  expect_equal(result_data$value, rep(c(10, 100), 365L))

  expect_error(analysis_metrics(test_data, c("min", "max"),
      by_wycategory = TRUE))

})


test_that("metrics analysis by base year works", {
  # sample data
  test_2007 = data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(2L), each = 365L),
    baseyear = factor(2007L, levels = c(1993L, 2007L)),
    date = seq(as.POSIXct("2001-10-01 00:00:00", tz = "UTC"),
      as.POSIXct("2003-09-30 00:00:00", tz = "UTC"), by = "1 day"),
    value = rep(c(10, 20), each = 365L)
  )
  test_1993 = data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq(3L, 4L), each = 365L),
    baseyear = factor(1993L, levels = c(1993L, 2007L)),
    date = seq(as.POSIXct("2005-10-01 00:00:00", tz = "UTC"),
      as.POSIXct("2007-09-30 00:00:00", tz = "UTC"), by = "1 day"),
    value = rep(c(100, 200), each = 365L)
  )
  test_data = watplotter.core::standardize_dataset(rbind(test_1993, test_2007))

  result_data = analysis_metrics(test_data, "min", by_baseyear = TRUE)

  expect_setequal(names(result_data), fields("baseyear", "metric",
    "alt", "refdate", "path", "value"))
  expect_setequal(result_data[[fields("baseyear")]], c(1993L, 2007L))
  expect_setequal(result_data$metric, "min")
  expect_equal(result_data$value, rep(c(100, 10), 365L))

  expect_error(analysis_metrics(test_data, c("min", "max"),
      by_baseyear = TRUE))
  expect_error(analysis_metrics(test_data, "min",
      by_wycategory = TRUE, by_baseyear = TRUE))

})


test_that("metric by frequency analysis works", {

  test_data = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), 5L),
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
        as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"),
      each = 101L),
    value = rep(seq(0, 100), 5L) + rep(seq_len(5L), 101L) 
  ))

  result_data = analysis_frequency(test_data, c("mean", "median"))

  expect_setequal(
    names(result_data),
    fields("alt", "path", "metric", "probability", "quantile")
  )
  expect_setequal(
    result_data$metric,
    c("mean", "median")
  )
  expect_equal(
    result_data$probability,
    rep(seq(0.001, 0.999, 0.001), 2)
  )

  expect_error(analysis_frequency(test_data, "min",
      by_wycategory = TRUE, by_baseyear = TRUE))

})


test_that("metric by frequency by water year class analysis works", {

  skip("test not completed")

  test_dry = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), 5L),
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
    wycategory = factor("wet", levels = c("dry", "wet")),
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
        as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"),
      each = 101L),
    value = rep(seq(0, 100), 5L) + rep(seq_len(5L), 101L) + 50
  ))
  test_data = rbind(test_dry, test_wet)

  analysis_frequency(test_data, "mean", by_wycategory = TRUE)
  expect_error(analysis_frequency(test_data, c("mean", "median"),
      by_wycategory = TRUE))

})


test_that("metric by frequency by base year analysis works", {

  skip("test not completed")

  test_2007 = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), 5L),
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
    baseyear = factor(1993L, levels = c(1993L, 2007L)),
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
        as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"),
      each = 101L),
    value = rep(seq(0, 100), 5L) + rep(seq_len(5L), 101L) + 50
  ))
  test_data = rbind(test_1993, test_2007)

  analysis_frequency(test_data, "mean", by_baseyear = TRUE)
  expect_error(analysis_frequency(test_data, c("mean", "median"),
      by_baseyear = TRUE))
  expect_error(analysis_frequency(test_data, "median",
      by_wycategory = TRUE, by_baseyear = TRUE))

})


test_that("duration analysis works", {

  skip("test not completed")

  test_data = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), 5L),
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
        as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"),
      each = 101L),
    value = rep(seq(0, 100), 5L)
  ))

  result_data = analysis_duration(test_data)

})


test_that("duration by water year class analysis works", {

  skip("test not completed")

  # sample data
  test_dry = data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(2L), each = 365L),
    wycategory = factor("dry", levels = c("dry", "wet")),
    date = seq(as.POSIXct("2001-10-01 00:00:00", tz = "UTC"),
      as.POSIXct("2003-09-30 00:00:00", tz = "UTC"), by = "1 day"),
    value = rep(c(10, 20), each = 365L)
  )
  test_wet = data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq(3L, 4L), each = 365L),
    wycategory = factor("wet", levels = c("dry", "wet")),
    date = seq(as.POSIXct("2005-10-01 00:00:00", tz = "UTC"),
      as.POSIXct("2007-09-30 00:00:00", tz = "UTC"), by = "1 day"),
    value = rep(c(100, 200), each = 365L)
  )
  test_data = watplotter.core::standardize_dataset(rbind(test_dry, test_wet))

  result_data = analysis_duration(test_data, by_wycategory = TRUE)

})


test_that("duration by base year analysis works", {

  skip("test not completed")

  # sample data
  test_2007 = data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(2L), each = 365L),
    baseyear = factor(2007L, levels = c(1993L, 2007L)),
    date = seq(as.POSIXct("2001-10-01 00:00:00", tz = "UTC"),
      as.POSIXct("2003-09-30 00:00:00", tz = "UTC"), by = "1 day"),
    value = rep(c(10, 20), each = 365L)
  )
  test_1993 = data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq(3L, 4L), each = 365L),
    baseyear = factor(1993L, levels = c(1993L, 2007L)),
    date = seq(as.POSIXct("2005-10-01 00:00:00", tz = "UTC"),
      as.POSIXct("2007-09-30 00:00:00", tz = "UTC"), by = "1 day"),
    value = rep(c(100, 200), each = 365L)
  )
  test_data = watplotter.core::standardize_dataset(rbind(test_1993, test_2007))

  result_data = analysis_duration(test_data, by_baseyear = TRUE)

  expect_error(analysis_duration(test_data,
    by_wycategory = TRUE, by_baseyear = TRUE))

})


test_that("metric vs metric analysis works", {

  test_data = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), each = 5L),
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
        as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"),
      101L),
    value = rep(seq_len(5), 101L) + 10 * rep(seq_len(101L), each = 5L)
  ))

  result_data = analysis_metric_vs_metric(test_data, test_data, "max", "min")

  expect_setequal(result_data$metric.x, "max")
  expect_setequal(result_data$metric.y, "min")
  expect_equal(result_data$value.x, seq(15, 1015, by = 10))
  expect_equal(result_data$value.y, seq(11, 1011, by = 10))

})


test_that("metric vs metric by water year class analysis works", {

  test_dry = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), each = 5L),
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
    wycategory = factor("wet", levels = c("dry", "wet")),
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
        as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"),
      101L),
    value = rep(seq_len(5), 101L) + 10 * rep(seq_len(101L), each = 5L) + 50
  ))
  test_data = rbind(test_dry, test_wet)

  result_data = analysis_metric_vs_metric(test_data, test_data, "max", "min",
    by_wycategory = TRUE)

  expect_setequal(result_data$metric.x, "max")
  expect_setequal(result_data$metric.y, "min")
  expect_equal(
    result_data$value.x,
    rep(c(0, 50), 101L) + rep(seq(15, 1015, by = 10), each = 2)
  )
  expect_equal(
    result_data$value.y,
    rep(c(0, 50), 101L) + rep(seq(11, 1011, by = 10), each = 2)
  )
})


test_that("metric vs metric by base year analysis works", {

  test_2007 = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), each = 5L),
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
    baseyear = factor(1993L, levels = c(1993L, 2007L)),
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
        as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"),
      101L),
    value = rep(seq_len(5), 101L) + 10 * rep(seq_len(101L), each = 5L) + 50
  ))
  test_data = rbind(test_1993, test_2007)

  result_data = analysis_metric_vs_metric(test_data, test_data, "max", "min",
    by_baseyear = TRUE)

  expect_setequal(result_data$metric.x, "max")
  expect_setequal(result_data$metric.y, "min")
  expect_equal(
    result_data$value.x,
    rep(c(50, 0), 101L) + rep(seq(15, 1015, by = 10), each = 2)
  )
  expect_equal(
    result_data$value.y,
    rep(c(50, 0), 101L) + rep(seq(11, 1011, by = 10), each = 2)
  )

  expect_error(analysis_metric_vs_metric(test_data, test_data,
      "max", "min", by_wycategory = TRUE, by_baseyear = TRUE))
})


test_that("metric vs forecast analysis works", {

  test_data = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), each = 5L),
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
        as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"),
      101L),
    value = seq_len(505L)
  ))

  test_forecast_data = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "/FORECAST/XXX/XXX/STOR//IR-CENTURY/XXX/",
    event = seq_len(101L),
    period = "31jan",
    date = rep(as.POSIXct("2002-01-31 00:00:00", tz = "UTC"), 101L),
    value = rep(seq(10, 110, by = 10), each = 10L, length.out = 101L)
  ))
  result_data = analysis_metric_vs_forecast(test_data, test_forecast_data, "min")

  expect_setequal(result_data$metric, "min")
  expect_equal(result_data$value.m, seq(1, 501, by = 5))
  expect_equal(result_data$value.f,
    rep(seq(10, 110, by = 10), each = 10L, length.out = 101L))

})


test_that("metric vs forecast analysis by water year class works", {

  test_dry = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), each = 5L),
    wycategory = factor("dry", levels = c("dry", "average", "wet")),
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
      as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"), 101L),
    value = seq_len(505L)
  ))
  test_wet = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), each = 5L),
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
    value = rep(seq(10, 110, by = 10), each = 10L, length.out = 101L)
  ))
  result_data = analysis_metric_vs_forecast(test_data, test_forecast_data, "min",
    by_wycategory = TRUE)

  expect_setequal(result_data$metric, "min")

  skip("test not completed")

  expect_equal(result_data$value.m, seq(1, 501, by = 5))
  expect_equal(result_data$value.f,
    rep(seq(10, 110, by = 10), each = 10L, length.out = 101L))

})


test_that("metric vs forecast analysis by base year works", {

  test_2007 = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), each = 5L),
    baseyear = factor(2007L, levels = c(1993L, 2007L)),
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
      as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"), 101L),
    value = seq_len(505L)
  ))
  test_1993 = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), each = 5L),
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
    value = rep(seq(10, 110, by = 10), each = 10L, length.out = 101L)
  ))
  result_data = analysis_metric_vs_forecast(test_data, test_forecast_data, "min",
    by_baseyear = TRUE)

  expect_setequal(result_data$metric, "min")
  
  skip("test not completed")

  expect_equal(result_data$value.m, seq(1, 501, by = 5))
  expect_equal(result_data$value.f,
    rep(seq(10, 110, by = 10), each = 10L, length.out = 101L))

  expect_error(analysis_metric_vs_forecast(test_data, test_forecast_data, "min",
    by_wycategory = TRUE, by_baseyear = TRUE))

})


test_that("summary analysis works", {
  # sample data
  test_dry = data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(2L), each = 365L),
    date = seq(as.POSIXct("2001-10-01 00:00:00", tz = "UTC"),
      as.POSIXct("2003-09-30 00:00:00", tz = "UTC"), by = "1 day"),
    value = rep(c(10, 20), each = 365L)
  )
  test_wet = data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq(3L, 4L), each = 365L),
    date = seq(as.POSIXct("2005-10-01 00:00:00", tz = "UTC"),
      as.POSIXct("2007-09-30 00:00:00", tz = "UTC"), by = "1 day"),
    value = rep(c(100, 200), each = 365L)
  )
  wyclasses = data.frame(wateryear = seq(2001L, 2007L),
    wycategory = c(rep("dry", 3L), "average", rep("wet", 3L)))
  test_data = watplotter.core::assign_wycategory(
    watplotter.core::standardize_dataset(rbind(test_dry, test_wet)),
    wyclasses)

  out1 = suppressWarnings(summary_analysis("metric over time",
      df = test_data))
  out2 = summary_analysis("metric over time", df = test_data,
    metrics = "min", by_wycategory = TRUE)

  # the actual analysis functions are tested elsewhere, so just make
  # sure they are dispatched correctly
  expect_warning(expect_equal(out1, analysis_metrics(test_data)))
  expect_equal(out2, analysis_metrics(test_data, "min", TRUE))

})



test_that("trace analysis works", {

  test_data = watplotter.core::standardize_dataset(data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(101L), 5L),
    date = rep(seq(as.POSIXct("2002-01-01 12:00:00", tz = "UTC"),
        as.POSIXct("2002-01-05 12:00:00", tz = "UTC"), by = "1 day"),
      each = 101L),
    value = rep(seq(0, 100), 5L)
  ))

  expect_identical(
    analysis_trace(test_data),
    test_data[fields("alt", "path", "refdate", "event", "value")]
  )

  expect_identical(
    nrow(analysis_trace(test_data, sample = 10)),
    50L
  )

})


test_that("trace by water year class analysis works", {

  # sample data
  test_dry = data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(2L), each = 365L),
    wycategory = factor("dry", levels = c("dry", "wet")),
    date = seq(as.POSIXct("2001-10-01 00:00:00", tz = "UTC"),
      as.POSIXct("2003-09-30 00:00:00", tz = "UTC"), by = "1 day"),
    value = rep(c(10, 20), each = 365L)
  )
  test_wet = data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq(3L, 4L), each = 365L),
    wycategory = factor("wet", levels = c("dry", "wet")),
    date = seq(as.POSIXct("2005-10-01 00:00:00", tz = "UTC"),
      as.POSIXct("2007-09-30 00:00:00", tz = "UTC"), by = "1 day"),
    value = rep(c(100, 200), each = 365L)
  )
  test_data = watplotter.core::standardize_dataset(rbind(test_dry, test_wet))

  expect_identical(
    analysis_trace(test_data, by_wycategory = TRUE),
    test_data[fields("alt", "path", "wycategory", "refdate", "event", "value")]
  )

  expect_identical(
    nrow(analysis_trace(test_data, sample = 1, by_wycategory = TRUE)),
    730L
  )

})


test_that("trace by base year analysis works", {

  # sample data
  test_2007 = data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq_len(2L), each = 365L),
    baseyear = factor(2007L, levels = c(1993L, 2007L)),
    date = seq(as.POSIXct("2001-10-01 00:00:00", tz = "UTC"),
      as.POSIXct("2003-09-30 00:00:00", tz = "UTC"), by = "1 day"),
    value = rep(c(10, 20), each = 365L)
  )
  test_1993 = data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = rep(seq(3L, 4L), each = 365L),
    baseyear = factor(1993L, levels = c(1993L, 2007L)),
    date = seq(as.POSIXct("2005-10-01 00:00:00", tz = "UTC"),
      as.POSIXct("2007-09-30 00:00:00", tz = "UTC"), by = "1 day"),
    value = rep(c(100, 200), each = 365L)
  )
  test_data = watplotter.core::standardize_dataset(rbind(test_1993, test_2007))

  expect_identical(
    analysis_trace(test_data, by_baseyear = TRUE),
    test_data[fields("alt", "path", "baseyear", "refdate", "event", "value")]
  )

  expect_identical(
    nrow(analysis_trace(test_data, sample = 1, by_baseyear = TRUE)),
    730L
  )

  expect_error(analysis_trace(test_data,
      by_wycategory = TRUE, by_baseyear = TRUE))

})
