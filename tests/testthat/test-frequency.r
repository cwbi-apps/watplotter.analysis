test_that("frequency calculation works", {

  test_data = data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = seq_len(1000L),
    metric = "max",
    refdate = as.POSIXct("3001-04-30 00:00:00", tz = "UTC"),
    value = seq(1000L)
  )

  result = compute_frequency(test_data, c("alt", "path", "metric"),
    c(0.001, 0.01, 0.1, 0.5, 0.9, 0.99, 0.999))

  expect_identical(
    names(result),
    c("alt", "path", "metric", "probability", "quantile")
  )
  expect_equal(
    result$probability,
    c(0.001, 0.01, 0.1, 0.5, 0.9, 0.99, 0.999)
  )
  expect_equal(
    result$quantile,
    c(1.001, 10.01, 100.1, 500.5, 900.9, 990.99, 999.999)
  )

})
