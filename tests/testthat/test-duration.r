test_that("duration calculation works", {

  test_data = data.frame(
    alt = "PA",
    path = "XXX.XXX-XXX.Ave.~1Day.1Day.XXX",
    event = seq_len(1001L),
    refdate = as.POSIXct("3001-04-30 00:00:00", tz = "UTC"),
    value = seq(0, 1000)
  )

  result = compute_duration(test_data, c("alt", "path"),
    seq(0, 1, 0.01))

  expect_identical(
    names(result),
    c("alt", "path", "probability", "quantile")
  )
  expect_equal(
    result$probability,
    seq(0, 1, 0.01)
  )
  expect_equal(
    result$quantile,
    seq(0, 1000, by = 10)
  )

})
