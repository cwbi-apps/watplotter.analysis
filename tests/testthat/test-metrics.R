test_that("available metrics work", {

  # fail if available metrics have been modified
  expect_identical(available_metrics(), c("mean", "median", "min",
    "max", "1% exceedance", "10% exceedance", "25% exceedance",
    "50% exceedance", "75% exceedance", "90% exceedance",
    "99% exceedance"))

  # metric_functions() will not return both median and 50% exceedance
  # so expected length is one less than available_metrics()
  expect_warning(metric_functions())
  expect_vector(suppressWarnings(metric_functions()), list(function() {}),
    length(available_metrics()) - 1L)
  expect_setequal(names(suppressWarnings(metric_functions(available_metrics()))),
    setdiff(available_metrics(), "50% exceedance"))
  expect_setequal(names(metric_functions(c("mean", "50% exceedance", "max"))),
    c("mean", "50% exceedance", "max"))
  expect_error(metric_functions("foo"))

})


test_that("standard quantile works", {

  testx = seq(0, 100, by = 1)
  testq = c(0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99)
  outq = c(0.51, 9.6, 24.75, 50, 75.25, 90.4, 99.49)

  expect_equal(standard_quantile(testx, testq), outq,
    ignore_attr = "names")
  expect_equal(standard_quantile(testx, 1 - testq), rev(outq),
    ignore_attr = "names")

  expect_error(standard_quantile(testx, testq, type = 4L))

})


test_that("weibull probabilities works", {

  testx2 = c(1, 5, 11, 17, 22, 33, 42, 56, 98)
  outx2 = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
  expect_equal(weibull_position(testx2), outx2)

})


test_that("normalize works", {

  expect_equal(
    sum(normalize(rnorm(100), 1)),
    1
  )
  expect_equal(
    sum(normalize(rnorm(100), 2.5)),
    2.5
  )
  expect_equal(
    normalize(rep(10, 10), 10),
    rep(1, 10)
  )
  expect_equal(
    normalize(rep(10, 10), 100),
    rep(10, 10)
  )
  expect_equal(
    normalize(seq_len(5), 3),
    c(0.2, 0.4, 0.6, 0.8, 1.0)
  )
  expect_equal(
    normalize(seq_len(5), 0),
    rep(0, 5)
  )
  expect_equal(
    normalize(c(seq_len(4), NA), 1),
    rep(NA_real_, 5)
  )
  expect_error(normalize(LETTERS[1:5], 1))

})



test_that("ordered cumulative sum works", {

  # cumulative sum
  expect_equal(ordered_cumsum(1:5), cumsum(1:5))
  expect_equal(ordered_cumsum(1), cumsum(1))
  expect_equal(ordered_cumsum(numeric(0)), cumsum(numeric(0)))

  # ordering
  test2 = c(0.2, 0.7, 0.1)
  idx2 = c(3L, 1L, 2L)
  out2 = c(0.3, 1, 0.1)

  expect_equal(ordered_cumsum(test2, order_by = idx2), out2)
  expect_error(ordered_cumsum(test2, order_by = rep(5, 3)))
  expect_error(ordered_cumsum(test2, order_by = c(idx3, idx3)))

})
