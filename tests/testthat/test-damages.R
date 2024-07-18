test_that("energy price works", {
  skip("not finished")
  df1 = data.frame(date = c(seq(as.POSIXct("1981-05-01 12:00:00",
    tz = "UTC"), as.POSIXct("2021-09-30 12:00:00", tz = "UTC"),
    length.out = 10), as.POSIXct("2020-02-29 14:00:00")),
    value = rnorm(11))



})


test_that("simple annual damages works", {
  skip("not finished")
  x1 = c(10, 3, 40, 399.99, 400, 800, NA_real_) * 1000
  outx1 = c(c(0.01, 0.01, 0.01, 0.01, NA_real_, 39.783358, NA_real_))

  # shouldn't EAD = 0.01 at 400,000 cfs?
  expect_equal(annual_damages_simple(x1), outx1, tolerance = 1e-5)

})
