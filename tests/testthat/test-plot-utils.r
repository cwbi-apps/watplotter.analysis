test_that("color generation works", {

  test1 = data.frame(alt = c("alt1", "alt2"))
  test2 = data.frame(alt = "alt", run = c("one", "two"))

  expect_equal(
    generate_colors(test1),
    cbind(test1, color = c("#1B9E77", "#D95F02")),
    ignore_attr = c("class")
  )
  expect_equal(
    generate_colors(test2),
    cbind(test2, color = c("#077F5E", "#42B38C")),
    ignore_attr = c("class")
  )
  expect_equal(
    generate_colors(test1, c(alt1 = "blue", alt2 = "red")),
    cbind(test1, color = c("blue", "red")),
    ignore_attr = c("class")
  )
  expect_equal(
    generate_colors(test2, c(alt = "red")),
    cbind(test2, color = c("#CE0505", "#FF6060")),
    ignore_attr = c("class")
  )
  expect_error(generate_colors(test1, c(alt1 = "red")))

})
