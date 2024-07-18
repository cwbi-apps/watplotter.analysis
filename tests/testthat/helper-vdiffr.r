# visual tests are skipped unless if vdiffr is installed
if (requireNamespace("vdiffr", quietly = TRUE) && utils::packageVersion("testthat") >= "3.0.3") {
  expect_doppelganger <- vdiffr::expect_doppelganger
} else {
  # Otherwise, assign a dummy function
  expect_doppelganger <- function(...) skip("vdiffr is not installed.")
}
