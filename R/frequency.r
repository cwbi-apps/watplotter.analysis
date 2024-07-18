#' Comput Frequency
#'
#' Compute frequencies from a dataset.
#'
#' @param df A dataframe.
#' @param groups The grouping to use when computing frequencies.
#' @param probs The output (non-exceedance) probabilities.
#' @param na.rm If `TRUE`, remove `NA` values before computing frequencies.
#' @return A dataframe, with specified `groups` fields and additional
#'   fields (non-exceedance) `"probability"` and `"quantile"`.
#'
#' @importFrom dplyr mutate filter group_by group_map bind_rows
#'   bind_cols across all_of
#' @importFrom stats approx
#' @keywords internal
compute_frequency = function(df, groups, probs = seq(0.001, 0.999, 0.001),
  na.rm = TRUE) {
  assert_fields(df, c(groups, fields("value")))
  if (na.rm) {
    df = filter(df, !is.na(.data[[fields("value")]]))
  }
  # group data and compute frequencies
  grouped_df = mutate(group_by(df, across(all_of(groups))),
    p = weibull_position(.data[[fields("value")]]))
  # interpolate frequency curve
  freq_list = group_map(grouped_df,
    function(x, y)
      bind_cols(y, approx(x[order(x[["p"]]), c("p", fields("value"))],
        xout = probs, ties = "ordered", na.rm = na.rm)))
  # clean up output
  rename_fields(bind_rows(freq_list), c("x", "y"),
    fields("probability", "quantile"))
}
