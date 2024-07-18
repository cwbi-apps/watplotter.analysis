#' Compute Duration
#'
#' Compute durations from a dataset.
#'
#' @param df A dataframe.
#' @param groups The grouping to use when computing durations.
#' @param probs The output (non-exceedance) probabilities.
#' @param na.rm If `TRUE`, remove `NA` values before computing durations.
#' @return A dataframe, with specified `groups` fields and additional
#'   fields (non-exceedance) `"probability"` and `"quantile"`.
#'
#' @importFrom dplyr mutate group_by group_map bind_rows bind_cols
#'   rename across all_of percent_rank select
#' @importFrom rlang .data
#' @importFrom stats approx
#' @keywords internal
compute_duration = function(df, groups, probs = seq(0, 1, 0.001),
  na.rm = TRUE) {
  assert_fields(df, c(groups, fields("value")))
  if (na.rm) {
    df = filter(df, !is.na(.data[[fields("value")]]))
  }
  # compute non-exceedance by group
  grouped_df = mutate(group_by(df, across(all_of(groups))),
    p = percent_rank(.data[[fields("value")]]))
  # interpolate duration curve
  dur_list = group_map(grouped_df,
    function(x, y)
      bind_cols(y, approx(x[order(x[["p"]]), c("p", fields("value"))],
        xout = probs, ties = "ordered", na.rm = na.rm)))
  # clea up output
  rename_fields(bind_rows(dur_list), c("x", "y"),
    fields("probability", "quantile"))
}
