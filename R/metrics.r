#' Available Metrics
#'
#' Metrics available for computation.
#'
#' @return A character vector of available metrics.
#'
#' @details Descriptions of the various functions are included below:
#' - `"p% exceedance"`: return the `p`% exceedance value based on the
#'   CRT standard quantile calculation, i.e.,
#'   `standard_quantile(x, probs = 1 - p)`. See [standard_quantile()]
#'   for more information. `1%`, `10%`, `25%`, `50%`, `75%`, `90%`,
#'   and `99%` exceedance probabilities are supported.
#' - `"mean"`: Return the average value.
#' - `"min"`: Return the minimum value.
#' - `"max"`: Return the maximum value.
#' - `"median"`: return the median value. Equivalent to `"50% exceedance"`.
#'
#' @export
available_metrics = function() {
  c("mean", "median", "min", "max", "1% exceedance", "10% exceedance",
    "25% exceedance", "50% exceedance", "75% exceedance",
    "90% exceedance", "99% exceedance")
}


#' Compute Metrics
#'
#' Compute metrics for a dataset.
#'
#' @param df A dataframe.
#' @param metrics A vector of metrics to compute.
#' @param groups The grouping to use when computing metrics.
#' @param na.rm If `TRUE`, remove `NA` values before computing metrics.
#' @return A dataframe, with field "date" is replaced with
#'   "wydate".
#'
#' @importFrom watplotter.core rename_fields
#' @importFrom dplyr summarize mutate group_by bind_rows
#' @importFrom purrr map
#' @importFrom rlang .data .env
#' @keywords internal
compute_metrics = function(df, metrics, groups, na.rm = TRUE) {

  # get list of metric functions
  assert_fields(df, c(groups, fields("value")))
  mfuns = metric_functions(metrics, na.rm = na.rm)

  # apply groups
  grouped_df = group_by(df, across(all_of(groups)))
  # calculate metrics
  metrics_df = bind_rows(map(mfuns, function(fun)
    summarize(grouped_df, value = fun(.data[[fields("value")]]),
      .groups = "drop")),
    .id = "metric"
  )
  # cleanup output
  rename_fields(mutate(metrics_df, metric = factor(.data$metric,
    .env$metrics)), "metric", fields("metric"))
}


#' Metric Functions
#'
#' Get List of metric functions.
#'
#' @param metrics A character vector of metric functions to return.
#'   Default is to return all available metrics.
#' @inheritParams base::min
#' @return A named list of functions.
#'
#' @keywords internal
metric_functions = function(metrics = available_metrics(), na.rm = TRUE) {
  # check for unknown metrics
  unknown_metrics = setdiff(metrics, available_metrics())
  if (length(unknown_metrics) > 0L) {
    stop("Unrecognized metrics: ", paste(shQuote(unknown_metrics),
      collapse = ", "))
  }
  # remove duplicate metrics
  if (all(c("50% exceedance", "median") %in% metrics)) {
    warning("Both \"median\" and \"50% exceedance\" requested, ",
      "returning \"median\" only")
    metrics = setdiff(metrics, "50% exceedance")
  }
  # complete list of metrics
  all_funs = list(
    "1% exceedance" =  function(x)
      standard_quantile(x, 1 - 0.01, na.rm = na.rm),
    "10% exceedance" = function(x)
      standard_quantile(x, 1 - 0.1, na.rm = na.rm),
    "25% exceedance" = function(x)
      standard_quantile(x, 1 - 0.25, na.rm = na.rm),
    "50% exceedance" = function(x)
      standard_quantile(x, 1 - 0.50, na.rm = na.rm),
    "75% exceedance" = function(x)
      standard_quantile(x, 1 - 0.75, na.rm = na.rm),
    "90% exceedance" = function(x)
      standard_quantile(x, 1 - 0.90, na.rm = na.rm),
    "99% exceedance" = function(x)
      standard_quantile(x, 1 - 0.99, na.rm = na.rm),
    "mean" = function(x)
      mean(x, na.rm = na.rm),
    "min" =  function(x)
      min(x, na.rm = na.rm),
    "max" =  function(x)
      max(x, na.rm = na.rm),
    "median" = function(x)
      standard_quantile(x, 0.50, na.rm = na.rm)
  )
  # return specified subset
  all_funs[metrics]
}


#' CRT Standard Quantile
#'
#' Standardized quantile calculation for CRT work. The primary change
#' from the default [stats::quantile()] is specification of `type = 5`
#' and removing NA values by default (`na.rm = TRUE`).
#'
#' @inheritParams stats::quantile
#'
#' @importFrom stats quantile
#' @keywords internal
standard_quantile = function(x, probs, ...) {
  if ("type" %in% names(list(...))) {
    stop("Argument 'type' cannot be overridden.")
  }
  quantile(x, probs, type = 5L, ...)
}


#' Weibull Plotting Position
#'
#' Calculate the Weibull plotting positions of a vector.
#'
#' @param x A numeric vector.
#' @return A vector of (non-exceedance) probabilities.
#'
#' @details This implementation uses [rank()] with the following
#'   parameters:
#'   - Ties are ranked in order of occurrence in the data
#'     (`ties.method = "first"`)
#'   - `NA` values are put first.
#'
#' @keywords internal
weibull_position = function(x) {
  rank(x, ties.method = "first", na.last = FALSE) / (length(x) + 1L)
}


#' Normalize Vector
#'
#' Normalize a vector so that it sums to a target value.
#'
#' @param x A numeric vector.
#' @param target The target value that the new vector will sum to.
#'   Default is 1.
#' @return The argument `x`, rescaled to sum to 1.
#'
#' @keywords internal
normalize = function(x, target = 1) {
  x * target / sum(x)
}


#' Ordered Cumulative Sum
#'
#' Perform a cumulative sum of a vector given a specified element order.
#'
#' @param x A numeric vector.
#' @param order_by An integer vector to order `x` by during the
#'   cumulative sum. Default is to preserve order of `x`, resulting in
#'   identical behavior to [base::cumsum()]. To order from smallest to
#'   largest, use `order_by = order(x)`.
#' @return The cumulative sum of `x with respect to the specified
#'   ordering.
#'
#' @keywords internal
ordered_cumsum = function(x, order_by = seq_along(x)) {
  if (!identical(seq_along(x), sort(order_by))) {
    stop("Argument 'order_by' is not an index vector for 'x'.")
  }
  cumsum(x[order_by])[order(order_by)]
}
