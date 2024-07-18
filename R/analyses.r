#' Check Groupings
#'
#' Check for valid grouping combination.
#'
#' @inheritParams analysis_metrics
#' @return `TRUE` (invisibily), or an error if grouping is invalid.
#'
#' @keywords internal
check_groupings = function(metrics, by_wycategory, by_baseyear) {

  multiple_metrics = length(metrics) > 1L
  if (by_wycategory && by_baseyear) {
    stop("Plotting functions support grouping by multiple base years ",
      "or multiple water year categories, but not both.")
  } else if (multiple_metrics && by_wycategory) {
    stop("Plotting functions support mutiple metrics and ",
      "multiple water year categories, but not both.")
  } else if (multiple_metrics && by_baseyear) {
    stop("Plotting functions support mutiple metrics and ",
      "multiple base years, but not both.")
  }
  invisible(TRUE)
}

#' Get Data Fields
#'
#' Check for required fields and return optional fields if available.
#'
#' @inheritParams analysis_metrics
#' @param required Required fields.
#' @param optional Optional fields.
#' @return A character vector of field names.
#'
#' @keywords internal
get_fields = function(df, required, optional = NULL) {
  assert_fields(df, required)
  c(required, intersect(optional, names(df)))
}

#' @rdname get_fields
#'
#' @inheritParams analysis_metric_vs_metric
#' @keywords internal
join_fields = function(df.x, df.y, required, optional) {
  intersect(
    get_fields(df.x, required, optional),
    get_fields(df.y, required, optional)
  )
}

#' Available Analyses
#'
#' Available analyses.
#'
#' @return A vector of analysis types.
#'
#' @export
available_analyses = function() {
  c("metric over time", "metric by frequency", "duration",
    "metric vs forecast", "metric vs metric", "trace")
}


#' Summary Analysis
#'
#' Generate a summary analysis. Provides a generic interface for
#' [analysis_metrics()], [analysis_frequency()], [analysis_duration()],
#' [analysis_metric_vs_metric()], and [analysis_metric_vs_forecast()].
#'
#' @param type The analysis type.
#' @param ... Additional arguments passed to the specified analysis
#'   generator.
#' @inheritDotParams analysis_metrics
#' @inheritDotParams analysis_frequency
#' @inheritDotParams analysis_duration
#' @inheritDotParams analysis_metric_vs_metric
#' @inheritDotParams analysis_metric_vs_forecast
#' @return A dataframe.
#'
#' @export
summary_analysis = function(type = available_analyses(), ...) {

  type = match.arg(tolower(type), available_analyses())
  switch(type,
    "metric over time" = analysis_metrics(...),
    "metric by frequency" = analysis_frequency(...),
    "duration" = analysis_duration(...),
    "metric vs forecast" = analysis_metric_vs_forecast(...),
    "metric vs metric" = analysis_metric_vs_metric(...),
    "trace" = analysis_trace(...)
  )
}


#' Metric Over Time Analysis
#'
#' Compute metrics over time for a dataset.
#'
#' @param df A dataset.
#' @inheritParams compute_metrics
#' @param by_wyclass If `TRUE`, compute metrics by water year category.
#' @param by_baseyear If `TRUE`, compute metrics by base year.
#' @return A dataframe with fields `"alt"`, (`"run"`), `"path"`,
#'   (`"period"`), `"refdate"`, (`"wycategory"`, `"baseyear"`),
#'   `"metric"`, and `"value"`.
#'
#' @importFrom watplotter.core assert_fields
#' @keywords internal
analysis_metrics = function(df, metrics = available_metrics(),
  by_wycategory = FALSE, by_baseyear = FALSE, na.rm = TRUE) {
  # input validation
  assert_unique(df, fields("path"))

  req_fields = fields("alt", "path", "refdate")

  check_groupings(metrics, by_wycategory, by_baseyear)
  if (by_wycategory) {
    req_fields = c(req_fields, fields("wycategory"))
  } else if (by_baseyear) {
    req_fields = c(req_fields, fields("baseyear"))
  }
  opt_fields = fields("run", "period")
  groups = get_fields(df, req_fields, opt_fields)

  compute_metrics(df, metrics = metrics, groups = groups,
    na.rm = na.rm)
}


#' Duration Curve Analysis
#'
#' @inheritParams analysis_metrics
#' @return A dataframe with fields `"alt"`, (`"run"`), `"path"`,
#'   (`"period"`, `"wycategory"`, `"baseyear"`), `"metric"`,
#'   "probability"`, and `"quantile"`.
#'
#' @importFrom watplotter.core assert_fields
#' @keywords internal
analysis_duration = function(df, by_wycategory = FALSE,
  by_baseyear = FALSE, na.rm = TRUE) {
  # input validation
  assert_unique(df, fields("path"))

  req_fields = fields("alt", "path")

  check_groupings(NULL, by_wycategory, by_baseyear)
  if (by_wycategory) {
    req_fields = c(req_fields, fields("wycategory"))
  } else if (by_baseyear) {
    req_fields = c(req_fields, fields("baseyear"))
  }
  opt_fields = fields("run", "period")
  groups = get_fields(df, req_fields, opt_fields)

  compute_duration(df, groups = groups, na.rm = na.rm)
}


#' Frequency Curve Analysis
#'
#' Generate frequency analyses for each metric.
#'
#' @inheritParams analysis_metrics
#' @return A dataframe with fields `"alt"`, (`"run"`), `"path"`,
#'   (`"period"`, `"wycategory"`, `"baseyear"`), `"metric"`,
#'    `"probability"` and `"quantile"`.
#'
#' @importFrom watplotter.core rename_fields assert_fields
#' @importFrom dplyr ungroup mutate group_by across all_of
#' @importFrom rlang .data
#' @keywords internal
analysis_frequency = function(df, metrics = available_metrics(),
  by_wycategory = FALSE, by_baseyear = FALSE, na.rm = TRUE) {
  # input validation
  assert_unique(df, fields("path"))

  req_metric_fields = fields("alt", "path", "event")
  req_freq_fields = fields("alt", "path", "metric")

  check_groupings(metrics, by_wycategory, by_baseyear)
  if (by_wycategory) {
    req_metric_fields = c(req_metric_fields, fields("wycategory"))
    req_freq_fields = c(req_freq_fields, fields("wycategory"))
  } else if (by_baseyear) {
    req_metric_fields = c(req_metric_fields, fields("baseyear"))
    req_freq_fields = c(req_freq_fields, fields("baseyear"))
  }
  opt_fields = fields("run", "period")

  metric_groups = get_fields(df, req_metric_fields, opt_fields)
  metric_df = compute_metrics(df, metrics = metrics,
    groups = metric_groups, na.rm = na.rm)

  freq_groups = get_fields(metric_df, req_freq_fields, opt_fields)
  compute_frequency(metric_df, groups = freq_groups, na.rm = na.rm)
}


#' Metric vs Metric Analysis
#'
#' Calculate metrics of two datasets and join by reference date.
#'
#' @param df.x The dataset for the first axis.
#' @param df.y The dataset for the second axis.
#' @param metrics.x The metric for the first axis.
#' @param metrics.y The metric for the second axis.
#' @inheritParams analysis_metrics
#' @return A dataframe with fields `"alt"`, (`"run"`), `"path.x"`,
#'   `"path.y"`, (`"period.x"`), (`"period.y"`), `"refdate"`,
#'   (`"wycategory"`, `"baseyear"`), `"metric.x"`, `"metric.y"`,
#'   `"value.x"`, and `"value.y"`.
#'
#' @importFrom watplotter.core assert_fields rename_fields
#' @importFrom dplyr full_join
#' @keywords internal
analysis_metric_vs_metric = function(df.x, df.y, metrics.x, metrics.y,
  by_wycategory = FALSE, by_baseyear = FALSE, na.rm = TRUE) {
  # input validation
  assert_unique(df.x, fields("path"))
  assert_unique(df.y, fields("path"))
  # only one metric per dataset supported
  if (length(metrics.x) != 1L || length(metrics.y) != 1L) {
    stop("Arguments \"metrics.x\" and \"metrics.y\" must each be length 1")
  }

  req_fields = fields("alt", "event", "path")
  check_fields = fields("event")
  check_groupings(metrics.x, by_wycategory, by_baseyear)
  check_groupings(metrics.y, by_wycategory, by_baseyear)
  if (by_wycategory) {
    req_fields = c(req_fields, fields("wycategory"))
  } else if (by_baseyear) {
    req_fields = c(req_fields, fields("baseyear"))
  }
  opt_fields = fields("run", "period")

  groups.x = get_fields(df.x, req_fields, opt_fields)
  groups.y = get_fields(df.y, req_fields, opt_fields)

  h.x = compute_metrics(df.x, metrics = metrics.x, groups = groups.x,
    na.rm = na.rm)
  h.y = compute_metrics(df.y, metrics = metrics.y, groups = groups.y,
    na.rm = na.rm)

  # fix for period
  h.x = rename_fields(h.x, fields("period"),
    paste0(fields("period"), ".x"))
  h.y = rename_fields(h.y, fields("period"),
    paste0(fields("period"), ".y"))

  # define fields used to combine data
  by_fields = join_fields(h.x, h.y, fields("alt", "event"),
    fields("run", "wycategory", "baseyear"))

  # join datasets
  full_join(h.x, h.y, by = by_fields, suffix = c(".x", ".y"))
}


#' Metric vs Forecast Analysis
#'
#' Calculate metrics of a dataset and compares against a forecast.
#'
#' @param df.m The dataset to calculate the metric on.
#' @param df.f The forecast dataset.
#' @inheritParams analysis_metrics
#' @return A dataframe with fields `"alt"`, (`"run"`),
#'   `"path.m"`, `"path.f"`, (`"period.m"`, `"period.f"`),
#'   `"event`", `"date"`, (`"wateryear"`),
#'   `"refdate"`, (`"wycategory"`, `"baseyear"`), `"metric"`,
#'   `"value.m"`, and `"value.f"`.
#'
#' @importFrom dplyr full_join count across select any_of
#' @keywords internal
analysis_metric_vs_forecast = function(df.m, df.f, metrics,
  by_wycategory = FALSE, by_baseyear = FALSE, na.rm = TRUE) {
  # only one metric per dataset supported
  if (length(metrics) != 1L) {
    stop("Argument \"metrics\" must be length 1")
  }

  # define fields used to combine data
  by_fields = join_fields(df.m, df.f, fields("alt", "event"),
    fields("run"))

  assert_single(df.f, by_fields)

  #compute metric for df.x
  req_fields = fields("alt", "event", "path")

  check_groupings(metrics, by_wycategory, by_baseyear)
  if (by_wycategory) {
    req_fields = c(req_fields, fields("wycategory"))
  } else if (by_baseyear) {
    req_fields = c(req_fields, fields("baseyear"))
  }
  opt_fields = fields("run", "period")

  groups = get_fields(df.m, req_fields, opt_fields)
  h.m = compute_metrics(df.m, metrics = metrics, groups = groups,
    na.rm = na.rm)
  h.f = select(df.f, any_of(fields("alt", "run", "path", "event",
        "period", "date", "value", "refdate", "wateryear")))

  #join the data and forecast
  full_join(
    rename_fields(h.m, fields("period"), paste0(fields("period"), ".m")),
    rename_fields(h.f, fields("period"), paste0(fields("period"), ".f")),
    by = by_fields, suffix = c(".m", ".f"))
}


#' Trace Analysis
#'
#' Extract traces from a dataset.
#'
#' @inheritParams analysis_duration
#' @param sample The maximum number of traces per group to return.
#' @return A dataframe with fields `"alt"`, (`"run"`), `"path"`,
#'   (`"period"`), `"event"`, `"date"`, `"refdate"`,
#'   (`"wycategory"`, `"baseyear"`), and `"value"`.
#'
#' @importFrom dplyr select filter distinct inner_join slice_sample
#'   across all_of
#' @importFrom rlang .data
#' @keywords internal
analysis_trace = function(df, sample = Inf, by_wycategory = FALSE,
  by_baseyear = FALSE, na.rm = TRUE) {

  # input validation
  assert_unique(df, fields("path"))
  assert_fields(df, fields("refdate", "event", "value"))
  req_fields = fields("alt", "path")

  check_groupings(NULL, by_wycategory, by_baseyear)
  if (by_wycategory) {
    req_fields = c(req_fields, fields("wycategory"))
  } else if (by_baseyear) {
    req_fields = c(req_fields, fields("baseyear"))
  }
  opt_fields = fields("run", "period")
  groups = get_fields(df, req_fields, opt_fields)

  if (is.finite(sample)) {
    if (sample < 0L) {
      stop("Argument \"sample\" must be greater than 0.")
    }
    all_events = distinct(df, across(all_of(c(groups, fields("event")))))
    sampled_events = ungroup(slice_sample(
      group_by(all_events, across(all_of(groups))),
      n = sample, replace = FALSE
    ))
    df = inner_join(df, sampled_events, by = names(sampled_events))
  }
  if (na.rm) {
    df = filter(df, !is.na(.data[[fields("value")]]))
  }

  select(df, all_of(c(groups, fields("refdate", "event", "value"))))
}
