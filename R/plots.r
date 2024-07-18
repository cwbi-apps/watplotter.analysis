#' Render Summary
#'
#' Render a summary plot as an interactive HTML widget.
#'
#' @param plot A summary_analysis plot object, i.e., output of
#'   [summary_plot()].
#' @return a `plotly` object, i.e., output of [plotly::ggplotly()].
#'
#' @export
summary_render = function(plot) {
  if (!requireNamespace("plotly")) {
    stop("Package \"plotly\" is required ",
      "to render a summary plot.")
  }
  plotly = plotly::ggplotly(plot, tooltip = c("text"))
  plotly::layout(plotly, hovermode = "closest")
}


#' Save Summary
#'
#' Save a summary analysis.
#'
#' @inheritParams summary_render
#' @param filename The output filename, including extension. Supports
#'   formats `"csv"`, `"png"`, `"html"`, and `"rds"`.
#' @param ... For future expansion. Currently ignored.
#' @return A file path.
#'
#' @importFrom ggplot2 ggsave
#' @importFrom stringr str_ends
#' @importFrom utils write.csv
#' @export
summary_save = function(plot, filename, ...) {
  formats = c("csv", "png", "html", "rds")
  which_format = which(str_ends(filename, sprintf("\\.%s", formats)))
  if (any(which_format)) {
    format = formats[which_format]
  } else {
    stop("Unrecognized file format. Must be one of: ",
      paste(shQuote(formats), collapse = ", "), ".")
  }
  if (format == "csv") {
    write.csv(plot$data, file = filename, na = "",
      row.names = FALSE)
  } else if (format == "png") {
    ggsave(filename, plot = plot, width = 10, height = 7.5)
  } else if (format == "html") {
    plotly = summary_render(plot)
    if (!requireNamespace("htmlwidgets")) {
      stop("Package \"htmlwidgets\" is required ",
        "to export to HTML.")
    }
    # need to work in output directory for selfcontained to work properly
    # see https://github.com/ramnathv/htmlwidgets/issues/296
    cwd = getwd()
    on.exit(setwd(cwd), add = TRUE)
    setwd(dirname(filename))
    htmlwidgets::saveWidget(plotly, file = basename(filename),
      selfcontained = TRUE)
  } else {
    saveRDS(list(data = plot$data, plot = plot), file = filename)
  }
}


#' Summary Plot
#'
#' Visualize a summary analysis. Provides a generic interface for
#' [plot_metrics()], [plot_frequency()], [plot_duration()],
#' [plot_metric_vs_metric()], and [plot_metric_vs_forecast()].
#'
#' @inheritParams summary_analysis
#' @param df A dataset, i.e., output of [summary_analysis()].
#' @param ... Additional arguments passed to [plot_overrides()].
#' @inheritParams generate_colors
#' @inheritDotParams plot_metrics
#' @inheritDotParams plot_frequency
#' @inheritDotParams plot_duration
#' @inheritDotParams plot_metric_vs_metric
#' @inheritDotParams plot_metric_vs_forecast
#' @return A dataframe.
#'
#' @importFrom purrr set_names
#' @export
summary_plot = function(type, df, ..., alt_colors) {
  type = match.arg(tolower(type), available_analyses())
  alt_colors = generate_colors(df, alt_colors)
  switch(type,
    "metric over time" = plot_metrics(df, ...,
      alt_colors = alt_colors),
    "metric by frequency" = plot_frequency(df, ...,
      alt_colors = alt_colors),
    "duration" = plot_duration(df, ...,
      alt_colors = alt_colors),
    "metric vs forecast" = plot_metric_vs_forecast(df, ...,
      alt_colors = alt_colors),
    "metric vs metric" = plot_metric_vs_metric(df, ...,
      alt_colors = alt_colors),
    "trace" = plot_trace(df, ..., alt_colors = alt_colors)
  )
}


#' Metrics Over Time Plot
#'
#' Plot a metrics over time analysis.
#'
#' @inheritParams summary_plot
#' @return A `ggplot` object.
#'
#' @importFrom ggplot2 ggplot aes geom_line ggtitle element_blank
#' @importFrom scales comma
#' @importFrom rlang .data
#' @keywords internal
plot_metrics = function(df, ..., alt_colors) {
  assert_fields(df, fields("alt", "path", "metric", "value"))
  settings = plot_overrides(...)
  add_themes = list()
  if (is.null(settings[["ylabel"]])) {
    settings[["ylabel"]] = path_to_label(unique(df$path))
  }
  if (is.null(settings[["xlabel"]])) {
    add_themes[["axis.title.x"]] = element_blank()
  }
  group_fields = get_fields(df, fields("alt", "metric"),
    fields("run", "wycategory", "baseyear", "period"))
  df["aes.group"] = build_aes_groups(df, group_fields)
  df["aes.text"] = paste0(
    df$aes.group, "\n",
    format(df[[fields("refdate")]], refdate_label), ": ",
    comma(df[[fields("value")]], accuracy = 0.01)
  )
  guide = guide_watplotter(df[[fields("alt")]], df$aes.group)

  ggplot(df) +
    aes(x = .data[[fields("refdate")]], y = .data[[fields("value")]],
      color = .data$aes.group,
      linetype = .data$aes.group,
      linewidth = .data$aes.group,
      group = .data$aes.group,
      text = .data$aes.text) +
    geom_line() +
    scale_line_group(df$aes.group, guide = guide,
      wycategories = df[[fields("wycategory")]],
      baseyears = df[[fields("baseyear")]]) +
    scale_color_alt(df$aes.group, alt_colors, guide = guide) +
    scale_x_refdate(settings[["xlabel"]]) +
    scale_y_value(settings[["ylabel"]], settings[["yshift"]],
      settings[["ydivisor"]]) +
    ggtitle(settings[["title"]]) +
    theme_watplotter(add_themes)
}


#' Duration Plot
#'
#' Plot a duration analysis.
#'
#' @inheritParams plot_metrics
#'
#' @importFrom ggplot2 ggplot aes geom_line ggtitle
#' @importFrom scales percent
#' @importFrom rlang .data
#' @keywords internal
plot_duration = function(df, ..., alt_colors) {
  assert_fields(df, fields("alt", "path", "probability", "quantile"))
  settings = plot_overrides(...)
  if (is.null(settings[["ylabel"]])) {
    settings[["ylabel"]] = path_to_label(unique(df[[fields("path")]]))
  }
  if (is.null(settings[["xlabel"]])) {
    settings[["xlabel"]] = "Duration of Exceedance"
  }
  group_fields = get_fields(df, fields("alt"),
    fields("run", "wycategory", "baseyear", "period"))
  df["aes.group"] = build_aes_groups(df, group_fields)
  df["aes.text"] = paste0(
    df$aes.group, "\n",
    percent(df[[fields("probability")]], accuracy = 0.01), ": ",
    comma(df[[fields("quantile")]], accuracy = 0.01)
  )
  guide = guide_watplotter(df[[fields("alt")]], df$aes.group)

  ggplot(df) +
    aes(x = .data[[fields("probability")]],
      y = .data[[fields("quantile")]],
      color = .data$aes.group,
      linetype = .data$aes.group,
      linewidth = .data$aes.group,
      group = .data$aes.group,
      text = .data$aes.text) +
    geom_line() +
    scale_line_group(df$aes.group, guide = guide,
      wycategories = df[[fields("wycategory")]],
      baseyears = df[[fields("baseyear")]]) +
    scale_color_alt(df$aes.group, alt_colors, guide = guide) +
    scale_x_duration(settings[["xlabel"]]) +
    scale_y_value(settings[["ylabel"]], settings[["yshift"]],
      settings[["ydivisor"]]) +
    ggtitle(settings[["title"]]) +
    theme_watplotter()
}


#' Frequency Curve Plot
#'
#' Plot a frequency analysis.
#'
#' @inheritParams plot_metrics
#'
#' @importFrom ggplot2 ggplot aes geom_line ggtitle
#' @importFrom scales percent
#' @importFrom rlang .data
#' @keywords internal
plot_frequency = function(df, ..., alt_colors) {
  assert_fields(df, fields("alt", "path", "metric", "probability", "quantile"))
  settings = plot_overrides(...)
  if (is.null(settings[["ylabel"]])) {
    settings[["ylabel"]] = path_to_label(unique(df[[fields("path")]]))
  }
  if (is.null(settings[["xlabel"]])) {
    settings[["xlabel"]] = "Frequency of Exceedance"
  }
  group_fields = get_fields(df, fields("alt", "metric"),
    fields("run", "wycategory", "baseyear", "period"))
  df["aes.group"] = build_aes_groups(df, group_fields)
  df["aes.text"] = paste0(
    df$aes.group, "\n",
    percent(df[[fields("probability")]], accuracy = 0.01), ": ",
    comma(df[[fields("quantile")]], accuracy = 0.01)
  )
  guide = guide_watplotter(df[[fields("alt")]], df$aes.group)

  ggplot(df) +
    aes(x = .data[[fields("probability")]],
      y = .data[[fields("quantile")]],
      color = .data$aes.group,
      linetype = .data$aes.group,
      linewidth = .data$aes.group,
      group = .data$aes.group,
      text = .data$aes.text) +
    geom_line() +
    scale_line_group(df$aes.group, guide = guide,
      wycategories = df[[fields("wycategory")]],
      baseyears = df[[fields("baseyear")]]) +
    scale_color_alt(df$aes.group, alt_colors, guide = guide) +
    scale_x_frequency(settings[["xlabel"]]) +
    scale_y_value(settings[["ylabel"]], settings[["yshift"]],
      settings[["ydivisor"]]) +
    ggtitle(settings[["title"]]) +
    theme_watplotter()
}


#' Metric vs Metric Plot
#'
#' @inheritParams analysis_metrics
#' @inheritDotParams plot_overrides
#' @return A `ggplot` object.
#'
#' @importFrom ggplot2 ggplot aes geom_point ggtitle
#' @importFrom stringr str_squish
#' @importFrom rlang .data
#' @keywords internal
plot_metric_vs_metric = function(df, ..., alt_colors) {
  assert_fields(df, c(fields("alt"),
    paste0(fields("path", "metric", "value"), c(".x", ".y"))))
  settings = plot_overrides(...)
  if (is.null(settings[["xlabel"]])) {
    xpath = path_to_label(unique(df[[paste0(fields("path"), ".x")]]))
    xmetric = sprintf("(%s)", unique(df[[paste0(fields("metric"), ".x")]]))
    xperiod = sprintf("[%s]", unique(df[[paste0(fields("period"), ".x")]]))
    settings[["xlabel"]] = str_squish(paste(xpath, xperiod, xmetric))
  }
  if (is.null(settings[["ylabel"]])) {
    ypath = path_to_label(unique(df[[paste0(fields("path"), ".y")]]))
    ymetric = sprintf("(%s)", unique(df[[paste0(fields("metric"), ".y")]]))
    yperiod = sprintf("[%s]", unique(df[[paste0(fields("period"), ".y")]]))
    settings[["ylabel"]] = str_squish(paste(ypath, yperiod, ymetric))
  }
  group_fields = get_fields(df, fields("alt"),
    fields("run", "wycategory", "baseyear"))
  df["aes.group"] = build_aes_groups(df, group_fields)
  df["aes.text"] = paste0(
    sprintf("%s : Event %d", df$aes.group, df[[fields("event")]]),
    "\n",
    sprintf("\t%s (%s): %s", df[[paste0(fields("path"), ".x")]],
      df[[paste0(fields("metric"), ".x")]],
      comma(df[[paste0(fields("value"), ".x")]], accuracy = 0.01)),
    "\n",
    sprintf("\t%s (%s): %s", df[[paste0(fields("path"), ".y")]],
      df[[paste0(fields("metric"), ".y")]],
      comma(df[[paste0(fields("value"), ".y")]], accuracy = 0.01))
  )
  guide = guide_watplotter(df[[fields("alt")]], df$aes.group)

  ggplot(df) +
    aes(x = .data[[paste0(fields("value"), ".x")]],
      y = .data[[paste0(fields("value"), ".y")]],
      color = .data$aes.group,
      shape = .data$aes.group,
      size = .data$aes.group,
      group = .data$aes.group,
      text = .data$aes.text) +
    geom_point() +
    scale_point_group(df$aes.group, guide = guide,
      wycategories = df[[fields("wycategory")]],
      baseyears = df[[fields("baseyear")]]) +
    scale_color_alt(df$aes.group, alt_colors, guide = guide) +
    scale_x_value(settings[["xlabel"]], settings[["xshift"]],
      settings[["xdivisor"]]) +
    scale_y_value(settings[["ylabel"]], settings[["yshift"]],
      settings[["ydivisor"]]) +
    ggtitle(settings[["title"]]) +
    theme_watplotter()
}


#' Metric vs Forecast Plot
#'
#' @inheritParams analysis_metrics
#' @inheritDotParams plot_overrides
#' @return A `ggplot` object.
#'
#' @importFrom ggplot2 ggplot aes geom_point ggtitle
#' @importFrom rlang .data
#' @keywords internal
plot_metric_vs_forecast = function(df, ..., alt_colors) {
  assert_fields(df, c(fields("alt", "date", "metric"),
    paste0(fields("path", "value"), c(".m", ".f"))))
  settings = plot_overrides(...)
  if (is.null(settings[["xlabel"]])) {
    settings[["xlabel"]] = sprintf("%s [%s]",
      path_to_label(unique(df[[paste0(fields("path"), ".f")]])),
      unique(format(df[[fields("date")]], "%d%b")))
  }
  if (is.null(settings[["ylabel"]])) {
    settings[["ylabel"]] = sprintf("%s (%s)",
      path_to_label(unique(df[[paste0(fields("path"), ".m")]])),
      unique(df[[fields("metric")]]))
  }
  df = rename_fields(df, paste0(fields("period"), ".m"),
    fields("period"))
  group_fields = get_fields(df, fields("alt", "metric"),
    fields("run", "wycategory", "baseyear", "period"))
  df["aes.group"] = build_aes_groups(df, group_fields)
  df["aes.text"] = paste0(
    sprintf("%s : Event %d", df$aes.group, df[[fields("event")]]),
    "\n",
    sprintf("\t%s (%s): %s", df[[paste0(fields("path"), ".m")]],
      df[[paste0(fields("metric"))]],
      comma(df[[paste0(fields("value"), ".m")]], accuracy = 0.01)),
    "\n",
    sprintf("\t%s (%s): %s", df[[paste0(fields("path"), ".f")]],
      df[[paste0(fields("period"), ".f")]],
      comma(df[[paste0(fields("value"), ".f")]], accuracy = 0.01))
  )
  guide = guide_watplotter(df[[fields("alt")]], df$aes.group)

  ggplot(df) +
    aes(x = .data[[paste0(fields("value"), ".f")]],
      y = .data[[paste0(fields("value"), ".m")]],
      color = .data$aes.group,
      shape = .data$aes.group,
      size = .data$aes.group,
      group = .data$aes.group,
      text = .data$aes.text) +
    geom_point() +
    scale_point_group(df$aes.group, guide = guide,
      wycategories = df[[fields("wycategory")]],
      baseyears = df[[fields("baseyear")]]) +
    scale_color_alt(df$aes.group, alt_colors, guide = guide) +
    scale_x_value(settings[["xlabel"]], settings[["xshift"]],
      settings[["xdivisor"]]) +
    scale_y_value(settings[["ylabel"]], settings[["yshift"]],
      settings[["ydivisor"]]) +
    ggtitle(settings[["title"]]) +
    theme_watplotter()
}

#' Trace Plot
#'
#' @inheritParams analysis_duration
#' @inheritDotParams plot_overrides
#' @return A `ggplot` object.
#'
#' @importFrom ggplot2 ggplot aes geom_line ggtitle
#' @importFrom rlang .data
#' @keywords internal
plot_trace = function(df, ..., alt_colors) {
  assert_fields(df, fields("alt", "path", "event", "value"))
  settings = plot_overrides(...)
  add_themes = list()
  if (is.null(settings[["ylabel"]])) {
    settings[["ylabel"]] = path_to_label(unique(df[[fields("path")]]))
  }
  if (is.null(settings[["xlabel"]])) {
    add_themes[["axis.title.x"]] = element_blank()
  }
  group_fields = get_fields(df, fields("alt"),
    fields("run", "wycategory", "baseyear", "period"))
  df["aes.group"] = build_aes_groups(df, group_fields)

  trace_fields = c(group_fields, fields("event"))
  df["trace.group"] = build_aes_groups(df, trace_fields)

  guide = guide_watplotter(df[[fields("alt")]], df$aes.group)

  ggplot(df) +
    aes(x = .data[[fields("refdate")]], y = .data[[fields("value")]],
      color = .data$aes.group,
      linetype = .data$aes.group,
      linewidth = .data$aes.group,
      group = .data$trace.group
    ) +
    geom_line(alpha = 0.2) +
    scale_line_group(df$aes.group, guide = guide,
      wycategories = df[[fields("wycategory")]],
      baseyears = df[[fields("baseyear")]]) +
    scale_color_alt(df$aes.group, alt_colors, guide = guide) +
    scale_x_refdate(settings[["xlabel"]]) +
    scale_y_value(settings[["ylabel"]], settings[["yshift"]],
      settings[["ydivisor"]]) +
    ggtitle(settings[["title"]]) +
    theme_watplotter(add_themes)
}