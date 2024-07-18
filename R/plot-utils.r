#' Build Aesthetic Groups
#'
#' Create groups From a set of fields.
#'
#' @inheritParams plot_metrics
#' @param fields A vector of fields to group.
#' @return A character vector.
#'
#' @importFrom dplyr select all_of
#' @importFrom purrr pmap_chr
#' @importFrom stringr str_squish
#' @keywords internal
build_aes_groups = function(df, fields) {
  main_group = intersect(fields("alt", "run"), fields)
  sub_group = intersect(fields("period", "wycategory", "baseyear"), fields)
  var_group = setdiff(fields, union(main_group, sub_group))

  if (length(main_group) == 2L) {
    prefix = sprintf("%s (%s)", df[[fields("alt")]], df[[fields("run")]])
  } else {
    prefix = df[[main_group]]
  }
  if (length(sub_group) > 0L) {
    midfix = pmap_chr(select(df, all_of(sub_group)), function(...)
      sprintf("[%s]", paste(..., collapse = ", ")))
  } else {
    midfix = ""
  }
  if (length(var_group) > 0L) {
    suffix = pmap_chr(select(df, all_of(var_group)), function(...)
      paste(..., sep = ": "))
    var_sep = ":"
  } else {
    suffix = ""
    var_sep = ""
  }

  str_squish(paste(prefix, midfix, var_sep, suffix))
}


#' Extract Aesthetic Specification
#'
#' Extract aesthetic specification from a list based on string match.
#'
#' @param group The aesthetic group name.
#' @param spec A named vector of aesthetic specifications.
#' @param default The default aesthetic specification to use when no
#'   match is found.
#' @return An aesthetic specification
#'
#' @importFrom stringr str_detect fixed
#' @keywords internal
extract_spec = function(group, spec, default) {
  c(spec[which(str_detect(group, fixed(names(spec))))], default)[1]
}


#' Generate Colors
#'
#' Generate colors for alternatives and runs. In cases where multiple
#' runs per alternative exist, a color gradient within each alternative
#' is generated.
#'
#' @inheritParams build_aes_groups
#' @param alt_colors A named list of colors to map to alternatives.
#'   If missing, default colors will be generated with
#'   [scales::brewer_pal()] "Dark2" palette.
#' @return A tibble of alternative (and possibly run) colors.
#'
#' @importFrom colorspace lighten
#' @importFrom scales brewer_pal
#' @importFrom purrr map2_chr
#' @importFrom dplyr tibble distinct across any_of pull group_by
#'   ungroup mutate transmute left_join if_else row_number n
#' @keywords internal
generate_colors = function(df, alt_colors) {
  # get default alt colors if not supplied
  assert_fields(df, fields("alt"))
  alts = unique(df[[fields("alt")]])
  if (missing(alt_colors)) {
    alt_colors = set_names(brewer_pal(palette = "Dark2")(length(alts)),
      alts)
  } else {
    missing_alts = setdiff(alts, names(alt_colors))
    if (length(missing_alts) > 0L) {
      stop("No colors specified for alternatives: ",
        paste(shQuote(missing_alts), collapse = ","))
    }
    alt_colors = alt_colors[alts]
  }
  alt_colors = tibble(alt = names(alt_colors),
    color = unname(alt_colors))
  # check if multiple runs per alt occur in data
  groups = distinct(df, across(any_of(fields("alt", "run"))))
  if (any(duplicated(pull(groups, fields("alt"))))) {
    colortable = ungroup(mutate(group_by(groups, across(fields("alt"))),
      amount = as.vector(scale(row_number()) * 0.135 * n()),
      amount = if_else(is.finite(.data$amount), .data$amount, 0)))
    transmute(left_join(colortable, alt_colors, by = fields("alt")),
      across(fields("alt", "run")),
      color = map2_chr(.data$color, .data$amount, lighten))
  } else {
    alt_colors
  }
}


#' Plot Defaults and Overrides
#'
#' Aesthetic defaults and overrides.
#'
#' @param title The plot title.
#' @param xlabel The x-axis label.
#' @param xdivisor A multiplier applied to the x-axis values.
#' @param xshift an offset applied to the x-axis values.
#' @param ylabel The y-axis label.
#' @param ydivisor A multiplier applied to the y-axis values.
#' @param yshift an offset applied to the y-axis values.
#' @return A list of overrides.
#'
#' @keywords internal
plot_overrides = function(title, xlabel, xdivisor, xshift,
  ylabel, ydivisor, yshift) {
  # identify what overrides were not supplied
  missing_overrides = c(
    title = missing(title),
    xlabel = missing(xlabel),
    xdivisor = missing(xdivisor),
    xshift = missing(xshift),
    ylabel = missing(ylabel),
    ydivisor = missing(ydivisor),
    yshift = missing(yshift)
  )
  # use eval-quote to evaluate non-missing overrides
  overrides = eval(c(
    title = quote(title),
    xlabel = quote(xlabel),
    xdivisor = quote(xdivisor),
    xshift = quote(xshift),
    ylabel = quote(ylabel),
    ydivisor = quote(ydivisor),
    yshift = quote(yshift)
  )[!missing_overrides])
  # plot defaults
  defaults = list(
    title = NULL,
    xlabel = NULL,
    xdivisor = 1,
    xshift = 0,
    ylabel = NULL,
    yshift = 0,
    ydivisor = 1,
    yshift = 0
  )[missing_overrides]
  # return complete set
  c(defaults, overrides)
}


#' Path To Label Template
#'
#' Create a standard axis label from a path.
#'
#' @param path A DSS Path.
#' @return A string.
#'
#' @importFrom watplotter.core get_path_part
#' @importFrom stringr str_replace_all str_to_title
#' @keywords internal
path_to_label = function(path) {

  label = str_replace_all(path, c(
    # pool storage
    "^//(.*)-POOL/STOR/.*/1DAY/(.*)/$" = "\\1 Pool Storage (acre-ft), \\2",
    # pool elevation
    "^//(.*)-POOL/ELEV/.*/1DAY/(.*)/$" = "\\1 Pool Elevation (ft, NAVD88), \\2",
    "^//(.*)-POOL/ELEV-NGVD29/.*/1DAY/(.*)/$" = "\\1 Pool Elevation (ft, NGVD29), \\2",
    # tailwater elevation
    # flow
    "^//(.*)_IN/FLOW/.*/1DAY/(.*)/$" = "\\1 Inflow (cfs), \\2",
    "^//(.*)/FLOW-IN/.*/1DAY/(.*)/$" = "\\1 Inflow (cfs), \\2",
    "^//(.*)_OUT/FLOW/.*/1DAY/(.*)/$" = "\\1 Outflow (cfs), \\2",
    "^//(.*)/FLOW-OUT/.*/1DAY/(.*)/$" = "\\1 Outflow (cfs), \\2",
    "^//(.*)/FLOW/.*/1DAY/(.*)/$" = "\\1 Flow (cfs), \\2",
    "^//(.*)/FLOW-LOCAL/.*/1DAY/(.*)/$" = "\\1 Local Flow (cfs), \\2",
    "^//(.*)/FLOW-CUMLOC/.*/1DAY/(.*)/$" = "\\1 Cumulative Local Flow (cfs), \\2",
    # power
    "^//(.*)-POWER PLANT/POWER/.*/1DAY/(.*)/$" = "\\1 Power Generation (MW), \\2",
    "^//(.*)-POWER PLANT/POWER-CAPACITY/.*/1DAY/(.*)/$" = "\\1 Power Capacity (MW), \\2",
    "^//(.*)-POWER PLANT/EFFICIENCY/.*/1DAY/(.*)/$" = "\\1 Power Efficiency, \\2",
    "^//(.*)-POWER PLANT/ENERGY/.*/1DAY/(.*)/$" = "\\1 Power Energy (MJ), \\2",
    # forecasts
    "^//(.+)/VOLUME\\[(.+)\\] FCST//.*/.*/$" = "\\1 Volume Forecast (KAF), \\2"
  ))
  paste(label, collapse = "\n")
}


#' WATPlotter Theme
#'
#' Default watplotter `ggplot` theme.
#'
#' @param add Additional [ggplot2::theme()] elements.
#'
#' @importFrom ggplot2 theme_bw theme
#' @keywords internal
theme_watplotter = function(add = list()) {
  list(
    theme_bw(base_size = 16),
    do.call(theme, c(add, legend.position = "bottom"))
  )
}


#' @rdname theme_watplotter
#'
#' @importFrom ggplot2 guide_legend
#' @keywords internal
guide_watplotter = function(alts, groups, max_col = 3L) {
  num_alts = length(unique(alts))
  num_groups = length(unique(groups))
  groups_per_alt = num_groups / num_alts
  if (max_col < num_alts) {
    ncol = max_col
  } else if (groups_per_alt < 5L) {
    ncol = num_alts
  } else {
    max_col_per_alt = max_col %/% num_alts
    possible_cols_per_alt = seq(1, max_col_per_alt)
    divisors = groups_per_alt %% possible_cols_per_alt
    ncol = num_alts * possible_cols_per_alt[max(which(divisors == 0L))]
  }
  guide_legend(ncol = ncol)
}


#' Shift and Scale Tranformation
#'
#' Basic WAT Plotter scale transform of the form `(x + shift) / divisor`.
#'
#' @param shift Numeric shift.
#' @param divisor Numeric divisor.
#' @return A `scales` tranformation.
#'
#' @importFrom scales trans_new
#' @keywords internal
rescale_trans = function(shift = 0, divisor = 1) {
  force(shift)
  force(divisor)
  trans = function(x) {
    (x + shift) / divisor
  }
  inv = function(x) {
    x * divisor - shift
  }
  trans_new("rescale", trans, inv)
}


#' Exceedance Probability Label
#'
#' Exceedance probability scale labels.
#'
#' @param percent If `TRUE`, format labels as percents.
#' @return A labeling function.
#'
#' @importFrom scales percent
#' @keywords internal
exceedance = function(percent = TRUE) {
  force(percent)
  if (percent) {
    function(x) percent(1 - x)
  } else {
    function(x) 1 - x
  }
}


#' Z Score Tranformation
#'
#' Z Score scale transform.
#'
#' @param reverse If `TRUE`, also apply a reverse transformation.
#' @return A `scales` tranformation.
#'
#' @importFrom stats pnorm qnorm
#' @importFrom scales regular_minor_breaks
#' @keywords internal
zscore_trans = function(reverse = TRUE) {
  force(reverse)
  if (reverse) {
    trans = function(x) -qnorm(x)
    inv = function(x) pnorm(-x)
  } else {
    trans = function(x) qnorm(x)
    inv = function(x) pnorm(x)
  }
  trans_new("zscore", trans, inv,
    minor_breaks = regular_minor_breaks(reverse))
}


refdate_label = "%d %b"

#' Field Aesthetics
#'
#' Default aesthetics for fields.
#'
#' @name field-aesthetics
#' @importFrom ggplot2 scale_x_datetime
#' @keywords internal
scale_x_refdate = function(xlabel) {
  scale_x_datetime(xlabel, date_labels = refdate_label)
}


#' @rdname field-aesthetics
#' @importFrom ggplot2 scale_x_reverse
#' @keywords internal
scale_x_duration = function(xlabel) {
  breaks = 1 - c(0.99, 0.9, 0.8, 0.6, 0.4, 0.2, 0.1, 0.01)
  scale_x_reverse(xlabel, breaks = breaks,
    labels = exceedance(TRUE))
}


#' @rdname field-aesthetics
#' @importFrom ggplot2 scale_x_reverse
#' @keywords internal
scale_x_frequency = function(xlabel) {
  # breaks are non-exceedance, but our list is defined for exceedance
  breaks = 1 - c(0.99, 0.9, 0.5, 0.1, 0.01, 0.001)
  scale_x_continuous(xlabel, breaks = breaks, labels = exceedance(TRUE),
    trans = zscore_trans(FALSE))
}


#' @rdname field-aesthetics
#' @importFrom ggplot2 scale_color_manual
#' @importFrom purrr map_chr set_names
#' @importFrom stringr str_detect fixed
#' @keywords internal
scale_color_alt = function(groups, alt_colors, guide) {
  # construct scale values by detecting alts in the supplied groups
  alt_colors["name"] = build_aes_groups(alt_colors,
    intersect(fields("alt", "run"), names(alt_colors)))
  group_colors = set_names(alt_colors$color, alt_colors$name)
  colors = map_chr(set_names(unique(groups)), function(x) 
    group_colors[which(str_detect(x, fixed(names(group_colors))))])
  scale_color_manual(NULL, values = colors, guide = guide)
}


#' @rdname field-aesthetics
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom scales comma
#' @keywords internal
scale_x_value = function(xlabel, xshift, xdivisor) {
  scale_x_continuous(xlabel, labels = comma,
    trans = rescale_trans(xshift, xdivisor))
}


#' @rdname field-aesthetics
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom scales comma
#' @keywords internal
scale_y_value = function(ylabel, yshift, ydivisor) {
  scale_y_continuous(ylabel, labels = comma,
    trans = rescale_trans(yshift, ydivisor))
}


#' @rdname field-aesthetics
#' @importFrom ggplot2 scale_linewidth_manual scale_linetype_manual
#' @importFrom purrr map_chr map_dbl set_names
#' @keywords internal
scale_line_group = function(groups, guide, wycategories = NULL,
  baseyears = NULL) {

  if (!is.null(wycategories)) {
    wycategories = levels(droplevels(wycategories))
    linespec = wycategory_lines(wycategories)
  } else if (!is.null(baseyears)) {
    baseyears = levels(droplevels(baseyears))
    linespec = baseyear_lines(baseyears)
  } else {
    linespec = metric_lines()
  }

  # construct scale values by detecting metrics or categories in groups
  linetypes = map_chr(set_names(unique(groups)), function(x) {
    extract_spec(x, linespec[["types"]], "solid")
  })
  linewidths = map_dbl(set_names(unique(groups)), function(x) {
    extract_spec(x, linespec[["widths"]], 1)
  })

  list(
    scale_linetype_manual(NULL, values = linetypes, guide = guide),
    scale_linewidth_manual(NULL, values = linewidths, guide = guide)
  )
}


#' @rdname field-aesthetics
#' @importFrom ggplot2 scale_shape_manual scale_size_manual
#' @importFrom purrr map_int map_dbl set_names
#' @keywords internal
scale_point_group = function(groups, guide, wycategories = NULL,
  baseyears = NULL) {

  if (!is.null(wycategories)) {
    wycategories = levels(droplevels(wycategories))
    pointspec = wycategory_points(wycategories)
  } else if (!is.null(baseyears)) {
    baseyears = levels(droplevels(baseyears))
    pointspec = baseyear_points(baseyears)
  } else {
    pointspec = metric_points()
  }
  # construct scale values by detecting metrics or categories in groups
  pointshapes = map_int(set_names(unique(groups)), function(x) {
    extract_spec(x, pointspec[["shapes"]], 16L)
  })
  pointsizes = map_dbl(set_names(unique(groups)), function(x) {
    extract_spec(x, pointspec[["sizes"]], 3)
  })

  list(
    scale_shape_manual(NULL, values = pointshapes, guide = guide),
    scale_size_manual(NULL, values = pointsizes, guide = guide)
  )
}


#' Metric Geometry Aesthetics
#'
#' Aesthetics for metric geometries.
#'
#' @return A list with elements "types" and "widths".
#'
#' @name metric-aesthetics
#' @keywords internal
metric_lines = function() {
  # use metrics as linetype and linewidth value names
  linetype_values = c(
    "1% exceedance" = "dashed",
    "10% exceedance" = "dashed",
    "25% exceedance" = "dashed",
    "50% exceedance" = "dotted",
    "75% exceedance" = "dashed",
    "90% exceedance" = "dashed",
    "99% exceedance" = "dashed",
    "mean" = "solid",
    "min" = "solid",
    "max" = "solid",
    "median" = "dotted"
  )
  linewidth_values = c(
    "1% exceedance" = 0.2,
    "10% exceedance" = 0.4,
    "25% exceedance" = 0.6,
    "50% exceedance" = 1,
    "75% exceedance" = 0.6,
    "90% exceedance" = 0.4,
    "99% exceedance" = 0.2,
    "mean" = 1,
    "min" = 0.2,
    "max" = 0.2,
    "median" = 1
  )

  list(
    types = linetype_values,
    widths = linewidth_values
  )
}


#' Water Year Category Geometry Aesthetics
#'
#' Aesthetics for water year category geometries.
#'
#' @return A list with elements "types" and "widths".
#'
#' @name wycategory-aesthetics
#' @importFrom purrr set_names
#' @keywords internal
wycategory_lines = function(wycategories = NULL) {
  num_categories = length(wycategories)
  if (num_categories > 9L) {
    stop("Cannot plot more than 9 water year categories at a time.")
  }
  # specified aesthetics for 1, 3, or 5 categories
  if (num_categories == 1L) {
    linetype_values = "solid"
    linewidth_values = 1
  } else if (num_categories <= 3L) {
    linetype_values = c("dotted", "solid", "dashed")[seq_len(num_categories)]
    linewidth_values = c(1, 1, 1)[seq_len(num_categories)]
  } else if (num_categories <= 5L) {
    linetype_values = c("dotted", "dotted", "solid",
      "dashed", "dashed")[seq_len(num_categories)]
    linewidth_values = c(0.6, 1, 1, 1, 0.6)[seq_len(num_categories)]
  } else {
    # otherwise, create nested linetype-linewidth categories
    n_groups = ceiling(num_categories / 3)
    n_subgroups = ceiling(num_categories / n_groups)
    topgroups = cut(seq_len(num_categories), n_groups)
    subgroups = unlist(sapply(table(topgroups), seq_len,
        USE.NAMES = FALSE))
    linetype_values = c("dotted", "solid", "dashed")[topgroups]
    linewidth_values = c(1, 0.6, 0.4)[subgroups]
  }
  # use categories as linetype and linewidth value names
  linetype_values = set_names(linetype_values, wycategories)
  linewidth_values = set_names(linewidth_values, wycategories)

  list(
    types = linetype_values,
    widths = linewidth_values
  )
}


#' Base Year Geometry Aesthetics
#'
#' Aesthetics for base year geometries.
#'
#' @return A list with elements "types" and "widths".
#'
#' @name baseyear-aesthetics
#' @importFrom purrr set_names
#' @keywords internal
baseyear_lines = function(baseyears = NULL) {
  num_categories = length(baseyears)
  if (num_categories > 9L) {
    stop("Cannot plot more than 9 base years at a time.")
  }
  # specified aesthetics for 1, 3, or 5 categories
  if (num_categories == 1L) {
    linetype_values = "solid"
    linewidth_values = 1
  } else if (num_categories <= 3L) {
    linetype_values = c("dotted", "solid", "dashed")[seq_len(num_categories)]
    linewidth_values = c(1, 1, 1)[seq_len(num_categories)]
  } else if (num_categories <= 5L) {
    linetype_values = c("dotted", "dotted", "solid",
      "dashed", "dashed")[seq_len(num_categories)]
    linewidth_values = c(0.6, 1, 1, 1, 0.6)[seq_len(num_categories)]
  } else {
    # otherwise, create nested linetype-linewidth categories
    n_groups = ceiling(num_categories / 3)
    n_subgroups = ceiling(num_categories / n_groups)
    topgroups = cut(seq_len(num_categories), n_groups)
    subgroups = unlist(sapply(table(topgroups), seq_len,
        USE.NAMES = FALSE))
    linetype_values = c("dotted", "solid", "dashed")[topgroups]
    linewidth_values = c(1, 0.6, 0.4)[subgroups]
  }
  linetype_values = set_names(linetype_values, baseyears)
  linewidth_values = set_names(linewidth_values, baseyears)
  list(
    types = linetype_values,
    widths = linewidth_values
  )
}


#' @rdname metric-aesthetics
#' @keywords internal
metric_points = function() {
  # use metrics as linetype and linewidth value names
  pointshape_values = c(
    "1% exceedance" = 16L,
    "10% exceedance" = 16L,
    "25% exceedance" = 16L,
    "50% exceedance" = 16L,
    "75% exceedance" = 16L,
    "90% exceedance" = 16L,
    "99% exceedance" = 16L,
    "mean" = 16L,
    "min" = 16L,
    "max" = 16L,
    "median" = 16L,
    "duration" = 16L
  )
  pointsize_values = c(
    "1% exceedance" = 3,
    "10% exceedance" = 3,
    "25% exceedance" = 3,
    "50% exceedance" = 3,
    "75% exceedance" = 3,
    "90% exceedance" = 3,
    "99% exceedance" = 3,
    "mean" = 3,
    "min" = 3,
    "max" = 3,
    "median" = 3,
    "duration" = 3
  )

  list(
    shapes = pointshape_values,
    sizes = pointsize_values
  )
}


#' @rdname wycategory-aesthetics
#' @importFrom purrr set_names
#' @keywords internal
wycategory_points = function(wycategories) {
  num_categories = length(wycategories)
  if (num_categories > 9L) {
    stop("Cannot plot more than 9 water year categories at a time.")
  }
  circle = c(solid = 16L, open = 1L, hatched = 10L)
  square = c(solid = 15L, open = 0L, hatched = 12L)
  diamond = c(solid = 18L, open = 5L, hatched = 9L)

  # specified aesthetics for 1, 3, or 5 categories
  if (num_categories == 1L) {
    pointshape_values = circle["solid"]
  } else if (num_categories <= 3L) {
    pointshape_values = c(square["solid"], circle["solid"],
      diamond["solid"])[seq_len(num_categories)]
  } else if (num_categories <= 5L) {
    pointshape_values = c(square[c("solid", "open")], circle["solid"],
      diamond[c("solid", "open")])[seq_len(num_categories)]
  } else {
    # otherwise, create shape groups
    n_groups = ceiling(num_categories / 3)
    n_subgroups = ceiling(num_categories / n_groups)
    topgroups = cut(seq_len(num_categories), n_groups)
    subgroups = unlist(sapply(table(topgroups), seq_len,
        USE.NAMES = FALSE))
    pointshape_values = c(circle, square, diamond)[seq_along(subgroups)]
  }
  # use categories as linetype and linewidth value names
  pointshape_values = set_names(pointshape_values, wycategories)
  # also set size
  pointsize_values = set_names(rep(3, num_categories), wycategories)

  list(
    shapes = pointshape_values,
    sizes = pointsize_values
  )
}


#' @rdname baseyear-aesthetics
#' @importFrom purrr set_names
#' @keywords internal
baseyear_points = function(baseyears) {
  num_categories = length(baseyears)
  if (num_categories > 9L) {
    stop("Cannot plot more than 9 base years at a time.")
  }
  circle = c(solid = 16L, open = 1L, hatched = 10L)
  square = c(solid = 15L, open = 0L, hatched = 12L)
  diamond = c(solid = 18L, open = 5L, hatched = 9L)

  # specified aesthetics for 1, 3, or 5 categories
  if (num_categories == 1L) {
    pointshape_values = circle["solid"]
  } else if (num_categories <= 3L) {
    pointshape_values = c(square["solid"], circle["solid"],
      diamond["solid"])[seq_len(num_categories)]
  } else if (num_categories <= 5L) {
    pointshape_values = c(square[c("solid", "open")], circle["solid"],
      diamond[c("solid", "open")])[seq_len(num_categories)]
  } else {
    # otherwise, create shape groups
    n_groups = ceiling(num_categories / 3)
    n_subgroups = ceiling(num_categories / n_groups)
    topgroups = cut(seq_len(num_categories), n_groups)
    subgroups = unlist(sapply(table(topgroups), seq_len,
        USE.NAMES = FALSE))
    pointshape_values = c(circle, square, diamond)[seq_along(subgroups)]
  }
  # use categories as linetype and linewidth value names
  pointshape_values = set_names(pointshape_values, baseyears)
  # also set size
  pointsize_values = set_names(rep(3, num_categories), baseyears)

  list(
    shapes = pointshape_values,
    sizes = pointsize_values
  )
}
