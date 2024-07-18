#' Wat Plotter Fields
#'
#' Standard Fields in WAT Plotter data frames. This is an extension of
#' [watplotter.core::fields()] that includes additional fields produced
#' by analysis functions.
#'
#' @param ... A vector of field codes, any of: "alt", "run", "path",
#'   "event", "date", "value", "refdate", "wateryear", "wycategory",
#'   "wypercentile", "baseyear", "baselabel", "metric", "probability",
#'   "quantile".
#' @return A vector of field names.
#'
#' @export
fields = function(...) {

  core_fields = watplotter.core::fields()
  new_fields = c(
    metric = "metric",
    probability = "probability",
    quantile = "quantile"
  )
  all_fields = c(core_fields, new_fields)
  selected_fields = c(...)
  bad_fields = setdiff(selected_fields, names(all_fields))
  if (length(bad_fields) > 0L) {
    stop("Unknown field codes: ", paste(shQuote(bad_fields),
      collapse = ", "))
  } else if (length(selected_fields) == 0L) {
    all_fields
  } else {
    unname(all_fields[selected_fields])
  }
}


#' Assert Single Entries
#'
#' Check that only one entry for each grouping is present.
#'
#' @param df A dataset.
#' @param fields The fields to group by, i.e., output of [fields()].
#' @return (Invisibly) TRUE if groups are unique, error otherwise.
#'
#' @importFrom dplyr count pull across any_of
#' @keywords internal
assert_single = function(df, fields) {
  df.count = count(df, across(any_of(fields)), name = "num_entries")
  if (any(pull(df.count, "num_entries") > 1L)) {
    stop(sprintf("Multiple (%s) entries detected in argument \"%s\".",
        paste(intersect(names(df.count), fields), collapse = ", "),
        deparse(substitute(df))))
  }
  invisible(TRUE)
}


#' Assert Unique Entries
#'
#' Check that only one value for each field is present.
#'
#' @param df A dataset.
#' @param fields The fields to group by, i.e., output of [fields()].
#' @return (Invisibly) TRUE if groups are unique, error otherwise.
#'
#' @importFrom dplyr summarize select across any_of where
#' @keywords internal
assert_unique = function(df, fields) {
  df.unique = summarize(df, across(any_of(fields), function(x)
    length(unique(x))))
  problems = names(select(df.unique, where(function(x) x > 1L)))
  if (length(problems) > 1L) {
    stop(sprintf("Multiple entries detected in argument \"%s\" fields: %s.",
        deparse(substitute(df)), paste(problems, collapse = ", ")))
  }
  invisible(TRUE)
}


#' Assert Unique Entries
#'
#' Check that only one value for each field is present.
#'
#' @param df.x A dataset.
#' @param df.y A second dataset.
#' @param fields The fields to group by, i.e., output of [fields()].
#' @return (Invisibly) TRUE if groups are unique, error otherwise.
#'
#' @importFrom dplyr distinct arrange across any_of
#' @keywords internal
assert_match = function(df.x, df.y, fields) {
  dx = arrange(distinct(df.x, across(any_of(fields))),
    across(any_of(fields)))
  dy = arrange(distinct(df.y, across(any_of(fields))),
    across(any_of(fields)))

  if (!identical(as.list(dx), as.list(dy))) {
    stop(sprintf(
      "Non-matching (%s) entries detected in arguments \"%s\" and \"%s\".",
      paste(fields, collapse = ", "), deparse(substitute(df.x)),
      deparse(substitute(df.y))
    ))
  }
  invisible(TRUE)
}
