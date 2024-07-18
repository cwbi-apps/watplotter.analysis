#' Energy Price
#'
#' Calculate energy prices.
#'
#' @param df A dataframe. Must contain fields fields `"month"`, `"event"`,
#'   and `"value"` (MWhr).
#' @param market A dataframe of monthly and event-based market prices.
#'   Includes fields `"month"`, `"event"`, and `"price"` ($/MWhr).
energy_price = function(df, market) {

  stop("not implemented")

 # orig_fields = names(df)
 # df = mutate(df, month = as.integer(month(.data[[fields("date")]])))

#  joined_df = left_join(df, market, by = c("month", fields("event")))
#  joined_df = mutate(joined_df, cost =  .data[[fields("value")]] * .data[["price"]])

  

  # Correction for model spinup time: set 01Oct-05Oct prices to zero


}


#' Expected Annual Damages (Simple)
#'
#' Compute Expected Annual Damages (EAD) as a simple linear function
#' of flow.
#'
#' @param x A numeric vector of flows, in cfs.
#' @return A vector of damages.
#'
#' @details Damages are calculated using the formula
#' - For \eqn{Q < 400, 000 \text{ cfs}}: \eqn{EAD = 0.01}
#' - For \eqn{Q \geq 400, 000 \text{ cfs}}: \eqn{EAD = 10^{0.001mQ + b} - \alpha}
#'
#' Where
#' - \eqn{Q} is flow, in cfs
#' - \eqn{m = 0.006127839}
#' - \eqn{b = -3.301029996}
#' - \eqn{\alpha = 0.1413}
#'
#' @importFrom dplyr if_else
#' @keywords internal
annual_damages_simple = function(x) {

  stop("not implemented")

  min_flow = 400e3
  min_damage = 0.01
  slope = 0.006127839 # m
  intercept = -3.301029996 # b
  offset = 0.1413 # alpha

  # alternate approach
  #damage = 10^(0.001 * x * slope + intercept)
  #if_else(damages < min_damage, min_damage, damages)

  if_else(x < min_flow, min_damage,
    10^(0.001 * x * slope + intercept) - offset)
}
