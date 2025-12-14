#' Create ID for Fixed Effects
#'
#' @name diffid.for.fixed.effects
#'
#' Creates indicator for panel fixed effects.
#'
#' @param id Panel identifier.
#'
#' @return Factor for fixed effects.
#'
#' @family data-manipulation
#' @export

iaw$diffid.for.fixed.effects <- function(id) {
    as.factor(id)
}
