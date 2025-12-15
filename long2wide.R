#' Reshape Long to Wide Format
#'
#' @name long2wide
#'
#' Converts long data frame to wide format.
#'
#' @param d Data frame in long format.
#' @param id.time Time variable (becomes columns).
#' @param id.firm Entity variable (becomes rows).
#' @param id.val Value variable.
#'
#' @return Data frame in wide format.
#'
#' @family data-reshaping
#' @export
#'
#' @seealso tidyr::pivot_wider(d, names_from = id.time, values_from = id.val)
#'
#' @examples
#' df <- data.frame(firm = c("A","A","B","B"), year = c(1,2,1,2), val = 1:4)
#' iaw$long2wide(df, "year", "firm", "val")

iaw$long2wide <- function(d, id.time, id.firm, id.val) {
    stopifnot(is.data.frame(d))
    stopifnot(is.character(id.time), length(id.time) == 1L)
    stopifnot(is.character(id.firm), length(id.firm) == 1L)
    stopifnot(is.character(id.val), length(id.val) == 1L)
    stopifnot(id.time %in% names(d))
    stopifnot(id.firm %in% names(d))
    stopifnot(id.val %in% names(d))

    rv <- reshape(
        subset(d, TRUE, select = c(id.firm, id.time, id.val)),
        idvar = id.firm,
        timevar = id.time,
        direction = "wide"
    )
    rownames(rv) <- rv[, 1]
    rv[, 2:ncol(rv)]
}
