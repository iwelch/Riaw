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
#' # 2 firms x 2 years -> 2x2 wide matrix of returns
#' df <- data.frame(firm = c("A","A","B","B"), year = c(1,2,1,2), val = 1:4)
#' iaw$long2wide(df, "year", "firm", "val")
#'
#' # Typical panel: stock returns, pivot time to columns
#' panel <- data.frame(
#'   ticker = rep(c("AAPL","GOOG"), each = 3),
#'   yyyymm = rep(c(202001, 202002, 202003), times = 2),
#'   ret    = c(0.05, -0.02, 0.03, 0.01, 0.04, -0.01)
#' )
#' iaw$long2wide(panel, "yyyymm", "ticker", "ret")
#'
#' # Single entity: result is a 1-row data frame
#' solo <- data.frame(firm = rep("X", 3), year = 1:3, val = c(0.1, 0.2, 0.3))
#' iaw$long2wide(solo, "year", "firm", "val")
#'
#' # Compute correlation matrix from wide panel of returns
#' panel2 <- data.frame(
#'   stock = rep(c("AAPL","MSFT","GOOG"), each = 4),
#'   qtr   = rep(1:4, 3),
#'   ret   = c(0.05, -0.02, 0.03, 0.01, 0.02, 0.04, -0.01, 0.03,
#'             0.01, 0.03, 0.02, -0.02)
#' )
#' wide <- iaw$long2wide(panel2, "qtr", "stock", "ret")
#' cor(t(wide))  # pairwise return correlations

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
