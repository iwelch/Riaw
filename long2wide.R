#' Reshape Data Frame from Long to Wide Format
#'
#' Converts a long-format data frame (one observation per row) to wide format
#' (with one column per time period or other spreading variable).
#'
#' @param d A data frame in long format.
#' @param id.time Character; name of the column to spread across (becomes column names).
#' @param id.firm Character; name of the identifier column (becomes row names).
#' @param id.val Character; name of the value column (becomes cell values).
#'
#' @return A data frame in wide format. Row names are set to the firm/unit
#'   identifiers, columns are named by time periods, and cells contain the values.
#'
#' @details
#' This is a convenience wrapper around \code{reshape()} for converting panel
#' data from long format (where each firm-time observation is a row) to wide
#' format (where each firm is a row and time periods are columns).
#'
#' @export
#'
#' @seealso \code{\link{iaw$wide2long}} for the reverse transformation,
#'   \code{\link{reshape}}, \code{\link[tidyr]{pivot_wider}}
#'
#' @examples
#' # Create long-format panel data
#' df_long <- data.frame(
#'     firm = rep(c("A", "B", "C"), each = 4),
#'     year = rep(2020:2023, 3),
#'     revenue = c(100, 110, 120, 130,   # Firm A
#'                 200, 220, 240, 260,   # Firm B
#'                 150, 160, 170, 180)   # Firm C
#' )
#' df_long
#'
#' # Convert to wide format
#' df_wide <- iaw$long2wide(df_long, id.time = "year",
#'                          id.firm = "firm", id.val = "revenue")
#' df_wide
#' #   revenue.2020 revenue.2021 revenue.2022 revenue.2023
#' # A          100          110          120          130
#' # B          200          220          240          260
#' # C          150          160          170          180
#'
#' # Wide format is useful for correlation matrices
#' cor(df_wide)

iaw$long2wide <- function(d, id.time, id.firm, id.val) {
    stopifnot(is.data.frame(d))
    stopifnot(is.character(id.time) && length(id.time) == 1)
    stopifnot(is.character(id.firm) && length(id.firm) == 1)
    stopifnot(is.character(id.val) && length(id.val) == 1)
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
    rv <- rv[, 2:ncol(rv)]

    rv
}
