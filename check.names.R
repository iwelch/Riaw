#' Verify Required Column Names Exist in Data Frame
#'
#' Checks that all specified column names exist in a data frame. Aborts with
#' an error message listing missing names if any are not found.
#'
#' @param wanted Character vector of required column names.
#' @param df A data frame to check.
#'
#' @return Invisibly returns TRUE if all names exist. Aborts if any are missing.
#'
#' @export
#'
#' @seealso \code{\link{iaw$require.variables}}, \code{\link{names}},
#'   \code{\link{setdiff}}
#'
#' @examples
#' df <- data.frame(a = 1, b = 2, c = 3)
#'
#' # All names exist - returns silently
#' iaw$check.names(c("a", "b"), df)
#'
#' \dontrun{
#' # Missing names - aborts with error
#' iaw$check.names(c("a", "x", "y"), df)
#' # Error: names do not exist: x, y
#' }
#'
#' # Use in function to validate input
#' my_analysis <- function(data) {
#'     iaw$check.names(c("date", "return", "volume"), data)
#'     # ... proceed with analysis
#' }

iaw$check.names <- function(wanted, df) {
    (is.data.frame(df)) %or% "second argument must be a data frame"
    (is.character(wanted)) %or% "first argument must be character vector of names"

    m <- setdiff(wanted, names(df))

    (length(m) == 0) %or% "names do not exist: {{paste(m, collapse=', ')}}"

    invisible(TRUE)
}
