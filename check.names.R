#' Verify Column Names Exist
#'
#' @name check.names
#'
#' Checks that all specified names exist in data frame.
#'
#' @param wanted Character vector of required names.
#' @param df Data frame to check.
#'
#' @return Invisibly returns TRUE.
#'
#' @family utilities
#' @export
#'
#' @examples
#' df <- data.frame(a = 1, b = 2)
#' iaw$check.names(c("a", "b"), df)
#'
#' # Useful at the top of a function to validate required columns early
#' process <- function(df) {
#'   iaw$check.names(c("date", "price", "volume"), df)
#'   # ... rest of function
#' }
#'
#' # Missing column triggers an informative error
#' df2 <- data.frame(date = 1, price = 2)
#' tryCatch(
#'   iaw$check.names(c("date", "price", "volume"), df2),
#'   error = function(e) message(e$message)
#' )

iaw$check.names <- function(wanted, df) {
    stopifnot(is.data.frame(df))
    stopifnot(is.character(wanted))
    
    m <- setdiff(wanted, names(df))
    if (length(m) > 0) {
        stop("Names do not exist: ", paste(m, collapse = ", "))
    }
    invisible(TRUE)
}
