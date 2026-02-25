#' Require Variables Exist
#'
#' @name require.variables
#'
#' Checks required variables exist in data frame.
#'
#' @param vars Required variable names.
#' @param df Data frame.
#'
#' @return Invisible TRUE if all exist.
#'
#' @examples
#' df <- data.frame(firm = 1:3, date = 4:6, ret = 7:9)
#'
#' # All required variables present -- returns invisible TRUE silently
#' iaw$require.variables(c("firm", "date"), df)
#'
#' # Missing variable triggers an informative error
#' tryCatch(
#'   iaw$require.variables(c("ret", "beta"), df),
#'   error = function(e) message(e$message)
#' )
#'
#' # Guard at the start of a data pipeline
#' panel <- data.frame(permno = 1:5, date = 1:5, ret = rnorm(5))
#' iaw$require.variables(c("permno", "date", "ret"), panel)  # silent, TRUE
#'
#' # Check a single required column
#' iaw$require.variables("ret", panel)  # invisible TRUE
#'
#' # Multiple missing variables listed in error message
#' tryCatch(
#'   iaw$require.variables(c("alpha", "beta", "gamma"), panel),
#'   error = function(e) message(e$message)
#' )  # "Missing variables: alpha, beta, gamma"
#'
#' @family utilities
#' @export

iaw$require.variables <- function(vars, df) {
    stopifnot(is.character(vars))
    stopifnot(is.data.frame(df))
    
    missing <- setdiff(vars, names(df))
    if (length(missing) > 0) {
        stop("Missing variables: ", paste(missing, collapse = ", "))
    }
    invisible(TRUE)
}
