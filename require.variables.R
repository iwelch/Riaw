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
