#' Remove Rows with NA (Deprecated)
#'
#' This function is deprecated. Use \code{na.omit()} or \code{complete.cases()}
#' instead for removing rows with missing values.
#'
#' @param object A data frame or matrix.
#'
#' @return Throws an error directing you to use alternatives.
#'
#' @export
#'
#' @seealso \code{\link{na.omit}}, \code{\link{complete.cases}}
#'
#' @examples
#' # Instead of completeobs(), use:
#' df <- data.frame(x = c(1, 2, NA, 4), y = c(NA, 2, 3, 4))
#' na.omit(df)
#' df[complete.cases(df), ]

iaw$completeobs <- function(object) {
    .Deprecated("na.omit")
    iaw$abort("Please use na.omit() or complete.cases() instead")
}
