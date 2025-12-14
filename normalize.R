#' Normalize a Vector
#'
#' @name normalize
#'
#' Scales vector to [0, 1] range.
#'
#' @param x Numeric vector.
#'
#' @return Numeric vector in [0, 1].
#'
#' @family data-transformation
#' @export
#'
#' @examples
#' iaw$normalize(c(1, 2, 3, 4, 5))

iaw$normalize <- function(x) {
    stopifnot(is.numeric(x))
    (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
