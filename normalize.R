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
#' # Basic normalization to [0, 1]
#' iaw$normalize(c(1, 2, 3, 4, 5))
#'
#' # NA values are preserved; range computed from non-NA elements
#' iaw$normalize(c(0, NA, 5, 10))
#'
#' # Constant vector maps to 0.5
#' iaw$normalize(c(7, 7, 7))
#'
#' # Normalize each column of a matrix
#' m <- matrix(c(10, 20, 30, 100, 200, 300), nrow = 3)
#' apply(m, 2, iaw$normalize)

iaw$normalize <- function(x) {
    stopifnot(is.numeric(x))
    if (all(is.na(x))) return(x)
    rng <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
    if (rng == 0) return(ifelse(is.na(x), NA_real_, 0.5))
    (x - min(x, na.rm = TRUE)) / rng
}
