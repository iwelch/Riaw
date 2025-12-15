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
    rng <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
    if (rng == 0) return(rep(0.5, length(x)))  # or NA
    (x - min(x, na.rm = TRUE)) / rng
}
