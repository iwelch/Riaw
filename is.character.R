#' Test if Object is Character Vector of Specific Length
#'
#' @name is.character
#'
#' Extends base is.character to also check length.
#'
#' @param cvec Object to test.
#' @param required.length.of.cvec Required length, or \code{NULL} to skip
#'   length check (default \code{NULL}).
#'
#' @return Logical scalar.
#'
#' @family type-checking
#' @export
#'
#' @examples
#' # Basic character check (no length constraint)
#' iaw$is.character(c("foo", "bar"))
#'
#' # Length-constrained check: must be character AND exactly length 1
#' iaw$is.character("hello", 1)
#' iaw$is.character(c("a", "b"), 1)  # FALSE: wrong length
#'
#' # Validate a fixed-length character vector
#' iaw$is.character(c("a", "b"), 2)
#'
#' # Check that a data frame column is character
#' df <- data.frame(id = c("A", "B", "C"), val = 1:3, stringsAsFactors = FALSE)
#' iaw$is.character(df$id)   # TRUE
#' iaw$is.character(df$val)  # FALSE
#'
#' # Numbers are not characters
#' iaw$is.character(c(1, 2, 3))

iaw$is.character <- function(cvec, required.length.of.cvec = NULL) {

    c1 <- is.character(cvec)
    if (is.null(required.length.of.cvec)) return(c1)

    stopifnot(is.numeric(required.length.of.cvec), length(required.length.of.cvec) == 1L)
    c1 && (length(cvec) == required.length.of.cvec)
}
