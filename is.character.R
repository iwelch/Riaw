#' Test if Object is Character Vector of Specific Length
#'
#' Extends base \code{is.character()} to also check that the vector has a
#' specific length. Useful for validating function arguments.
#'
#' @param cvec Object to test.
#' @param required.length.of.cvec Required length (default 0 means any length
#'   character vector passes if \code{cvec} is character).
#'
#' @return Logical: TRUE if \code{cvec} is character AND has exactly
#'   \code{required.length.of.cvec} elements.
#'
#' @export
#'
#' @seealso \code{\link{iaw$is.numeric}}, \code{\link{iaw$is.scalar}},
#'   \code{\link{is.character}}
#'
#' @examples
#' # Check for single character string
#' iaw$is.character("hello", 1)
#' # TRUE
#'
#' iaw$is.character(c("a", "b"), 1)
#' # FALSE (wrong length)
#'
#' iaw$is.character(123, 1)
#' # FALSE (not character)
#'
#' # Common usage in validation
#' validate_filename <- function(f) {
#'     (iaw$is.character(f, 1)) %or% "filename must be a single string"
#'     # ...
#' }

iaw$is.character <- function(cvec, required.length.of.cvec = 0) {
    c1 <- .Primitive("is.character")(cvec)
    c2 <- length(cvec) == required.length.of.cvec
    c1 && c2
}
