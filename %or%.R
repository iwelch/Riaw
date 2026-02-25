#' Conditional OR Operator with Error Handling
#'
#' @name %or%
#'
#' A Perl-like 'or' condition that executes the right-hand side only if the
#' left-hand side is FALSE. Primary use is for assertions.
#'
#' @param e1 A logical scalar condition to evaluate.
#' @param e2 An expression to execute.
#'
#' @return Invisibly returns NULL.
#'
#' @family operators
#' @export
#'
#' @seealso \code{\link{%and%}}
#'
#' @examples
#' # Execute right-hand side only when condition is FALSE (assertion style)
#' x <- "hello"
#' (is.character(x)) %or% stop("x must be a character")
#'
#' # Condition is FALSE: right-hand side fires
#' \dontrun{
#' (is.numeric(x)) %or% iaw$abort("x must be numeric")
#' }
#'
#' # Guard a function with readable preconditions
#' \dontrun{
#' process <- function(n, label) {
#'   (n > 0)             %or% iaw$abort("n must be positive")
#'   (is.character(label)) %or% iaw$abort("label must be a string")
#'   paste(label, 1:n)
#' }
#' process(3, "item")   # works fine
#' process(-1, "item")  # aborts with message
#' }

`%or%` <- function(e1, e2) {
    e2 <- substitute(e2)
    stopifnot(is.logical(e1), length(e1) == 1L, !is.na(e1))
    if (!e1) {
        if (is.character(e2)) stop("Use (cond) %or% abort('message'), not (cond) %or% 'message'")
        eval.parent(e2)
    }
    invisible(NULL)
}
