#' Assert Condition
#'
#' @name assert
#'
#' Checks a condition and aborts with an error message if FALSE.
#'
#' @param cond Logical scalar to test.
#' @param ... Error message arguments passed to abort().
#'
#' @return Invisible NULL if condition is TRUE; otherwise stops.
#'
#' @family error-handling
#' @export
#'
#' @examples
#' # Basic assertion that passes silently
#' iaw$assert(1 + 1 == 2, "math is broken")
#'
#' # Glue-style interpolation in the message
#' n <- 5
#' iaw$assert(n > 0, paste("n must be positive, got", n))
#'
#' # Common pattern: validate function arguments
#' \dontrun{
#' check_input <- function(x) {
#'   iaw$assert(is.numeric(x), paste("x must be numeric, got", class(x)))
#'   iaw$assert(length(x) > 0, "x must not be empty")
#' }
#' check_input(c(1, 2, 3))  # passes
#' check_input("oops")      # aborts with message
#' }
#'
#' # Validate portfolio weights sum to 1
#' weights <- c(0.4, 0.35, 0.25)
#' iaw$assert(abs(sum(weights) - 1) < 1e-10, "weights must sum to 1")  # passes
#'
#' # Assert catches the failure and returns a clean error via tryCatch
#' err <- tryCatch(
#'   iaw$assert(FALSE, "expected positive returns"),
#'   error = function(e) e$message
#' )
#' err   # "expected positive returns"
#'
#' # Guard data dimensions before matrix operations
#' m <- matrix(1:12, nrow = 3)
#' iaw$assert(nrow(m) >= 2, paste("need >= 2 rows, got", nrow(m)))  # passes

iaw$assert <- function(cond, ...) {
    stopifnot(is.logical(cond), length(cond) == 1L)
    if (!cond) {
        msg <- paste(...)
        if (nchar(msg) == 0) msg <- paste(deparse(substitute(cond)), collapse = " ")
        iaw$abort(msg)
    }
    invisible(NULL)
}
