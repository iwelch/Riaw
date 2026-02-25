#' Abort Program Execution with Error Message
#'
#' @name abort
#'
#' Terminates program execution with an informative error message.
#'
#' @param errstring A character string describing the error.
#'
#' @return Does not return; stops execution.
#'
#' @family error-handling
#' @export
#'
#' @examples
#' # Basic abort with a plain message
#' \dontrun{
#' iaw$abort("File not found")
#' }
#'
#' # Abort with a dynamic message built from context
#' \dontrun{
#' load_data <- function(path) {
#'   if (!file.exists(path))
#'     iaw$abort(paste("Cannot find file:", path))
#'   readRDS(path)
#' }
#' load_data("/no/such/file.rds")
#' }
#'
#' # Combined with %or% for readable guard clauses
#' \dontrun{
#' (is.numeric(x)) %or% iaw$abort("x must be numeric")
#' }
#'
#' # Abort is caught by tryCatch, enabling structured error handling
#' result <- tryCatch(
#'   iaw$abort("invalid configuration"),
#'   error = function(e) paste("caught:", e$message)
#' )
#' result   # "caught: invalid configuration"
#'
#' # Abort with formatted context from financial data validation
#' \dontrun{
#' validate_returns <- function(ret) {
#'   if (any(ret < -1))
#'     iaw$abort(sprintf("return below -100%% at index %d", which.min(ret)))
#' }
#' validate_returns(c(0.05, -0.02, -1.5))  # aborts with index 3
#' }
#'
#' # Abort inside a pipeline to stop on unexpected NA counts
#' \dontrun{
#' df <- data.frame(price = c(100, NA, 102, NA, NA))
#' na_pct <- mean(is.na(df$price))
#' if (na_pct > 0.5) iaw$abort(sprintf("%.0f%% NAs in price column", na_pct * 100))
#' }

iaw$abort <- function(errstring) {
    stopifnot(is.character(errstring), length(errstring) == 1L)
    message("iaw$abort: ", errstring)
    stop(errstring, call. = FALSE)
}
