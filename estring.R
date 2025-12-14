#' Interpolated Character Strings
#'
#' Evaluates expressions embedded in strings using \code{\{\{expr\}\}} syntax.
#' Similar to template strings in other languages. Expressions are evaluated
#' in the calling environment.
#'
#' @param es A character string containing \code{\{\{expr\}\}} placeholders
#'   where \code{expr} is any valid R expression.
#'
#' @return A character string with all \code{\{\{expr\}\}} placeholders replaced
#'   by the evaluated and formatted results.
#'
#' @details This function searches up the call stack to find variables referenced
#'   in the expressions. The output format is \code{expr=value} for each
#'   interpolated expression.
#'
#' @note This function should not internally use \code{\%or\%}, \code{\%and\%},
#'   or anything else that calls estring, to avoid infinite recursion.
#'
#' @export
#'
#' @seealso \code{\link{iaw$abort}}, \code{\link{sprintf}}, \code{\link{glue}}
#'
#' @examples
#' # Simple variable interpolation
#' x <- 42
#' iaw$estring("The value is {{x}}")
#' # "The value is x=42"
#'
#' # Expression evaluation
#' a <- 10
#' b <- 20
#' iaw$estring("Sum is {{a + b}}, product is {{a * b}}")
#' # "Sum is a + b=30, product is a * b=200"
#'
#' # Function calls in expressions
#' vec <- c(1, 2, 3, 4, 5)
#' iaw$estring("Mean: {{mean(vec)}}, SD: {{sd(vec)}}")
#'
#' # Class inspection
#' obj <- list(a = 1)
#' iaw$estring("Object is of class {{class(obj)}}")

iaw$estring <- function(es) {
    rx <- "(?<=\\{\\{).*?(?=\\}\\})"

    match <- gregexpr(rx, es, perl = TRUE)
    toeval <- regmatches(es, match)[[1]]

    subback <- rep(NA, length(toeval))
    if (length(toeval) == 0) return(es)

    for (i in 1:length(toeval)) {
        for (en in sys.nframe():0) {
            subback[i] <- tryCatch(
                eval(parse(text = toeval[i]), envir = en),
                error = function(e) "ERR"
            )
            if (is.na(subback[i])) subback[i] <- "NA"
            if (subback[i] != "ERR") break
        }
        subback[i] <- paste(toeval[i], "=", subback[i], sep = "")
    }
    regmatches(es, match) <- "%s"

    do.call("sprintf", c(fmt = es, as.list(subback)))
}
