#' Interpolated Character Strings
#'
#' @name estring
#'
#' Evaluates expressions embedded in strings using double-brace syntax.
#'
#' @param es A character string containing placeholders.
#'
#' @return A character string with placeholders replaced.
#'
#' @family error-handling
#' @export
#'
#' @examples
#' x <- 42
#' iaw$estring("The value is x={{x}}")

iaw$estring <- function(es) {
    stopifnot(is.character(es), length(es) == 1L)
    
    rx <- "(?<=\\{\\{).*?(?=\\}\\})"
    match <- gregexpr(rx, es, perl = TRUE)
    toeval <- regmatches(es, match)[[1]]
    
    subback <- rep(NA, length(toeval))
    if (length(toeval) == 0) return(es)
    
    for (i in seq_along(toeval)) {
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
