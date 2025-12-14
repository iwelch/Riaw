#' Instrument R File for Debugging
#'
#' @name instrumentR
#'
#' Adds line number messages to R file.
#'
#' @param input_file Input R file.
#' @param output_file Output file (default: debug-input).
#'
#' @return Output filename.
#'
#' @family utilities
#' @export

instrumentR <- function(input_file, output_file = NULL) {
    stopifnot(is.character(input_file), length(input_file) == 1L)
    stopifnot(grepl("\\.R", input_file, ignore.case = TRUE))
    
    if (is.null(output_file)) {
        output_file <- paste0("debug-", input_file)
    }
    
    lines <- readLines(input_file, warn = FALSE)
    exprs <- parse(input_file, keep.source = TRUE)
    
    starts <- vapply(exprs, function(e) attr(e, "srcref")[[1]], integer(1))
    
    out <- character(0)
    current_expr <- 1
    
    for (i in seq_along(lines)) {
        if (current_expr <= length(starts) && i == starts[current_expr]) {
            out <- c(out, sprintf('message("%d")', i))
            current_expr <- current_expr + 1
        }
        out <- c(out, lines[i])
    }
    
    writeLines(out, output_file)
    message("Created: ", output_file)
    invisible(output_file)
}
