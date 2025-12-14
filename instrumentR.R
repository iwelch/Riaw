#' Instrument R Source Files for Debugging
#'
#' Takes an R source file and creates a new version with \code{message()} calls
#' inserted before each top-level expression. This makes it easier to identify
#' which line caused an error when debugging.
#'
#' @param input_file Path to an R source file (must end in \code{.R} or similar).
#' @param output_file Optional output path. Defaults to \code{"debug-<input_file>"}.
#'
#' @return The path to the output file (invisibly).
#'
#' @details
#' The function parses the input file to identify top-level expressions, then
#' inserts \code{message("<line_number>")} before each one. When the instrumented
#' file is run and encounters an error, the last printed line number indicates
#' approximately where the error occurred.
#'
#' This is particularly useful for debugging scripts that fail during batch
#' execution where interactive debugging isn't available.
#'
#' @note After identifying the problematic area, use \code{browser()}, 
#' \code{debug()}, or \code{iaw$source.debug()} for detailed debugging.
#'
#' @export
#'
#' @seealso \code{\link{iaw$source.debug}}, \code{\link{iaw$debug.advice}},
#'   \code{\link{traceback}}
#'
#' @examples
#' \dontrun{
#' # Create instrumented version of a script
#' instrumentR("analysis.R")
#' # Creates "debug-analysis.R" with line number messages
#'
#' # Run the instrumented version
#' base::source("debug-analysis.R")
#' # Output shows line numbers as they execute:
#' # 1
#' # 5
#' # 12
#' # Error in ... (occurred after line 12)
#'
#' # Custom output filename
#' instrumentR("analysis.R", "analysis_debug.R")
#' }
#'
#' # Example of what instrumented code looks like:
#' # Original:
#' #   x <- 1:10
#' #   y <- mean(x)
#' #
#' # Instrumented:
#' #   message("1")
#' #   x <- 1:10
#' #   message("2")
#' #   y <- mean(x)

instrumentR <- function(input_file, output_file = NULL) {
    if (is.null(output_file)) {
        output_file <- paste0("debug-", input_file)
    }

    stopifnot(grepl("\\.R", input_file, ignore.case = TRUE))

    # Read original lines
    lines <- readLines(input_file, warn = FALSE)

    # Parse with source references
    exprs <- parse(input_file, keep.source = TRUE)

    # Get start line of each top-level expression
    starts <- vapply(
        exprs,
        function(e) attr(e, "srcref")[[1]],
        integer(1)
    )

    # Build output with message() calls inserted
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
    message("Created instrumented file: ", output_file)
    invisible(output_file)
}
