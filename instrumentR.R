#' INSTRUMENTR
#'
#' @name instrumentR
#'
#' takes a source R file and creates a destination R file with messages that makes it easier to find the bug
#'
#' @usage instrumentR("mycode.R"); base::source("debug-mycode.R")
#'
#' @param input_file --- a .R or .Rinclude file (that has a bug)
#' @param output_file --- optional
#'
#' @return output_file
#'


instrumentR <- function(input_file, output_file =NULL) {
    if (output_file == NULL) output_file <- paste0("debug-", input_file)

    stopifnot( grepl("*\\.R*", input_file) )

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

  # Build output
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
    invisible(output_file)
}
