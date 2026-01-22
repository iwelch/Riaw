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

iaw$instrumentR <- function(input_file, output_file = NULL) {
    stopifnot(is.character(input_file), length(input_file) == 1L)
    stopifnot(grepl("\\.R", input_file, ignore.case = TRUE))

    if (is.null(output_file)) {
        output_file <- paste0("debug-", input_file)
    }

    lines <- readLines(input_file, warn = FALSE)

    # Check if a line looks incomplete (would continue on next line)
    is_incomplete <- function(line) {
        # Remove strings to avoid false positives
        line_clean <- gsub('"[^"]*"', '""', line)
        line_clean <- gsub("'[^']*'", "''", line_clean)
        # Remove comments
        line_clean <- sub("#.*$", "", line_clean)

        trimmed <- trimws(line_clean)
        if (trimmed == "") return(FALSE)

        # Ends with continuation indicators
        grepl("[,+*/|&<>=-]\\s*$|\\|>\\s*$|%[^%]*%\\s*$", trimmed) ||
        # Has unclosed parens/brackets
        nchar(gsub("[^({[]", "", line_clean)) > nchar(gsub("[^)}\\]]", "", line_clean))
    }

    out <- character(0)

    for (i in seq_along(lines)) {
        trimmed <- trimws(lines[i])

        # Should we instrument before this line?
        should_instrument <- FALSE

        if (trimmed != "" && !grepl("^#", trimmed) && !grepl("^[})\\]]+", trimmed)) {
            # Check if previous line was incomplete
            if (i == 1 || !is_incomplete(lines[i - 1])) {
                should_instrument <- TRUE
            }
        }

        # Insert message if appropriate
        if (should_instrument) {
            out <- c(out, sprintf('message("Line %d")', i))
        }

        # Add the original line
        out <- c(out, lines[i])
    }

    writeLines(out, output_file)
    message("Created: ", output_file)
    invisible(output_file)
}
