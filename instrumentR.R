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
#' @examples
#' \dontrun{
#' # Instrument myscript.R -> debug-myscript.R
#' iaw$instrumentR("myscript.R")
#'
#' # Specify a custom output path
#' iaw$instrumentR("analysis.R", output_file = "/tmp/analysis-debug.R")
#'
#' # Source the instrumented version to see line-by-line progress messages
#' iaw$instrumentR("myscript.R")
#' source("debug-myscript.R")
#'
#' # Instrument a long ETL pipeline to find where it stalls
#' iaw$instrumentR("etl_pipeline.R")
#' source("debug-etl_pipeline.R")  # each top-level line prints "Line N"
#'
#' # Save debug version alongside the original for comparison
#' iaw$instrumentR("model.R", output_file = "model-instrumented.R")
#' }
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

    # Strip strings and comments from a line for bracket/operator analysis
    clean_line <- function(line) {
        line <- gsub('"[^"]*"', '""', line)
        line <- gsub("'[^']*'", "''", line)
        sub("#.*$", "", line)
    }

    # Check if a line ends with a continuation operator (ignoring brackets)
    ends_with_operator <- function(line) {
        trimmed <- trimws(clean_line(line))
        if (trimmed == "") return(FALSE)
        grepl("[,+*/|&<>=-]\\s*$|\\|>\\s*$|%[^%]*%\\s*$", trimmed)
    }

    # Check if a line is a continuation that must stay attached to previous line
    is_continuation <- function(line, prev_eff = "") {
        if (grepl("^\\s*(else|\\}\\s*else)", line)) return(TRUE)
        if (grepl("^\\s*[|%]", line)) return(TRUE)
        # Opening brace after ) or else/repeat is a block body, not a new statement
        if (grepl("^\\s*\\{", line)) {
            prev <- trimws(clean_line(prev_eff))
            if (nchar(prev) > 0 &&
                (grepl("\\)\\s*$", prev) || grepl("\\b(else|repeat)\\s*$", prev)))
                return(TRUE)
        }
        FALSE
    }

    out <- vector("list", length(lines))
    depth <- 0L  # running bracket depth across lines
    prev_effective <- ""  # last non-blank non-comment line

    for (i in seq_along(lines)) {
        trimmed <- trimws(lines[i])

        # Should we instrument before this line?
        should_instrument <- FALSE

        if (trimmed != "" && !grepl("^#", trimmed) &&
            !grepl("^[})\\]]+", trimmed) && !is_continuation(lines[i], prev_effective)) {
            # Instrument only at bracket depth 0 and not after a continuation operator
            if (depth == 0L && (i == 1L || !ends_with_operator(lines[i - 1]))) {
                should_instrument <- TRUE
            }
        }

        # Update running bracket depth from this line
        lc <- clean_line(lines[i])
        depth <- max(0L, depth +
            nchar(gsub("[^({[]", "", lc)) - nchar(gsub("[^])}]", "", lc)))

        # Insert message if appropriate
        if (should_instrument) {
            out[[i]] <- c(sprintf('message("Line %d")', i), lines[i])
        } else {
            out[[i]] <- lines[i]
        }

        # Track last effective line for continuation detection
        if (trimmed != "" && !grepl("^#", trimmed)) {
            prev_effective <- lines[i]
        }
    }

    out <- unlist(out)

    writeLines(out, output_file)
    message("Created: ", output_file)
    invisible(output_file)
}
