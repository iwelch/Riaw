#' Kable Wrapper
#'
#' @name kable
#'
#' Wrapper for knitr::kable.
#'
#' @param df Table to format.
#' @param both indicator whether to also give plain text output
#' @param format request to underlying kable
#' @param booktabs usually TRUE
#' @param linesep usually outputs \\addlinespace every 6th row
#'
#' @return Formatted table.
#'
#' @family utilities
#' @export


iaw$align_kable <- function(kable_output) {

    ## unfortunately, this corrupts the numeric table into a string one, and then all hell breaks loose --- like '\n' in the string instead of just
    ## but it would need to come first, because otherwise the indentation fails again.  grrr...
    ## [1] add "," into integers, surrounded by spaces
    ## regex_pattern <- "(?<= )\\d+(?= )"
    ## m <- gregexpr(regex_pattern, kable_output, perl = TRUE)
    ## regmatches(kable_output, m) <- lapply(regmatches(kable_output, m), function(x) { prettyNum(x, big.mark = ",") })


  lines <- unlist(strsplit(as.character(kable_output), "\n"))

  # Find lines with & (data rows)
  data_lines <- grep("&", lines)

  if (length(data_lines) == 0) return(kable_output)

  # Split each data line by &
  split_lines <- lapply(lines[data_lines], function(x) { strsplit(x, "&")[[1]] })

  # Find max width for each column
  n_cols <- max(sapply(split_lines, length))
  col_widths <- sapply(1:n_cols, function(i) {
    max(sapply(split_lines, function(x) {
      if (i <= length(x)) nchar(trimws(x[i])) else 0
    }))
  })

  # Pad each cell and rejoin
  aligned <- sapply(split_lines, function(cells) {
    padded <- sapply(1:length(cells), function(i) {
      cell <- trimws(cells[i])
      # Right-pad for first column (rownames), left-pad for numbers
      if (i == 1) {
        sprintf("%-*s", col_widths[i], cell)
      } else {
        sprintf("%*s", col_widths[i], cell)
      }
    })
    paste(padded, collapse = " & ")
  })

  lines[data_lines] <- aligned

  result <- paste(lines, collapse = "\n")

  ## this is beyond align, and fixes up the table to my preferences now

  ## [1] indent all but the first and last tabular lines
  lines <- unlist(strsplit(result, "\n"))
                                        # Indent lines only if they do NOT start with \begin or \end
  to_indent <- !grepl("^\\\\(begin|end)\\{tabular\\}", trimws(lines))

  lines[to_indent] <- paste0("  ", lines[to_indent])
  result <- paste(lines, collapse = "\n")

  ## [2] replace tabular by ctabular
  result <- gsub("\\{tabular\\}", "{ctabular}", result)  ## an iaw-ism.  just define ctabular in your latex

  ## [3] add a space before the final \\
  result <- gsub("\\\\\\\\", " \\\\\\\\", result)

  class(result) <- class(kable_output)
  attr(result, "format") <- attr(kable_output, "format")
  result
}




iaw$kable <- function( df, both=FALSE, format="latex", booktabs=TRUE, linesep="", ... ) {
    if (both) print( df )
    o <- kable( df, format=format,  booktabs=booktabs, linesep=linesep, ... )
    if (format=="latex") iaw$align_kable(o) else o

}
