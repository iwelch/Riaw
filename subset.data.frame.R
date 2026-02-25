#' Enhanced Subset for Data Frames
#'
#' @name subset.data.frame
#'
#' An enhanced version of base R's \code{subset.data.frame} that provides
#' informative error messages when selecting undefined columns. Works with
#' both character vectors and non-standard evaluation (NSE) column selection.
#'
#' @param x A data frame to subset.
#' @param subset Logical expression indicating rows to keep. Evaluated in the
#'   context of \code{x}, so column names can be used directly.
#' @param select Expression indicating columns to select. Can be:
#'   \itemize{
#'     \item A character vector: \code{c("a", "b")}
#'     \item NSE column names: \code{c(a, b)} or \code{a:c}
#'   }
#' @param drop Passed to \code{[.data.frame}. Default \code{FALSE}.
#' @param ... Additional arguments (checked but not used).
#'
#' @return A data frame containing the selected rows and columns.
#'
#' @details
#' Unlike base \code{subset.data.frame}, this version validates column names
#' before subsetting and provides clear error messages listing which columns
#' are undefined. This catches typos and misspellings early.
#'
#' To replace the base version globally, use:
#' \preformatted{
#' ns <- getNamespace("base")
#' unlockBinding("subset.data.frame", ns)
#' assign("subset.data.frame", iaw$subset.data.frame, envir = ns)
#' lockBinding("subset.data.frame", ns)
#' }
#'
#' Or use \code{trace()} for a non-invasive hook.
#'
#' Suggest also:
#'   ns <- getNamespace("base")
#'   unlockBinding("subset.data.frame", ns)
#'   assign("subset.data.frame", iaw$subset.data.frame, envir = ns)
#'   lockBinding("subset.data.frame", ns)
#'
#' @family data-manipulation
#' @export
#'
#' @examples
#' d <- data.frame(a = 1:5, b = 6:10, c = 11:15)
#'
#' # Basic row + column subset
#' iaw$subset.data.frame(d, a > 2, select = c("a", "b"))
#'
#' # Row filter only (all columns kept)
#' iaw$subset.data.frame(d, b >= 8)
#'
#' # NSE column selection works when installed as base replacement
#' \dontrun{
#' subset(d, a > 2, select = c(a, b))
#' subset(d, a <= 3, select = a:b)
#' }
#'
#' # Invalid columns give informative errors
#' \dontrun{
#' iaw$subset.data.frame(d, TRUE, select = c("a", "typo"))
#' # Error: in `[.data.frame`(x, r, vars, drop = drop) :
#' #   undefined columns in 'select': typo
#'
#' iaw$subset.data.frame(d, TRUE, select = c(a, typo))
#' # Error: in `[.data.frame`(x, r, vars, drop = drop) :
#' #   undefined columns in 'select': typo
#' }
#'
#' @seealso \code{\link[base]{subset.data.frame}}, \code{\link{check.names}}

iaw$subset.data.frame <- function (x, subset, select, drop = FALSE, ...)
{
  chkDots(...)

  ## ---- row subset ----
  r <- if (missing(subset)) {
    rep_len(TRUE, nrow(x))
  } else {
    e <- substitute(subset)
    r <- eval(e, x, parent.frame())
    if (!is.logical(r))
      stop("'subset' must be logical", call. = FALSE)
    r & !is.na(r)
  }

  ## ---- column subset ----
  vars <- if (missing(select)) {
    rep_len(TRUE, ncol(x))
  } else {
    sel_expr <- substitute(select)

    ## evaluate once in caller to detect character selection
    sel_val <- try(eval(sel_expr, parent.frame()), silent = TRUE)

    if (is.character(sel_val)) {
      ## character selection: c("a", "yz")
      missing_vars <- setdiff(sel_val, names(x))
      if (length(missing_vars)) {
        stop(
            "in `[.data.frame`(x, r, vars, drop = drop) :\n  ",
          "undefined columns in 'select': ",
          paste(missing_vars, collapse = ", "),
          call. = FALSE
        )
      }
      sel_val
    } else {
      ## NSE selection: c(a, c), a:c, etc.
      sel_vars <- all.vars(sel_expr)
      missing_vars <- setdiff(sel_vars, names(x))

      if (length(missing_vars)) {
        stop(
            "in `[.data.frame`(x, r, vars, drop = drop) :\n  ",
          "undefined columns in 'select': ",
          paste(missing_vars, collapse = ", "),
          call. = FALSE
        )
      }

      nl <- as.list(seq_along(x))
      names(nl) <- names(x)
      eval(sel_expr, nl, parent.frame())
    }
  }

  x[r, vars, drop = drop]
}
