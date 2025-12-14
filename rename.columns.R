#' Rename Columns in a Data Frame
#'
#' Renames columns in a data frame or elements in a character vector. Supports
#' both explicit from/to mapping and named vector syntax.
#'
#' @param d.or.n A data frame or character vector of names to modify.
#' @param from Character vector of current names, OR a named vector where
#'   names are current names and values are new names.
#' @param to Character vector of new names (same length as \code{from}).
#'   Ignored if \code{from} is a named vector.
#'
#' @return If \code{d.or.n} is a data frame, returns the data frame with
#'   renamed columns. If \code{d.or.n} is a character vector, returns the
#'   modified vector.
#'
#' @export
#'
#' @seealso \code{\link{iaw$recode}}, \code{\link{names}}, \code{\link{setNames}}
#'
#' @examples
#' # Create sample data
#' df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
#'
#' # Method 1: Explicit from/to vectors
#' iaw$rename.columns(df, from = c("a", "b"), to = c("x", "y"))
#' #   x y c
#' # 1 1 4 7
#' # ...
#'
#' # Method 2: Named vector (old = new)
#' iaw$rename.columns(df, c(a = "alpha", c = "gamma"))
#' #   alpha b gamma
#' # 1     1 4     7
#' # ...
#'
#' # Works on character vectors too
#' old_names <- c("var1", "var2", "var3")
#' iaw$rename.columns(old_names, c(var1 = "x", var3 = "z"))
#' # "x" "var2" "z"

iaw$rename.columns <- function(d.or.n, from, to = NULL) {
    nms <- if (is.data.frame(d.or.n)) names(d.or.n) else d.or.n

    if (is.null(to)) {
        to <- as.character(from)
        from <- names(from)
    }

    for (i in seq_along(from)) {
        nms[nms == from[i]] <- to[i]
    }

    if (is.data.frame(d.or.n)) {
        names(d.or.n) <- as.character(nms)
        return(d.or.n)
    } else {
        return(as.character(nms))
    }
}

#' @rdname rename.columns
#' @export
iaw$rename.column <- iaw$rename.columns
