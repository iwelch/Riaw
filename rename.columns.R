#' Rename Columns in Data Frame
#'
#' @name rename.columns
#'
#' Renames columns using from/to mapping or named vector.
#'
#' @param df Data frame or list.
#' @param from Current names or named vector (old=new).
#' @param to New names (ignored if from is named).
#'
#' @return Modified data frame or list.
#'
#' @family data-manipulation
#' @export
#'
#' @examples
#' df <- data.frame(a = 1, b = 2)
#' iaw$rename.columns(df, c(a = "x", b = "y"))
#'
#' # Rename a single column using from/to vectors
#' df <- data.frame(old_name = 1:3, keep = 4:6)
#' iaw$rename.columns(df, "old_name", "new_name")
#'
#' # Rename multiple columns in a pipeline-friendly way
#' df <- data.frame(ret = c(0.01, -0.02), vol = c(0.1, 0.2), id = 1:2)
#' iaw$rename.columns(df, c(ret = "return", vol = "volatility"))
#'
#' # Rename using paired from/to vectors
#' df <- data.frame(mktcap = 1:3, bm = 4:6, permno = 7:9)
#' iaw$rename.columns(df, c("mktcap", "bm"), c("size", "book_to_market"))
#' # columns: size, book_to_market, permno
#'
#' # Only matched names change; unmatched are left alone
#' df <- data.frame(x = 1, y = 2, z = 3)
#' names(iaw$rename.columns(df, c(x = "a")))  # c("a", "y", "z")
#'
#' # Works on lists too (e.g., renaming list elements)
#' lst <- list(old1 = 10, old2 = 20)
#' iaw$rename.columns(lst, c(old1 = "new1", old2 = "new2"))

iaw$rename.columns <- function(df, from, to = NULL) {
    stopifnot(is.list(df))
    nms <- names(df)

    if (is.null(to)) {
        to <- as.character(from)
        from <- names(from)
    }

    stopifnot(length(from) == length(to))

    for (i in seq_along(from)) {
        nms[nms == from[i]] <- to[i]
    }

    names(df) <- as.character(nms)
    df
}

#' @rdname rename.columns
#' @export
iaw$rename.column <- iaw$rename.columns
