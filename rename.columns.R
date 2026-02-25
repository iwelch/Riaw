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
