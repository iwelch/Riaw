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

iaw$rename.column <- iaw$rename.columns
