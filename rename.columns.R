#' Rename Columns in Data Frame
#'
#' @name rename.columns
#'
#' Renames columns using from/to mapping or named vector.
#'
#' @param d.or.n Data frame or character vector.
#' @param from Current names or named vector (old=new).
#' @param to New names (ignored if from is named).
#'
#' @return Modified data frame or character vector.
#'
#' @family data-manipulation
#' @export
#'
#' @examples
#' df <- data.frame(a = 1, b = 2)
#' iaw$rename.columns(df, c(a = "x", b = "y"))

iaw$rename.columns <- function(d.or.n, from, to = NULL) {
    nms <- if (is.data.frame(d.or.n)) names(d.or.n) else d.or.n
    
    if (is.null(to)) {
        to <- as.character(from)
        from <- names(from)
    }
    
    stopifnot(length(from) == length(to))
    
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

iaw$rename.column <- iaw$rename.columns
