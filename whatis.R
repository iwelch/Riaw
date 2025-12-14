#' Describe Object Type and Structure
#'
#' @name whatis
#'
#' Returns description of object type, class, and dimensions.
#'
#' @param x Any R object.
#'
#' @return Character string description.
#'
#' @family utilities
#' @export
#'
#' @examples
#' iaw$whatis(1:10)
#' iaw$whatis(data.frame(a = 1))

iaw$whatis <- function(x) {
    type <- typeof(x)
    cls <- class(x)
    len <- length(x)
    dims <- dim(x)
    
    desc <- paste0(cls[1], " (", type, ")")
    if (!is.null(dims)) {
        desc <- paste0(desc, " [", paste(dims, collapse = "x"), "]")
    } else {
        desc <- paste0(desc, " length=", len)
    }
    desc
}
