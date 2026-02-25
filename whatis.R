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
#' iaw$whatis(1:10)                        # "integer (integer) length=10"
#' iaw$whatis(data.frame(a = 1))           # "data.frame (list) [1x1]"
#'
#' # Matrix shows dimensions
#' iaw$whatis(matrix(1:6, nrow = 2))       # "matrix (integer) [2x3]"
#'
#' # List and logical
#' iaw$whatis(list(a = 1, b = "x"))        # "list (list) length=2"
#' iaw$whatis(c(TRUE, FALSE, TRUE))        # "logical (logical) length=3"
#'
#' # Useful for quick inspection inside scripts
#' x <- rnorm(50)
#' message(iaw$whatis(x))                  # "numeric (double) length=50"
#'
#' # NULL and empty vector
#' iaw$whatis(NULL)                         # "NULL (NULL) length=0"
#' iaw$whatis(integer(0))                   # "integer (integer) length=0"
#'
#' # Functions are described too
#' iaw$whatis(mean)                         # "function (builtin) length=1"
#'
#' # Time-series object shows its class
#' iaw$whatis(Sys.time())                   # "POSIXct (double) length=1"
#'
#' # Environment
#' iaw$whatis(globalenv())                  # "environment (environment) length=..."

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
