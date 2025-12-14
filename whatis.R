#' Describe an R Object
#'
#' Returns a human-readable description of an R object's type and dimensions.
#' Useful for debugging and understanding unfamiliar objects.
#'
#' @param object Any R object.
#'
#' @return A character string describing the object's type and size.
#'
#' @export
#'
#' @seealso \code{\link{class}}, \code{\link{str}}, \code{\link{typeof}}
#'
#' @examples
#' iaw$whatis(NULL)
#' # "is NULL"
#'
#' iaw$whatis(1:10)
#' # "integer with length 10"
#'
#' iaw$whatis(matrix(1:12, 3, 4))
#' # "matrix with 4 cols and 3 rows"
#'
#' iaw$whatis(data.frame(a = 1:5, b = letters[1:5]))
#' # "data.frame with 2 variables and 5 observations"
#'
#' iaw$whatis(factor(c("a", "b", "a", "c")))
#' # "factor with unique keys 3"
#'
#' iaw$whatis(lm)
#' # "it is function --- use unlist() or str() to determine contents"

iaw$whatis <- function(object) {
    if (is.null(object)) {
        return("is NULL")
    } else if (is.matrix(object)) {
        return(paste(class(object)[1], "with", ncol(object), "cols and", nrow(object), "rows"))
    } else if (is.data.frame(object)) {
        return(paste(class(object)[1], "with", ncol(object), "variables and", nrow(object), "observations"))
    } else if (is.vector(object)) {
        return(paste(class(object)[1], "with length", length(object)))
    } else if (is.factor(object)) {
        return(paste(class(object)[1], "with unique keys", length(unique(object))))
    } else {
        cat("it is", class(object)[1], "--- use unlist() or str() to determine contents")
        return(class(object)[1])
    }
}
