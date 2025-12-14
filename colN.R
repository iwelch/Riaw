#' Count Non-NA Values by Column
#'
#' Counts the number of non-NA values in each column of a matrix or data frame.
#'
#' @param x A matrix or data frame.
#' @param na.rm Logical; not used but included for interface consistency.
#' @param dims Integer; dimension to operate over. Default 1.
#'
#' @return A named numeric vector with counts for each column.
#'
#' @export
#'
#' @seealso \code{\link{colSums}}, \code{\link{iaw$colSds}}
#'
#' @examples
#' m <- matrix(c(1, NA, 3, 4, 5, NA, 7, 8, 9), nrow = 3)
#' colnames(m) <- c("a", "b", "c")
#' iaw$colN(m)
#' # a b c
#' # 2 2 3

iaw$colN <- function(x, na.rm = FALSE, dims = 1L) {
    if (is.data.frame(x)) x <- as.matrix(x)
    if (!is.array(x) || length(dn <- dim(x)) < 2L)
        stop("'x' must be an array of at least two dimensions")
    if (dims < 1L || dims > length(dn) - 1L)
        stop("invalid 'dims'")
    n <- prod(dn[id <- seq_len(dims)])
    dn <- dn[-id]
    z <- apply(x, 2, function(o) sum(!is.na(as.numeric(o))))
    if (length(dn) > 1L) {
        dim(z) <- dn
        dimnames(z) <- dimnames(x)[-id]
    } else {
        names(z) <- dimnames(x)[[dims + 1L]]
    }
    z
}
