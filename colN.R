iaw$colN <-
function (x, na.rm = FALSE, dims = 1L) 
{
    if (is.data.frame(x)) 
        x <- as.matrix(x)
    if (!is.array(x) || length(dn <- dim(x)) < 2L) 
        stop("'x' must be an array of at least two dimensions")
    if (dims < 1L || dims > length(dn) - 1L) 
        stop("invalid 'dims'")
    n <- prod(dn[id <- seq_len(dims)])
    dn <- dn[-id]
    z <- apply( x, 2, function(o) sum( !is.na( as.numeric(o) ) ) )
    # print(z)
    if (length(dn) > 1L) {
        dim(z) <- dn
        dimnames(z) <- dimnames(x)[-id]
    }
    else names(z) <- dimnames(x)[[dims + 1L]]
    z
}
