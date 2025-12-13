
iaw$MixColor <- function (col1, col2, frac=0.5) {
  .mix <- function(col1, col2, frac=0.5) {
    # calculate mix
    mix <- apply(col2rgb(c(col1, col2), alpha=TRUE), 1, function(x) frac * x[1] + (1-frac) * x[2])
    do.call("rgb", c(as.list(mix), maxColorValue=255))
  }
  m <- suppressWarnings(cbind(col1, col2, frac))
  apply(m, 1, function(x) .mix(col1=x[1], col2=x[2], frac=as.numeric(x[3])))
}
