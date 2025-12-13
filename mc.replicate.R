
#' MC.REPLICATE
#'
#' @name mc.replicate
#'
#'   replicate repeats a function call many times, unlike rep which repeats a value many times
#'
#'  @usage replicate(100, rnorm(100))
#'
#'  @param number number of calls
#'  @param expr function to call
#'  @return
#'
#'  @seealso replicate
#'


iaw$mc.replicate <- function (n, expr, simplify = "array")
    iaw$mc.sapply(integer(n), eval.parent(substitute(function(...) expr)),  simplify = simplify)
