
#' DIFFID.FOR.FIXED.EFFECTS
#'
#' @name diffid.for.fixed.effects
#'
#' fixed effects utility routine
#'
#' @usage diffid.for.fixed.effects (h, id)
#'
#' @param h the variable of interest
#' @param id the fixed id
#'
#' @return differenced out values for use in fixed-effects regressions
#'
#'  @examples
#'     dx <- data.frame( x1 = rnorm(200) )
#'     id <- rbinom(200,5,0.5)
#'     y <- rnorm(200)
#'     newx <- diffid.for.fixed.effects(dx, id)
#'     newy <- diffid.for.fixed.effects(y, id)
#'     iaw$printolm( newy ~ newx )
#'


iaw$diffid.for.fixed.effects <- function (h, id) {
  id <- droplevels(as.factor(id))
  xnew <- apply(as.matrix(h), 2, function(x) x - tapply(x, id, mean)[id])
  rownames(xnew) <- NULL
  xnew
}
