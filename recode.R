
#' RECODE
#'
#' @name recode
#'
#' recode the values of a factor or factor-like variable
#'
#' @usage recode (x, from, to, non.from.becomes.na =FALSE)
#'
#' @param x a vector
#' @param from original values
#' @param to replacement values.
#'
#' @return another vector
#'


iaw$recode <- function( x, from, to, non.from.becomes.na =FALSE ) {
  (length(from)==length(to)) %or% "bad from to length in recode"
  y <- if (non.from.becomes.na) rep(NA, length(x)) else x
  for (f in 1:length(from)) y[x==from[f]] <- to[f]
                                        #  not necessary: y[is.na(x)] <- NA
  y
}
