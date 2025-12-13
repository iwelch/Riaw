
#' RANKS IN GROUPS
#'
#' @name rank
#'
#' @seealso pctrank for an alternative implementation
#'
#'  @details if ngroups>0, then ranks are from 1 to ngroups.  if ngroup==0, then return quantiles
#'
#'  @usage rank (x, na.last = TRUE, ties.method = c("average", "first", "random", "max", "min"), ngroups =NULL)
#'
#'  @param ties.method = c("average", "first", "random", "max", "min")
#'
#'  @return
#'
#' @examples
#'    iaw$rank( runif(100000), ngroups=5 )
#'
#' @seealso pctrank for an alternative implementation

iaw$rank <- function( x, na.last = TRUE, ..., ngroups =NULL) {
  if (is.null(ngroups)) return( base:::rank(x, na.last, ...) )
  ## with groups, we determine the na.last method differently
  quantiles <- rank(x, na.last="keep")/(sum(!is.na(x)))
  if ((ngroups==0)|(ngroups=="quantiles")) return(quantiles)
  return( 1 + as.integer(quantiles*(ngroups-0.000001)) )
}
