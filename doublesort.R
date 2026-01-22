## #6 doublesort.R

#' Double Sort for Portfolio Formation
#'
#' Given two criteria, sort by criterion 1 into groups, then rank within
#' each group by criterion 2. Middle observations removed if N not divisible.
#'
#' @param crit.holdconstant First sort criterion (forms groups).
#' @param crit.spread Second sort criterion (ranks within groups).
#' @param NUMPERGROUP Observations per group. Default 5.
#'
#' @return Vector of within-group ranks (1 to NUMPERGROUP), NA for excluded obs.
#'
#' @family data-manipulation
#' @export

iaw$.mkusefullist <- function(N, NUMPERGROUP) {
  NUMGROUPS <- N %/% NUMPERGROUP
  KEEP <- NUMGROUPS * NUMPERGROUP
  RMNUM <- N - KEEP
  HALFKEEP <- KEEP %/% 2
  c(seq_len(HALFKEEP), rep(NA, RMNUM), HALFKEEP + seq_len(HALFKEEP))
}

iaw$doublesort <- function(crit.holdconstant, crit.spread, NUMPERGROUP = 5) {
  stopifnot(length(crit.holdconstant) == length(crit.spread))
  stopifnot(length(crit.holdconstant) >= NUMPERGROUP)
  stopifnot(NUMPERGROUP > 1)

  N <- length(crit.holdconstant)
  uselist <- iaw$.mkusefullist(N, NUMPERGROUP)

  ## assign group numbers after sorting by crit.holdconstant
  grouporder <- order(crit.holdconstant)
  groupnum <- ((uselist - 1) %/% NUMPERGROUP) + 1

  ## tag each observation with its group
  grp <- rep(NA_integer_, N)
  grp[grouporder] <- groupnum

  ## track which observations are excluded
  excluded <- grouporder[is.na(groupnum)]

  ## rank within groups by crit.spread
  r <- ave(crit.spread, grp, FUN = function(x) rank(x, ties.method = "max"))
  r[excluded] <- NA
  r
}

## tests
if (FALSE) {
  stopifnot(identical(iaw$.mkusefullist(9, 3), 1:9))
  stopifnot(identical(iaw$.mkusefullist(10, 3), c(1:4, NA, 5:9)))
  stopifnot(identical(iaw$.mkusefullist(11, 3), c(1:4, NA, NA, 5:9)))

  crit1 <- c(4, 9, 13, 11, 5, 7, 10, 1, 12, 8, 3, 6, 2)
  crit2 <- c(-1, 10, 104, 103, 22, -99, 102, 1, 105, 11, 2, 33, -2)
  result <- iaw$doublesort(crit1, crit2, NUMPERGROUP = 4)
  stopifnot(identical(result, c(2, 1, 3, 2, 3, NA, 1, 3, 4, 2, 4, 4, 1)))
}
