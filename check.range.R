
#' CHECK.RANGE
#'
#' @name check.range
#'
#' check that all (numeric) variables in a list of variable names are in this range
#'
#' @usage check.range(names, d, range)
#'
#' @param names the name of all variables in d to be checked
#' @param d the name of all variables to be checked
#' @param range valid range
#'
#' @return
#'


iaw$check.range <- function (names, d, range) {
    iaw$require.variables(names, d)
    for (i in 1:length(names)) {
        name <- names[i]
        v <- d[[name]]
        (!(all((is.na(v) | ((v >= range[1]) & (v <= range[2])))))) %or%
        cat("check.range fails for '", name, "', intended range='", range, "'. ", file=stderr())
        for (j in 1:nrow(d)) {
            ((is.na(v[j]) | ((v[j] >= range[1]) & (v[j] <= range[2])))) %or% "Incorrect at index {{j}} which has value {{v[j]}}"
        }
    }
}
