
#' Check if All Variables Exist
#'
#' @name require.variables
#'
#' check that variables exist in a list
#'
#'  @usage require.variables (names, d)
#'
#'  @param names the names that need to exist
#'  @param d the environment
#'
#'  @return aborts if a variable does not exist
#'
#' @note the R `exists` functions gives a strange answer for a vector argument as in exists( v1, v2 ).  If v1 exists but v2 does not, the answer is not 'TRUE FALSE' but 'TRUE'
#'

iaw$require.variables <- function(vnames, d) {
    (is.vector(vnames) & (class(vnames) == "character")) %or% "[require.variables: {{iaw$whatis(vnames)}} is wrong type.]
"
    names.d <- if (is.data.frame(d)) names(d) else d
    (is.character(names.d)) %or% "[require.variables: incorrect call. second arg should be data frame or names, not {{typeof(names.d)}}.]
"
    set.not.there <- (!( vnames %in% names.d ))
    if (any(set.not.there)) {
        ds <- iaw$strcat(names.d)
        ws <- iaw$strcat(vnames[set.not.there])
        FALSE %or% "Variable Name Set {{ws}} not found in Names {{ds}}"
    }
}

iaw$select <- iaw$require.variables
