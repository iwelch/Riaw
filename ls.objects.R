#' List Objects with Sizes
#'
#' @name ls.objects
#'
#' Lists objects in environment with memory sizes.
#'
#' @param envir Environment to list.
#' @param n Number to show.
#'
#' @return Data frame of objects and sizes.
#'
#' @family utilities
#' @export
#'
#' @examples
#' iaw$ls.objects()

iaw$ls.objects <- function(envir = .GlobalEnv, n = 20) {
    objs <- ls(envir = envir)
    if (length(objs) == 0) return(data.frame())
    
    sizes <- sapply(objs, function(x) object.size(get(x, envir = envir)))
    df <- data.frame(
        object = objs,
        size_MB = round(sizes / 1024^2, 2),
        stringsAsFactors = FALSE
    )
    df <- df[order(-df$size_MB), ]
    head(df, n)
}
