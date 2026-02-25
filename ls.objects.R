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
#' \dontrun{
#' # List all objects in the global environment, largest first
#' iaw$ls.objects()
#'
#' # Create some objects and inspect sizes
#' big   <- matrix(rnorm(1e6), ncol = 1000)
#' small <- 1:10
#' iaw$ls.objects()   # big should appear at the top
#'
#' # Limit to top 5 objects
#' iaw$ls.objects(n = 5)
#'
#' # Inspect a specific environment
#' e <- new.env()
#' e$x <- rnorm(1000)
#' e$y <- "hello"
#' iaw$ls.objects(envir = e)
#' }

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
