#' Setup Knitr Options
#'
#' @name setupknitr
#'
#' Sets default knitr chunk options.
#'
#' @return Invisible NULL.
#'
#' @family utilities
#' @export

iaw$setupknitr <- function() {
    if (requireNamespace("knitr", quietly = TRUE)) {
        knitr::opts_chunk$set(
            echo = TRUE,
            message = FALSE,
            warning = FALSE,
            fig.width = 7,
            fig.height = 5
        )
    }
    invisible(NULL)
}
