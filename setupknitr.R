#' Setup Knitr Options
#'
#' @name setupknitr
#'
#' Sets default knitr chunk options.
#'
#' @return Invisible NULL.
#'
#' @examples
#' \dontrun{
#' # Call at the top of an R Markdown document to apply standard chunk defaults
#' iaw$setupknitr()
#'
#' # After calling, individual chunks can still override individual options
#' # ```{r, fig.width=10}
#' # ... wider plot ...
#' # ```
#' }
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
