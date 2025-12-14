#' World Map Plot
#'
#' @name plot.worldmap
#'
#' Creates basic world map.
#'
#' @param ... Map arguments.
#'
#' @return Invisible NULL.
#'
#' @family plotting
#' @export

iaw$plot.worldmap <- function(...) {
    if (requireNamespace("maps", quietly = TRUE)) {
        maps::map("world", ...)
    } else {
        warning("Package 'maps' required")
    }
    invisible(NULL)
}
