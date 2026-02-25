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
#' @examples
#' \dontrun{
#' # Draw a basic world map outline
#' iaw$plot.worldmap()
#'
#' # Draw with fill and highlight a region
#' iaw$plot.worldmap(fill = TRUE, col = "lightgray")
#' maps::map("world", regions = "USA", fill = TRUE, col = "steelblue", add = TRUE)
#'
#' # Restrict to Europe
#' iaw$plot.worldmap(xlim = c(-10, 40), ylim = c(35, 70),
#'                   fill = TRUE, col = "lightyellow")
#' }
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
