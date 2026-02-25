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
#'
#' # Asia-Pacific region with title
#' iaw$plot.worldmap(xlim = c(60, 180), ylim = c(-50, 60),
#'                   fill = TRUE, col = "linen")
#' title("Asia-Pacific Region")
#'
#' # Add data points on a world map (e.g., city GDP centers)
#' iaw$plot.worldmap(fill = TRUE, col = "gray90")
#' points(c(-74, 0, 139.7), c(40.7, 51.5, 35.7),
#'        pch = 16, col = "red", cex = 2)  # NYC, London, Tokyo
#' text(c(-74, 0, 139.7), c(40.7, 51.5, 35.7),
#'      c("NYC", "London", "Tokyo"), pos = 3, cex = 0.8)
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
