#' Open PDF Graphics Device
#'
#' @name pdf.start
#'
#' Opens a PDF device with sensible defaults for publication-quality plots.
#' Uses Cairo on macOS for better font rendering. Automatically closes
#' a previously unclosed PDF device.
#'
#' @param pdffilename Character. Output filename (extension added if missing).
#'   If no path separator, auto-places in \code{pdf/}, \code{figs/}, or \code{fig/}.
#' @param scale Numeric. Scale factor for dimensions (default 0.75).
#' @param wd Numeric. Base width in inches (default 9).
#' @param ht Numeric. Base height in inches (default 6).
#' @param pointsize Numeric. Font size; if negative, computed as 12/scale.
#' @param embed Numeric. Font embedding flag (default 2).
#' @param verbose Logical. Print status message (default \code{TRUE}).
#' @param mar Numeric vector of length 4. Plot margins (default \code{c(4.1, 4.1, 1.1, 1.1)}).
#' @param mgp Numeric vector of length 3. Margin line settings (default \code{c(2.5, 0.75, 0)}).
#' @param ... Additional arguments passed to the PDF device function.
#'
#' @return Called for side effect. Opens a PDF device.
#'
#' @examples
#' \dontrun{
#' # Open a PDF, draw a plot, close it
#' iaw$pdf.start("myplot")
#' plot(1:10, main = "Test")
#' iaw$pdf.end()
#'
#' # Smaller custom dimensions
#' iaw$pdf.start("narrow", wd = 5, ht = 4)
#' hist(rnorm(200))
#' iaw$pdf.end()
#'
#' # Publication-quality figure with tight margins
#' iaw$pdf.start("pub_fig", wd = 7, ht = 5,
#'               mar = c(3.5, 3.5, 0.5, 0.5), mgp = c(2, 0.6, 0))
#' plot(1:20, cumsum(rnorm(20)), type = "l", lwd = 2,
#'      xlab = "Trading day", ylab = "Cumulative return")
#' iaw$pdf.end()
#'
#' # Larger pointsize for presentation slides
#' iaw$pdf.start("slides/overview", wd = 12, ht = 8, pointsize = 18)
#' barplot(c(2.1, 3.5, 4.8), names.arg = c("2023", "2024", "2025"),
#'         col = "steelblue", main = "Annual revenue ($B)")
#' iaw$pdf.end()
#' }
#'
#' @family plotting
#' @export
#'
#' @seealso [iaw$pdf.end()]
iaw$pdf.start <- function (pdffilename,
                           scale = 0.75, wd = 9, ht = 6, pointsize = (-1),
                           embed = 2,
                           verbose = TRUE,
                           mar = c(4, 4, 1, 1) + 0.1,
                           mgp = c(2.5, 0.75, 0), ...)
{
    # Validate inputs
    (iaw$is.character(pdffilename, 1)) %or% stop(paste("pdffilename ", pdffilename, " has to be a string"))
    (iaw$is.numeric(scale, 1) & (scale > 0)) %or% stop(paste("scale ", scale, " is wrong"))
    (iaw$is.numeric(wd, 1) & (wd > 0)) %or% stop(paste("wd ", wd, " is wrong"))
    (iaw$is.numeric(ht, 1) & (ht > 0)) %or% stop(paste("ht ", ht, " is wrong"))

    # Close previous PDF if not closed
    pdf.openfile <- options("pdf.openfile")$pdf.openfile
    if (!is.null(pdf.openfile)) {
        message("[you forgot to invoke pdf.end() for ", pdf.openfile[1], ". I am doing it for you]\n")
        iaw$pdf.end()
    }

    # Ensure correct file extension
    (grepl(".eps$", pdffilename)) %and% stop(paste("eps file extension in ", pdffilename, " is not allowed"))
    pdffilename <- paste0(gsub("\\.pdf", "", pdffilename, ignore.case = TRUE), ".pdf")

    # Place file in default directories if no path
    if (!grepl("/", pdffilename)) {
        for (dir in c("pdf/", "figs/", "fig/")) {
            if (dir.exists(dir)) {
                pdffilename <- paste0(dir, pdffilename)
                break
            }
        }
    }

    # Adjust pointsize
    if (pointsize < 0) pointsize <- 12 / scale
    scale <- 1
    width <- wd * scale
    height <- ht * scale

    if (verbose) iaw$cat.stderr("[pdf.start: ", pdffilename, "]", sep = "")
    options(pdf.openfile = c(file = pdffilename))

    # Determine script name
    scriptname <- getOption("Rscriptname", NA)
    if (is.na(scriptname) || scriptname == "") {
        cmdarg <- commandArgs(trailingOnly = FALSE)
        if (length(cmdarg) > 0 && !grepl("R$", cmdarg[1])) {
            scriptname <- basename(cmdarg[1])
        } else {
            scriptname <- "interactive"
        }
    }

    # Format title
    title_text <- paste0(
        getwd(), ": ",
        basename(pdffilename), " by ",
        scriptname, " on ",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ."
    )

    # Open PDF device
    if (getOption("os") == "macos") {
        if (!requireNamespace("Cairo", quietly = TRUE)) stop("Package 'Cairo' required")
        pdf <- Cairo::CairoPDF
        Cairo::CairoFonts(regular = "Charter:style=Regular")
    } else {
        message("not yet worked out how to pass a fontfamily on linux")
    }

    pdf(file = pdffilename,
        width = width, height = height, pointsize = pointsize, version = 1.4,
        title = title_text,
        ...)
    iaw$.Riolog("P", pdffilename)

    # Set plot parameters
    par(las = 1)
    par(mar = mar)
    par(mgp = mgp)
}
