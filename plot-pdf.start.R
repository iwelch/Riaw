
#' PDF.START
#'
#'  @params
#'
#'  @return
#'

iaw$pdf.start <- function (pdffilename,
                           scale = 0.75, wd = 9, ht = 6, pointsize = (-1),
                           embed = 2,
                           verbose = 1,
                           mar = c(4, 4, 1, 1) + 0.1,
                           mgp = c(2.5, 0.75, 0), ...)
{
    if (isTRUE(getOption('knitr.in.progress'))) return

    (iaw$is.character(pdffilename,1)) %or% "pdffilename {{pdffilename}} has to be a string"
    (iaw$is.numeric(scale,1) & (scale>0)) %or% "scale {{scale}} is wrong"
    (iaw$is.numeric(wd,1) & (wd>0)) %or% "wd {{wd} is wrong:"
    (iaw$is.numeric(ht,1) & (ht>0)) %or% "ht {{ht}} is wrong:"

    pdf.openfile <- options("pdf.openfile")$pdf.openfile
    if (!is.null(pdf.openfile)) {
        iaw$estring("[you forgot to invoke pdf.end() for {{pdf.openfile[1]}}.  doing it for you\n")
        iaw$pdf.end()
    }

    (grepl(".eps$", pdffilename)) %and% "eps file extension in {{pdffilename}} is not allowed"
    pdffilename <- paste0(gsub("\\.pdf", "", pdffilename, ignore.case=TRUE), ".pdf")

    if (!grepl("/", pdffilename)) {
        for (dir in c("pdf/", "figs/", "fig/")) {
            if (dir.exists(dir)) {
                pdffilename <- paste0(dir, pdffilename) }
        }
    }

    if (pointsize < 0) pointsize <- 12/scale
    scale <- 1
    width <- wd * scale
    height <- ht * scale

    if (verbose) iaw$cat.stderr("[pdf.start: ", pdffilename, "]", sep = "")
    options( pdf.openfile = c(file=pdffilename) )

    if (getOption("os")=="osx") {
        library("Cairo")
        pdf <- CairoPDF
        ## CairoFonts( regular="Bitstream Charter:style=Regular" )
        CairoFonts( regular="Charter:style=Regular" )
    } else {
        message("not yet worked out how to pass a fontfamily on linux")
    }
    pdf(file = pdffilename,
        width = width, height = height, pointsize = pointsize, version=1.4,
        title = paste("R Graphics iaw$pdf.start --", " in ", getwd(), ": ", pdffilename, iaw$now(), " -- ", paste(commandArgs(), "--", Rscriptname, collapse="")),
        ...)

    par(las = 1)  ## always horizontal labels
    par(mar = mar)  ## b, l , r, t.  (5,4,4,2)+0.1
    par(mgp = mgp) ## title, x, y (3,1,0)

    ## note: to increase space to y-label, just end y-label with a "\n"

}
