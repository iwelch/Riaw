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
    if (file.exists("Rio.log")) cat("[P]\t", pdffilename, "\t<-\t", getOption("Rscriptname"), "\t", Sys.time(), "\n", file= "Rio.log", append=TRUE)

    # Set plot parameters
    par(las = 1)
    par(mar = mar)
    par(mgp = mgp)
}
