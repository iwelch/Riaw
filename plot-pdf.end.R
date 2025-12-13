preamble <- c(doc= '
@TITLE pdf.end
@AUTHOR ivo.welch@gmail.com
@DATE 2013
@DESCRIPTION
@USAGE pdf.end (verbose = 1)
@ARGUMENTS
@DETAILS
@SEEALSO on.exit
@EXAMPLES
', test= '
', changes= '
')

iaw$pdf.end <- function (verbose = 1)
{
    if (isTRUE(getOption('knitr.in.progress'))) return

  dev.off()
                                        #  embedFonts( file=getOption("pdf.openfile"), fontpaths = getOption("pfbdir") )
  if (verbose) iaw$cat.stderr("[pdf.end]\n")
  options(pdf.openfile = NULL)
}
