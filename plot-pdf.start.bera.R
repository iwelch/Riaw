preamble <- c(doc= '
@TITLE pdf.start.bera
@AUTHOR ivo.welch@gmail.com
@DATE 2013
@DESCRIPTION
@USAGE pdf.start.bera (pdfbasename, ...) 
@ARGUMENTS
@DETAILS
@SEEALSO
@EXAMPLES
', test= '
', changes= '
')



iaw$pdf.start.bera <- function (pdfbasename, ...) 
{
  options(pdf.openfile = pdfbasename)
  bera.dir <- "/usr/local/texlive/2014/texmf-dist/fonts/afm/public/bera/"

  bera.names <- c("fvsr8a","fvsb8a", "fvsro8a","fvsbo8a")
  berasans <- Type1Font(family="BeraSans", metrics=paste0(bera.dir, "/", bera.names, ".afm"))
  pdf(file = paste0(pdfbasename, ".PDF"), family = berasans, ...)
}
