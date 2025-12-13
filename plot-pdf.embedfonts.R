preamble <- c(doc= '
@TITLE pdf.embedfonts
@AUTHOR ivo.welch@gmail.com
@DATE 2013
@DESCRIPTION
@USAGE pdf.embedfonts (fname) 
@ARGUMENTS
@DETAILS
@SEEALSO
@EXAMPLES
', test= '
', changes= '
')

iaw$pdf.embedfonts <- function (fname) 
{
  (system(paste0("ps2pdf14 -DPDFSETTINGS=/prepress -sFONTPATH=", getOption("bera")$pfb, " ",
    fname, ".PDF", " ", fname, ".pdf")) == 0) %or% "pdf.embedfonts failed";
}
