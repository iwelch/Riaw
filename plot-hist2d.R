preamble <- c(doc= '
@TITLE hist2d
@AUTHOR ivo.welch@gmail.com
@DATE 2013
@DESCRIPTION
@USAGE hist2d (x, y, ngroups = 20, bandwidth = 0.2) 
@ARGUMENTS
@DETAILS
@SEEALSO
@EXAMPLES
', test= '
', changes= '
')

iaw$hist2d <- function (x, y, ngroups = 20, bandwidth = 0.2) 
{
  library("KernSmooth")
  b2d <- KernSmooth::bkde2D(iaw$completeobs(cbind(x, y)),
                gridsize = c(ngroups, ngroups), bandwidth = bandwidth)
  names(b2d) <- c("x", "y", "z")
  return(b2d)
}
