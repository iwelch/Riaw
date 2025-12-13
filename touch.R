
#' Touch a File
#'
#' @name touch
#'
#'   creates a file that contains basic information about the process which created it.
#'
#' @usage touch( filename )
#'
#' @param filename the filename to create or touch
#'
#' @examples touch( "reserveme.csv.gz" )
#'

iaw$touch <- function(filename) if (!file.exists(filename)) cat( Sys.info(), " ", Sys.time(), file=filename) else system2(paste("touch", filename))  ## use "-c filename" to create the file, too
