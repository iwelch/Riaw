
#' LOCK
#'
#' @name lock.acquire, lock.release
#'
#' write a lock file (for concurrent print output in mc)
#'
#' @usage lock.acquire(); lock.release()
#'
#' @return
#'


iaw$lock.name <- "R.lock"

iaw$lock.acquire <- function(textarg="unnamed") {
    starttime <- Sys.time()
    while (file.exists(iaw$lock.name)) {
        if (Sys.time() - starttime > 10) {
            ## read who locks it and display it
            cat("[locked by", readChar(iaw$lock.name, file.info(iaw$lock.name)$size),"]", file=stderr())
            starttime <- Sys.time()
        }
        Sys.sleep( runif(1) )
    }
    cat( textarg, " ", Sys.time(), "
", file=iaw$lock.name)
}

iaw$lock.release <- function() {
    unlink( iaw$lock.name )
}

on.exit( { iaw$lock.release() } )
