
#' ESTRING
#'
#' @name estring
#'
#' interpolated character strings
#'
#' @details this function itself should not use %or%, %and%, or anything else that uses an estring.
#'
#' @usage estring("this is variable v: {{v}}")
#'
#' @param es.  All expressions inside {{ }} will be evaluated and inserted in
#'
#' @return a plain string
#'


iaw$estring <- function(es) {

  rx <- "(?<=\\{\\{).*?(?=\\}\\})"

  match <- gregexpr(rx, es, perl=TRUE)  ## ex: es="hi {{x}} and {{x+1}}"
  toeval <- regmatches(es, match)[[1]] ## now we have syntax <- list( "x", "x+1" )

  subback <- rep(NA, length(toeval))
  if (length(toeval)==0) return( es );
  for (i in 1:length(toeval)) {
      for (en in sys.nframe():0) {
          subback[i] <- tryCatch( eval(parse(text=toeval[i]), envir=en), error=function(e) "ERR")
          if (is.na(subback[i])) subback[i]="NA";
          if (subback[i] != "ERR") break
      }
      subback[i]= paste(toeval[i], "=", subback[i], sep="")
  }
  regmatches(es,match) <- "%s"

  do.call("sprintf", (c(fmt=es, ...=as.list(subback), recursive=FALSE)))
}

################################################################################################################################
