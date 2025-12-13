
#' WHAT IS OBJECT?
#'
#' @name whatis( sqrt )
#'       it is  function  --- use unlist() or str() to determine contents[1] "function"
#'
#'   describe what some arbitrary R object is
#'
#'  @usage whatis (object)
#'
#'  @param object
#'
#'  @return a text string that describes the object
#'
#' @examples
#'    iaw$whatis( sqrt )
#'       it is  function  --- use unlist() or str() to determine contents[1] "function"



iaw$whatis <- function (object) {
  if (is.null(object)) return("is NULL")
  else if (is.matrix(object))
    return(paste(class(object), "with", ncol(object), "cols and", nrow(object), "rows"))
  else if (is.data.frame(object))
    return(paste(class(object), "with ", ncol(object), "variables and ", nrow(object), "observations"))
  else if (is.vector(object))
    return(paste(class(object), "with length", length(object)))
  else if (is.factor(object))
    return(paste(class(object), "with unique keys", length(unique(object))))
  else cat("it is ", class(object), " --- use unlist() or str() to determine contents")
  return(class(object))
}
