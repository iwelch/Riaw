iaw$mcsapply <- function(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE, 
                     mc.cores = getOption("mc.cores", 2L)) {
  # Apply mclapply to get the results
  result <- mclapply(X, FUN, ..., mc.cores = mc.cores)
  
  # If simplify is FALSE, return as-is (like mclapply)
  if (!simplify) {
    return(result)
  }
  
  # Simplify the result (similar to sapply logic)
  if (length(result) == 0L) {
    return(list())
  }
  
  # Check if all elements have the same length and structure
  lens <- vapply(result, length, integer(1))
  
  if (all(lens == 1L)) {
    # All results are length 1, try to simplify to vector
    result <- unlist(result, recursive = FALSE, use.names = USE.NAMES)
  } else if (all(lens == lens[1L]) && lens[1L] > 0L) {
    # All results have same length > 1, try to simplify to matrix
    result <- tryCatch({
      matrix(unlist(result), nrow = lens[1L], 
             dimnames = if (USE.NAMES) list(names(result[[1L]]), names(result)) else NULL)
    }, error = function(e) result)
  }
  
  result
}
