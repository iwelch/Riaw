
#' BISECTION
#'
#' @name bisection
#'
#' a vectorized bisectioning root-finder
#'
#' @usage bisection(f, lower, upper, ..., numiter = 100, tolerance = .Machine$double.eps^0.25)
#'
#' @param f: a function, such as the Black-Scholes function in sigma
#' @param lower: the minimum value of the function (e.g., 0)
#' @param upper: the upper value of the function (e.g., 100 for 10,000%)
#' @param ...: other arguments to f
#' @param numiter: the maximum number of iterations
#' @param tolerance: the stopping point
#'
#'  @return
#'
#'  @examples
#' 	  f <- function(x,s) (x-s^2)  ## the solution to each formula is s^2
#' 	  bisection( f, lower=c(-99,-99), upper=c(99,99), s=c(2,3), numiter=50 )
#'
#' 	  ## a strange way to find the root in two dimensions, x and z. first, take z as exogenous
#' 	  f <- function(x,s,z) (x-z -s^2)
#' 	  b <- bisection(f, rep(-100,11), rep(100,11), z=seq(-5,5), s=seq(-5,5))
#' 	  ## and see where we are changing sign
#' 	  b$mid[(sign(b$mid) != lagseries(sign(b$mid)) | (sign(b$mid) != leadseries(sign(b$mid))))]
#'


iaw$bisection <- function (f, lower, upper, ..., numiter = 100, tolerance = .Machine$double.eps^0.25, verbose= 0)
{
  if (!is.null(getOption("strict"))) {
    (is.function(f)) %or% "f is not a function"

    (is.null(lower)) %and% "lower is null"
    (is.vector(lower, mode="numeric")) %or% "Your lower limit is not a numeric vector, but a {{class(lower)}}."
    (length(lower) > 0) %or% "Need some observations than in lower, not 0"

    (is.null(upper)) %and% "upper is null"
    (is.vector(upper, mode="numeric")) %or% "Your upper limit is not a numeric vector, but a {{class(upper)}}."
    (length(upper) > 0) %or% "Need some observations than in upper, not 0"

    (length(upper) == length(lower)) %or% "length of lower {{length(lower)}} must be equal to length of upper {{length(upper)}}"

    (is.vector(numiter, mode="numeric")) %or% "numiter must be a single integer"
    (iaw$is.numeric(numiter,1)) %or% "numiter must be a single integer"
  }

  flower <- f(lower, ...)
  (all(is.numeric(flower))) %or% "lower result to f(), i.e., f(lower), is not numeric"
  (all(!is.na(flower))) %or% "Some lower result to f(), i.e., f(lower) is NA"

  fupper <- f(upper, ...)
  (all(is.numeric(fupper))) %or% "upper result to f() is not numeric"
  (all(!is.na(flower))) %or% "some upper result to f() is NA"

  problem.cases.head <- head(which((fupper*flower)>0))
  (all(fupper*flower <= 0)) %or% "{{sum(fupper*flower>0)}}/{{length(flower)}} cases are impossible\n\t {{problem.cases.head}} : f(upper)={{fupper[problem.cases.head]}}  and f(lower)={{flower[problem.cases.head]}} have the same sign.\n"


  ## now do the actual root-finding
  for (n in 1:numiter) {
    mid <- (lower + upper)/2
    fmid <- f(mid, ...)

    if (verbose) cat("[bisection:",n,"/",numiter,"=", mean(abs(fmid), na.rm=TRUE), " and ", max(abs(fmid), na.rm=TRUE), "]\n", file=stderr())
    if (all(abs(fmid) < tolerance)) break
    samesign.mid.lower <- ((fmid < 0) & (flower < 0)) | ((fmid >= 0) & (flower >= 0))
    lower <- ifelse(samesign.mid.lower, mid, lower)
    flower <- ifelse(samesign.mid.lower, fmid, flower)
    upper <- ifelse(!samesign.mid.lower, mid, upper)
    fupper <- ifelse(!samesign.mid.lower, fmid, fupper)
  }
  return(list(mid = mid, fmid = fmid, lower = lower, upper = upper,
              flower = flower, fupper = fupper, n = n))
}
