


################################################################
## rarely used

iaw$leadlagpanel <- function(d, num, firmid, timeid) {
    is.data.frame(d) %or% "leadlagpanel: first arg must be data frame"  ## should also work with matrix, but has not been tested
    (nrow(d)>1) %or% "makes no sense to leadlag panel of 1 row"
    ((is.numeric(num)) & (length(num)==1)) %or% "leadlagpanel: second arg must be single scalar integer, not {{iaw$whatis(num)}}"

    firmidvec <- if ((is.character(firmid)) & length(firmid)==1) d[[firmid]] else firmid
    timeidvec <- if ((is.character(timeid)) & length(timeid)==1) d[[timeid]] else timeid

    ((is.vector(firmidvec)) & (length(firmidvec)==nrow(d))) %or% "leadlagpanel: firmid must be length of data panel {{nrow(d)}}, not {{length(firmidvec)}}"
    ((is.vector(timeidvec)) & (length(timeidvec)==nrow(d))) %or% "leadlagpanel: timeid must be length of data panel {{nrow(d)}}, not {{length(timeidvec)}}"

    x <- NULL
    pnm <- if (num<0) ".lead" else ".lag"
    if (abs(num)>1) pnm <- paste0(pnm, abs(num))

    for (nmi in 1:ncol(d)) {
        nm <- names(d)[nmi]
        x <- cbind(x, iaw$lagseries(d[[nm]], num, firmidvec, timeidvec ))
        colnames(x)[ncol(x)] <- paste0(nm, pnm)
    }
    x
}

iaw$leadpanel <- function(d, numleads, firmid, timeid) iaw$leadlagpanel(d, -numleads, firmid, timeid)
iaw$lagpanel <- function(d, numlags, firmid, timeid) iaw$leadlagpanel(d, numlags, firmid, timeid)
