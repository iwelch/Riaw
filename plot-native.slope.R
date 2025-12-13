preamble <- c(doc= '
@TITLE native.slope
@AUTHOR ivo.welch@gmail.com
@DATE 2013
@DESCRIPTION
@USAGE native.slope (x, y, where.i, debug = 0) 
@ARGUMENTS
@DETAILS
@SEEALSO
@EXAMPLES
', test= '
', changes= '
')

#if (!exists("libdir")) libdir <- "~/src/R/"

if (!exists('%and%')) source( paste0(libdir, "/%and%.R") )
if (!exists('%or%'))  source( paste0(libdir, "/%or%.R") )

## returns the srt in terms from axis location coordinate of x/y vector
iaw$native.slope <- function (x, y, where.i, debug = 0) {

    (length(x) == length(y)) %or% "x and y must have equal dimensions, not {{length(x)}} and {{length(y)}}"

    ## do not use this on the edge of the graph, dummy!

    (where.i > 1) %or% "The where.i of {{where.i}} should not be on the left edge."
    (where.i < length(x)) %or% "The where.i of {{where.i}} should not be on the right edge (vector length {{length(x)}})."

    l0 <- where.i - 1
    l1 <- where.i + 1

    left <- x[l0];  (!is.na(left)) %or% "Sorry, but left x[l0={{l0}}]=NA\n"; 
    right <- x[l1]; (!is.na(right)) %or% "Sorry, but right x[l0={{l0}}]=NA\n"; 
    (left != right) %or% "Sorry, but {{left}} is the same as {{right}}\n";

    dn <- y[l0];  (!is.na(dn)) %or% "Sorry, but dn y[l0={{l0}}]=NA\n"; 
    up <- y[l1];    (!is.na(up)) %or% "Sorry, but up y[l1={{l1}}]=NA\n"; 

    iaw$native.slope.make.angle( left, right, dn, up, debug )
}

iaw$native.slope.make.angle <- function( left, right, dn, up, debug=FALSE ) {

    .usr <- par("usr")  # usr coordinates
    .plt <- par("plt")  # plot region as fraction of current figure region

    if (par("xlog")) { stop("logs not yet working"); left <- log( left ); right <- log(right);  }
    if (par("ylog")) { stop("logs not yet working"); up <- log(up); dn <- log(dn) }

    geoslope <- (up - dn) / (right - left)
    (is.na(geoslope)) %and% return(0)  ## should never trigger

    viewxform <- (.usr[4] - .usr[3]) / (.usr[2] - .usr[1]) *
        (.plt[2] - .plt[1]) / (.plt[4] - .plt[3])
    (is.na(viewxform)) %and% "I do not have sensible axis dimensions ({{viewxform}})"

    .fin <- par("fin")  # region dimensions in inches
    asp.ratio <- .fin[1] / .fin[2]
    (is.na(asp.ratio)) %and% "I do not have a reasonable drawing aspect ratio {{asp.ratio}}"

    net.slope <- geoslope / asp.ratio / viewxform

    ## back to angular degrees
    slope <- atan(net.slope) / pi * 180

    if (debug) {
        print(sys.call())
        cat("\t", "debug: geoslope=", geoslope, " (LeftBot=(x=", left, ",y=",dn,").  Topright=(x=", right, ",y=",up,")\n")
        cat("\t\tviewxform (rescaler)=", viewxform, " (usr=", .usr, ", plt=", .plt, ")\n");
        cat("\t\tasp.ratio=", .fin, "\n\t\t==> net slope=", net.slope, "=", slope, "deg\n")
        points(left, dn, pch = 19, col="red")
        points(right, up, pch = 19, col="red")
        text( (left+right)/2, (up+dn)/2, "iaw$native.slope debug", col="red", srt= slope, cex=0.5)
    }
    return(slope = slope)
}
