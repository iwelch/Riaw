
#' Print OLM Results
#'
#' @name print.olm
#'
#' S3 print method for olm objects. Provides compact output.
#'
#' @param x An olm object.
#' @param yname Label for dependent variable.
#' @param xnames Labels for independent variables.
#' @param wantedcols Columns to display (default 1:7).
#' @param description Model description.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns the olm object.
#'
#' @family regression
#' @export

iaw$print.olm <- function(x, yname = NULL, xnames = NULL,
                      wantedcols = 1:7, description = NULL, ...) {

    ## Limit wantedcols to available columns
    wantedcols <- wantedcols[wantedcols <= ncol(coef(x))]
    ccc <- coef(x)[, wantedcols, drop = FALSE]
    csigma <- x$sigma

    if (!is.null(xnames)) {
        if (length(xnames) == nrow(ccc)) {
            rownames(ccc) <- xnames
        } else if (length(xnames) == nrow(ccc) - 1) {
            rownames(ccc) <- c("(Intercept)", xnames)
        } else {
            stop("xnames length mismatch")
        }
    }

    if (!is.null(description)) cat("---- Model:", description, "\n")
    if (is.null(yname)) yname <- as.character(formula(x))[2]
    cat("--- Explaining '", yname, "':\n", sep = "")
    print(ccc)
    cat("--- R2=", round(x$r.squared, 4),
        " adj.R2=", round(x$adj.r.squared, 4),
        " df=", x$df[2],
        " N=", x$df[2] + x$df[1],
        " sigma=", csigma, "\n", sep = "")

    ## F-statistic
    if (!is.null(x$fstatistic)) {
        fstat <- x$fstatistic
        pval <- pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
        cat("--- F=", round(fstat[1], 2),
            " on ", fstat[2], " and ", fstat[3], " DF",
            "  p=", format.pval(pval, digits = 3), "\n\n", sep = "")
    } else {
        cat("\n")
    }

    invisible(x)
}

print.olm <- iaw$print.olm
## If using an iaw environment, also store a reference there:
## iaw$print.olm <- print.olm
