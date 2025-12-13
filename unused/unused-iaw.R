useplus <- function(package, include.only) {
    loaded <- ls(sprintf("package:%s", package), all.names = TRUE)
    unloadNamespace(package)
    if (missing(include.only)) {
        use(package)
    } else {
        use(package, union(loaded, include.only))
    }
}



## new : my_url = config$url %||% "https://www.jumpingrivers.com/blog"
## sort_by(dfx, list(var1, var2), ...)
## mtcars |> lm(mpg ~ disp, data = _) |> _$coef
## r"literal string \textsf{hi}"

# try last_error(); last_trace()



################################################################

## Add helper functions for iaw environment
iaw$list <- function() {
    fnames <- sort(ls(iaw))
    cat("Available iaw functions (", length(fnames), " total):\n", sep = "")
    print(fnames)
    invisible(fnames)
}

iaw$help <- function(func) {
    if (missing(func)) return(iaw$list())
    fname <- deparse(substitute(func))
    if (!exists(fname, envir = iaw)) {
        message("Function '", fname, "' not found in iaw environment")
        return(invisible(NULL))
    }
    f <- get(fname, envir = iaw)
    if (is.function(f)) {
        cat("Function: iaw$", fname, "\n", sep = "")
        print(args(f))
    } else {
        cat("Object: iaw$", fname, " (not a function)\n", sep = "")
        str(f)
    }
}
