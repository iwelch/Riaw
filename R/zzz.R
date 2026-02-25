# Package-level binding for the iaw environment
iaw <- NULL

# Placeholder for S3 method -- .onLoad replaces this with the real iaw$print.olm
print.olm <- function(x, ...) invisible(x)

# Helpers that library files expect to exist at source time
# (defined here to avoid R CMD check NOTE about startup messages in .onLoad)
.iaw_assert <- function(cond, ...) {
    if (!cond) stop(paste(...), call. = FALSE)
}
.iaw_msgboth <- function(..., msgcat = "") message(...)
.iaw_catln <- function(...) cat(..., "\n")

.onLoad <- function(libname, pkgname) {

    ## 1. Create the iaw environment (visible as iaw::iaw after export)
    iaw <<- new.env(parent = baseenv())

    ## 2. Get the package namespace (where source(local=ns) will place iaw$foo definitions)
    ns <- getNamespace(pkgname)

    ## 3. Install helpers into namespace so sourced files can find them
    assign("assert", .iaw_assert, envir = ns)
    assign("msgboth", .iaw_msgboth, envir = ns)
    assign("catln", .iaw_catln, envir = ns)

    ## 4. Set options that library code expects
    if (is.null(getOption("os"))) {
        options(os = switch(Sys.info()["sysname"],
                            "Darwin"  = "macos",
                            "Linux"   = "linux",
                            "Windows" = "windows",
                            tolower(Sys.info()["sysname"])))
    }
    if (is.null(getOption("mc.cores"))) {
        options(mc.cores = max(1L, parallel::detectCores() - 2L, na.rm = TRUE))
    }

    ## 5. Find the installed library files
    libdir <- system.file("lib", package = pkgname)
    if (!nzchar(libdir) || !dir.exists(libdir)) {
        warning("iaw: inst/lib not found -- package may not have been built with 'make build'")
        return(invisible(NULL))
    }

    ## 6. Source operators first (other files may depend on %and%, %or%, %inrange%)
    for (opfile in c("%and%.R", "%or%.R", "%inrange%.R")) {
        fp <- file.path(libdir, opfile)
        if (file.exists(fp)) source(fp, local = ns)
    }

    ## 7. Source all remaining .R files into the namespace
    rfiles <- sort(list.files(libdir, pattern = "\\.R$", full.names = TRUE))
    rfiles <- rfiles[!basename(rfiles) %in% c("Rprofile.R", "%and%.R", "%or%.R", "%inrange%.R")]

    for (rfile in rfiles) {
        tryCatch(
            source(rfile, local = ns),
            error = function(e) {
                warning("iaw: failed to source ", basename(rfile), ": ",
                        conditionMessage(e), call. = FALSE)
            }
        )
    }

    ## 8. Register print.olm for S3 dispatch in the namespace
    if (exists("print.olm", envir = iaw)) {
        registerS3method("print", "olm", iaw$print.olm, envir = ns)
    }

    invisible(NULL)
}
