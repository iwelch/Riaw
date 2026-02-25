## Setup: load all iaw functions into an iaw environment for testing

## If running as an installed package, iaw is already loaded — skip manual sourcing
if (isNamespaceLoaded("iaw")) return(invisible(NULL))

## Find library root: two levels up from tests/testthat/setup.R
libdir <- tryCatch(
    normalizePath(file.path(testthat::test_path(), "../..")),
    error = function(e) {
        ## Fallback: assume we're run from tests/ or project root
        for (candidate in c("../..", "..", ".")) {
            d <- normalizePath(candidate, mustWork = FALSE)
            if (file.exists(file.path(d, "Rprofile.R"))) return(d)
        }
        stop("Cannot find Riaw library root")
    }
)

## Create iaw environment in globalenv so source(local=FALSE) and tests see the same object
iaw <- new.env(parent = globalenv())
assign("iaw", iaw, envir = globalenv())

## Source global operators first (some library files use %and%, %or%)
for (opfile in c("%and%.R", "%or%.R", "%inrange%.R")) {
    fp <- file.path(libdir, opfile)
    if (file.exists(fp)) base::source(fp, local = FALSE)
}

## Provide global helpers that Rprofile.R normally defines
if (!exists("assert", envir = globalenv())) {
    assign("assert", function(cond, ...) {
        if (!cond) stop(paste(...), call. = FALSE)
    }, envir = globalenv())
}
if (!exists("msgboth", envir = globalenv())) {
    assign("msgboth", function(..., msgcat = "") message(...), envir = globalenv())
}
if (!exists("catln", envir = globalenv())) {
    assign("catln", function(...) cat(..., "\n"), envir = globalenv())
}

## Set options that library code expects (normally set by Rprofile.R)
options(os = switch(Sys.info()["sysname"],
                    "Darwin" = "macos", "Linux" = "linux", "Windows" = "windows",
                    tolower(Sys.info()["sysname"])))
options(mc.cores = max(1L, parallel::detectCores() - 2L, na.rm = TRUE))

## Source all library .R files
rfiles <- sort(Sys.glob(file.path(libdir, "*.R")))
rfiles <- rfiles[!grepl("Rprofile\\.R$", rfiles)]

for (rfile in rfiles) {
    tryCatch(
        base::source(rfile, local = FALSE),
        error = function(e) message("Setup: skipping ", basename(rfile), ": ", conditionMessage(e))
    )
}
