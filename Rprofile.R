################################################################################
##  Rprofile.R - Personal R Configuration for Riaw
################################################################################


################################################################################
## Section 1: Set Options
################################################################################

Sys.setenv('R_MAX_VSIZE' = 64 * 1024 * 1024 * 1024)

Sys.setenv(C_INCLUDE_PATH = "/opt/X11/include")  ## Cairo Installation

options(hostname = as.character(Sys.info()["nodename"]))

message(libdir, "/Rprofile.R on ", getOption("hostname"))

options(os = switch(Sys.info()["sysname"],
    "Darwin"  = "macos",
    "Linux"   = "linux",
    "Windows" = "windows",
    tolower(sysname)  # fallback
))

options(scipen = 1000) ## Tilt away from scientific notation

options(digits = 4, max.print = 500)

options(stringsAsFactors = FALSE)

options(editor = "emacs")

options(keep.source = TRUE, keep.source.pkgs = TRUE)

options(show.error.locations = TRUE)


################################################################################
## Section 2: Assure Library Dependencies
################################################################################

addlibraries <- c("Cairo", "data.table", "R.utils", "inline", "knitr", "MASS",
                  "sandwich", "bit64", "glue", "RcppArmadillo", "dplyr")

## fails on intel, usually with ctest  "Rcpp", "fixest", "lmtest", "fst"

addlibraries.notinstalled <- (!addlibraries %in% .packages(TRUE))
if (any(addlibraries.notinstalled)) {
    cat(" install.packages(c(\"")
    cat(addlibraries[addlibraries.notinstalled], sep = "\",\"")
    cat("\"))\n")
    message("\n\nPlease download all from cran first. Missing: ")
    message(paste(addlibraries[addlibraries.notinstalled], collapse = ", "))
    stop(".Rprofile: FAIL - required packages not installed")
}


################################################################################
## Section 3: Load Libraries
################################################################################

## in case we later need other functions from a loaded library
useplus <- function(package, include.only) {
    loaded <- ls(sprintf("package:%s", package), all.names = TRUE)
    unloadNamespace(package)
    if (missing(include.only)) {
        use(package)
    } else {
        use(package, union(loaded, include.only))
    }
}

library("parallel")  ## we need the whole thing, mostly in iaw::mclapply, rbind.mc.by, ..options(mc.cores = max(1, detectCores() - 2))

library("knitr")

use("compiler", "cmpfun")

use("Cairo", c("CairoFonts", "CairoPDF"))  # NOTE: install fonts with `font_import(); loadfonts()`

use("data.table", c("fread", "fwrite", "first", "last", "nafill"))
options(datatable.fread.input.cmd.message = FALSE)

use("MASS")

# use("Rcpp"); use("inline")
# use("fst")
# use("sandwich")
# use("lmtest")

use("dplyr", "ntile")

### would be nice to be able to use "install.packages("devtools"); devtools::install_github("hadley/strict")



################################################################################
## Section 7: Universal Common Function
################################################################################

msgboth <- function(..., msgcat = "") {
    cat("\n----------------")
    message("\n\033[43m ==> ", msgcat, "\033[0m", appendLF = FALSE)
    cat("\n", ..., "\n")
}



################################################################################
## Section 7: TeX and Font Configuration
################################################################################

# Find most recent texlive installation
texlive_dirs <- list.files("/usr/local/texlive/", pattern = "^[0-9]{4}$", full.names = TRUE)
if (length(texlive_dirs) > 0) {
    texlive_year <- max(as.integer(basename(texlive_dirs)))
    options(texlive = paste0("/usr/local/texlive/", texlive_year, "/"))
    options(texfonts = paste0(getOption("texlive"), "/texmf-dist/fonts/"))
    options(pfbdir = paste0(getOption("texfonts"), "type1/public/bera/"))
} else {
    message("Warning: No texlive installation found in /usr/local/texlive/")
}


################################################################################
## Section 8: Parallel Processing
################################################################################

## This includes threads. The computer becomes unresponsive if we use more than these

################################################################################
## Section 9: Command Line Arguments
################################################################################

ARGV <- commandArgs(trailingOnly = TRUE)
ARGALL <- commandArgs()

################################################################################
## Section 10: Interactive vs Non-Interactive Setup
################################################################################

if (interactive()) {
    ## --- Interactive Mode ---

    Rscriptname <- "Rscriptname unknown (interactive)"
    utils::loadhistory("~/.Rhistory")

    ## Terminal width auto-adjustment
    tty.setcols <- function(rightmargin = 8) {
        cols <- system('tput cols', intern = TRUE)
        if (is.character(cols)) options(width = as.integer(cols) - rightmargin)  ## default
        message("[Col Widths of Terminal now ", getOption("width"), " characters]")
    }
    tty.setcols()

    ## Save history and clean up on exit
    .Last <- function() {
        if (interactive()) try(savehistory("~/.Rhistory"))
        try(system("rm -rf .RData ; echo '[removed any .RData]'"))
    }

} else {

    options(width = 1024)
    cat("[Non-Interactive: 1024 terminal widths]\n", file = stderr())

    use("tools", "md5sum")
    cat("#CommandArgs: ", paste(ARGALL, collapse = " "), "\n")

    if (grepl("--file=", ARGALL[4])) {
        Rfilename <- substr(ARGALL[4], 8, 100)
        md5sumval <- md5sum(Rfilename)
        Rscriptname <- paste("Script: ", R.home(), "//", Rfilename, " ", md5sumval, "\n")
        cat("#     ", Rscriptname)
        cat("#----------------\n")
        print(file.info(Rfilename))
        cat("#----------------\n")
    }
}

################################################################
## try to improve error handling
################################################################

if (requireNamespace("rlang", quietly = TRUE)) {
    library(rlang)
    options(error = rlang::entrace)
    options(rlang_backtrace_on_error = if (interactive()) "branch" else "full")

} else {

    if (interactive()) {
        options(error = function() {
            lineno <- function() {
                calls <- sys.calls()
                srcrefs <- sapply(calls, function(v) {
                    if (!is.null(srcref <- attr(v, "srcref"))) {
                        srcfile <- attr(srcref, "srcfile")
                        paste0(basename(srcfile$filename), "#", srcref[1L])
                    } else {
                        "."
                    }
                })
                cat("Current call stack locations:\n")
                cat(srcrefs, sep = " ")
                cat("\n")
            }

            lineno()
            message("\nRun traceback() for full stack trace")
            message("Tip: Install rlang for better error traces: install.packages('rlang')")
        })
    } else {
        options(error = traceback)
    }
}

################################################################################
## Section 11: Load Core Operators (needed before iaw environment)
################################################################################

if (!exists('%and%')) base::source(paste0(libdir, "/%and%.R"))
if (!exists('%or%'))  base::source(paste0(libdir, "/%or%.R"))
if (!exists('%inrange%')) base::source(paste0(libdir, "/%inrange%.R"))

################################################################################
## Section 12: iaw Environment - Loading & Compilation
################################################################################

fname.lib.cached <- paste0(libdir, "/library.Rdata")
iawRsourcefilenames <- c(Sys.glob(paste0(libdir, "/*.R")))

if (!exists("docs")) docs <- NULL

if (file.exists(fname.lib.cached) &&
    all(file.info(fname.lib.cached)$mtime > file.info(iawRsourcefilenames)$mtime)) {

    ## Load from cache if up to date
    load(fname.lib.cached)
    message("[loaded cached library ", fname.lib.cached, "]")

} else {
    ## Compile from source

    ## The functions can depend on one another, so first load all from source
    iaw <- new.env()
    for (Rfile in iawRsourcefilenames) {
        if (grepl("Rprofile.R", Rfile)) next  ## skip yourself!
        base::source(Rfile)
    }

    message("NOW COMPILING")

    for (Rfunc in ls(envir = iaw)) {
        if (is.function(iaw[[Rfunc]])) {
            cat("[Compiling '", Rfunc, "']\n", sep = "")
            iaw[[Rfunc]] <- cmpfun(iaw[[Rfunc]])
        }
    }

    save(iaw, file = fname.lib.cached)
    cat("[Compiled and Saved ", fname.lib.cached, "]\n\n", file = stderr())

    rm(Rfile, Rfunc)
}

rm(fname.lib.cached, iawRsourcefilenames)


################################################################################
## Section 13: iaw Environment - Helper Functions
################################################################################
#
# iaw$list <- function() {
#     fnames <- sort(ls(iaw))
#     cat("Available iaw functions (", length(fnames), " total):\n", sep = "")
#     print(fnames)
#     invisible(fnames)
# }
#
# iaw$help <- function(func) {
#     if (missing(func)) return(iaw$list())
#     fname <- deparse(substitute(func))
#     if (!exists(fname, envir = iaw)) {
#         message("Function '", fname, "' not found in iaw environment")
#         return(invisible(NULL))
#     }
#     f <- get(fname, envir = iaw)
#     if (is.function(f)) {
#         cat("Function: iaw$", fname, "\n", sep = "")
#         print(args(f))
#     } else {
#         cat("Object: iaw$", fname, " (not a function)\n", sep = "")
#         str(f)
#     }
# }
#
################################################################################
## Section 14: Base Function Enhancements
################################################################################

## --- Enhanced source() with automatic sink to .Rout in interactive mode

iaw$ARGV0 <- NULL
base_source <- base::source  # Keep base::source available for internal use

source <- function(file = file, verbose = FALSE, ...) {
    iaw$sink(paste0(file, "out"), split = TRUE, verbose)
    if (Rscriptname == "Rscriptname unknown") Rscriptname <<- paste(getwd(), file)
    iaw$ARGV0 <<- file
    try(base_source(file, keep.source = TRUE, ...))
    iaw$ARGV0 <<- NULL
    iaw$sink(NULL, verbose)
    if (verbose) iaw$msg("[automatic source sinking into ", file, "out was closed]")
}




################################################################
## --- Enhanced subset.data.frame with better error messages ---
################################################################

ns <- getNamespace("base")
unlockBinding("subset.data.frame", ns)
assign("subset.data.frame", iaw$subset.data.frame, envir = ns)
lockBinding("subset.data.frame", ns)



################################################################
## Protection
################################################################


## Protect against accidental misuse of T for "time-end" vs. TRUE
T <- TRUE
lockBinding('T', .GlobalEnv)

## Clean up commonly misused variables
rmf <- function(n) {
    if (exists(n, envir = .GlobalEnv)) rm(list = n, envir = .GlobalEnv)
}
suppressWarnings({ rmf("d"); rmf("lh"); rmf("lag"); rmf("rmf"); })


#################################################################
## Section 16: Cleanup & Final Message
#################################################################

message(libdir, "/Rprofile.R on ", getOption("hostname"), " completely loaded")




## Tips & Reminders:
## new : my_url = config$url %||% "https://www.jumpingrivers.com/blog"
## sort_by(dfx, list(var1, var2), ...)
## mtcars |> lm(mpg ~ disp, data = _) |> _$coef
## r"literal string \textsf{hi}"
## try last_error(); last_trace()
