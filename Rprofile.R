## new : my_url = config$url %||% "https://www.jumpingrivers.com/blog"
## sort_by(dfx, list(var1, var2), ...)
## mtcars |> lm(mpg ~ disp, data = _) |> _$coef
## r"literal string \textsf{hi}"

# try last_error(); last_trace()

T <- TRUE
lockBinding('T', .GlobalEnv)  ## protect the accidental misuse of T for "time-end" vs. TRUE

hostname <- function() as.character(Sys.info()["nodename"])

message(libdir, "/Rprofile.R on ", hostname())

options(show.error.locations = TRUE)

## Clean up commonly misused variables
rmf <- function(n) {
    if (exists(n, envir = .GlobalEnv)) rm(list = n, envir = .GlobalEnv)
}
suppressWarnings({ rmf("d"); rmf("lh") })

msgboth <- function(..., msgcat = "") {
    cat("\n----------------")
    message("\n\033[43m ==> ", msgcat, "\033[0m", appendLF = FALSE)
    cat("\n", ..., "\n")
}

################################################################
## base::source("~/lib/R/chatter.R")

addlibraries <- c("Cairo", "data.table", "R.utils", "inline", "knitr", "MASS", "sandwich", "bit64", "glue", "RcppArmadillo")
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

# message("[installing package libraries]")

useplus <- function(package, include.only) {
    loaded <- ls(sprintf("package:%s", package), all.names = TRUE)
    unloadNamespace(package)
    if (missing(include.only)) {
        use(package)
    } else {
        use(package, union(loaded, include.only))
    }
}

#message("trying R.utils")
# use("R.utils", c(".parseVersion"))
#message("done R.utils")

Sys.setenv(C_INCLUDE_PATH = "/opt/X11/include")  ## Cairo Installation

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




### would be nice to be able to use "install.packages("devtools"); devtools::install_github("hadley/strict")

message("[loaded all libraries]")

ARGV <- commandArgs(trailingOnly = TRUE)
ARGALL <- commandArgs()

if (!interactive()) {

    options(width = 1024)
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
    options(width = 1024)
    cat("[Non-Interactive: 1024 terminal widths]\n", file = stderr())

    if (requireNamespace("rlang", quietly = TRUE)) {
        library(rlang)
        options(
            error = rlang::entrace,
            rlang_backtrace_on_error = "full",
            show.error.locations = TRUE
        )
    } else {
        options(error = traceback)
    }

} else {

    Rscriptname <- "Rscriptname unknown (interactive)"
    utils::loadhistory("~/.Rhistory")

    tty.setcols <- iaw.autosetcols <- function( rightmargin= 8) {
        cols <- system('tput cols', intern = TRUE)
        if (is.character(cols)) options(width = as.integer(cols) - rightmargin)  ## default
        message("[iaw.autosetcols: Col Widths of Terminal now ", getOption("width"), " characters]\n")
    }

    iaw.autosetcols()

    .Last <- function() {
        if (interactive()) try(savehistory("~/.Rhistory"))
        try(system("rm -rf .RData ; echo '[removed any .RData]'"))
    }

    if (requireNamespace("rlang", quietly = TRUE)) {
        library(rlang)
        options(
            error = rlang::entrace,
            rlang_backtrace_on_error = "branch",
            show.error.locations = TRUE
        )
    } else {
        options(error = function() {
            lineno()
            message("\nRun traceback() for full stack trace")
            message("Tip: Install rlang for better error traces: install.packages('rlang')")
        })
    }
}

# Keep source references
options(keep.source = TRUE, keep.source.pkgs = TRUE)

################################################################
### set global options
################################################################

Sys.setenv('R_MAX_VSIZE' = 64 * 1024 * 1024 * 1024)

options(hostname = system("hostname", intern = TRUE))
options(uname = system("uname", intern = TRUE))
options(os = if (getOption("uname") == "Darwin") "osx" else "linux")
if ((getOption("os") != "osx") & (getOption("os") != "linux")) {
    cat("no 'os' option defined.", file = stderr())
}

## tilt away from scientific notation
options(scipen = 1000, digits = 4, max.print = 500, stringsAsFactors = FALSE, editor = "emacs")

################################################################
### TeX and Font Related
################################################################

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

### note: you can also use the Cairo package for charter:
### https://stat.ethz.ch/pipermail/r-help/2011-August/286508.html

## library(grDevices)  ## "graphics devices and support for colours and fonts"

## must have: brew tap homebrew/cask-fonts; brew cask install font-bitstream-vera;
## must have: install.packages("extrafont"); library(extrafont); font_import(); loadfonts()
## library(extrafont)

# pdfFonts(Bera = Type1Font("Bera", paste0(getOption("texfonts"), "afm/public/bera/",
#          c("fvsr8a","fvsb8a","fvsro8a","fvsbo8a"), ".afm")))

## if (getOption("os") == "osx") pdf <- CairoPDF
## <-- now use 'cairo_pdf' as a device, gets you better fonts, but may drop vector graphics for bitmaps

################################################################

## library(bit64)
library("parallel")  ## we need the whole thing, mostly in iaw::mclapply, rbind.mc.by, ...
## this includes threads. the computer becomes unresponsive if we use more than these
options(mc.cores = max(1, detectCores() - 2))

################################################################
## now precompile all the ~/src/R/*.R files into library.Rdata

## too difficult to negotiate file system, so let's just load it this way first, uncompiled, for all use
if (!exists('%and%')) base::source(paste0(libdir, "/%and%.R"))
if (!exists('%or%'))  base::source(paste0(libdir, "/%or%.R"))

if (!exists('%inrange%')) '%inrange%' <- function(x, range_vector) ( (x >= range_vector[1]) & x <= (range_vector[2]) )


## library(compiler)

fname.lib.cached <- paste0(libdir, "/library.Rdata")
iawRsourcefilenames <- c(Sys.glob(paste0(libdir, "/*.R")),
                         Sys.glob(paste0(libdir, "/plotsupport/*.R")))

if (!exists("docs")) docs <- NULL

if (file.exists(fname.lib.cached) &&
    all(file.info(fname.lib.cached)$mtime > file.info(iawRsourcefilenames)$mtime)) {

    load(fname.lib.cached)
    cat("Loaded cached library ", fname.lib.cached, "\n", file = stderr())

} else {

    ## libraries needed to be able to compile
    # library(utils)
    # library(parallel)
    # library(stats)
    # library(graphics)
    # library(grDevices)

    ## the functions can depend on one another, so first load all from source into the current environment
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

    # attr(iaw, "Rfiles") <- iawRsourcefilenames  ## in addition to containing the files, we also have a list of source files
    # attr(iaw, "libloc") <- fname.lib.cached
    ## quit()

    rm(Rfile, Rfunc)
}

rm(fname.lib.cached, iawRsourcefilenames, rmf)

################################################################
## redefine source to remember what .R file was last loaded. can be used in graphics

iaw$ARGV0 <- NULL

# Keep base::source available for internal use
base_source <- base::source

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

################################################################

## doBy package for category based stats. aggregate
## auto width adjustment every time a command is executed: look into 'addTaskCallback(.adjustWidth)' and define a functio by this name
## require(bit64)
## cat(iaw$debug.advice)

################################################################################

# base.ignorena <- function() {
# cat("[redefining base statistics functions to ignore NA]\n", file=stderr())
# sum <- function(x, ..., na.rm = TRUE) { base::sum(x, ..., na.rm = na.rm) }
# mean <- function(x, ..., na.rm = TRUE) { base::mean(x, ..., na.rm = na.rm) }
# median <- function(x, ..., na.rm = TRUE) { base::median(x, ..., na.rm = na.rm) }
# min <- function(x, ..., na.rm = TRUE) { base::min(x, ..., na.rm = na.rm) }
# max <- function(x, ..., na.rm = TRUE) { base::max(x, ..., na.rm = na.rm) }
# range <- function(x, ..., na.rm = TRUE) { base::range(x, ..., na.rm = na.rm) }
# skew <- function(x, ..., na.rm = TRUE) { base::skew(x, ..., na.rm = na.rm) }
# kurtosis <- function(x, ..., na.rm = TRUE) { base::kurtosis(x, ..., na.rm = na.rm) }
# n <- function(x, ..., na.rm = TRUE) { if (na.rm) sum(!is.na(x)) else length(x) }
# orig.sd <- sd; sd <- function(x, ..., na.rm = TRUE) { orig.sd(x, ..., na.rm = na.rm) }
# orig.var <- var; var <- function(x, ..., na.rm = TRUE) { orig.var(x, ..., na.rm = na.rm) }
# cat("[redefined base statistics functions to ignore NA]\n", file=stderr())
# }
# base.ignorena()

################################################################

if (exists("lag")) lag <- iaw$lagseries

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

message(libdir, "/Rprofile.R on ", hostname(), " completely loaded")
