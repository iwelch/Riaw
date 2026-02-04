################################################################################
##
##  Rprofile.R - Initialization and Configuration for Riaw
##
################################################################################


################################################################################
## Section 1: Set System Options
################################################################################

Sys.setenv('R_MAX_VSIZE' = 64 * 1024 * 1024 * 1024)

Sys.setenv(C_INCLUDE_PATH = "/opt/X11/include")  ## Cairo Installation

options(

    hostname = as.character(Sys.info()["nodename"]),

    os = switch(Sys.info()["sysname"],
                "Darwin"  = "macos",
                "Linux"   = "linux",
                "Windows" = "windows",
                tolower(sysname)
                ),  # fallback


    scipen = 1000, ## Tilt away from scientific notation

    digits = 4,

    max.print = 500,

    stringsAsFactors = FALSE,

    editor = "emacs",

    keep.source = TRUE, keep.source.pkgs = TRUE,

    show.error.locations = TRUE,

    Rscriptname = NULL

)

message(libdir, "/Rprofile.R on ", getOption("hostname"))



################################################################################
## Section 2: Assure Library Dependencies
################################################################################

addlibraries <- c("Cairo", "data.table", "R.utils", "inline", "knitr", "MASS",
                  "sandwich", "bit64", "glue", "RcppArmadillo", "dplyr")

addlibraries <- c(addlibraries, "fst", "fixest", "lmtest", "Rcpp")  ## maybe fails

addlibraries.notinstalled <- (!addlibraries %in% .packages(TRUE))

if (any(addlibraries.notinstalled)) {
    message("\n\nPlease download all from cran first. Missing: ")
    message(paste(addlibraries[addlibraries.notinstalled], collapse = ", "))
    stop(".Rprofile: FAIL - required packages not installed")
}


################################################################################
## Section 3: Load Libraries
################################################################################

library("parallel")  ## we need the whole thing, mostly in iaw$mclapply, iaw$rbind.mc.by, ...
options(mc.cores = max(1, detectCores() - 2))

library("knitr")

use("compiler", "cmpfun")

use("Cairo", c("CairoFonts", "CairoPDF"))  # NOTE: install fonts with `font_import(); loadfonts()`

# library(data.table)
use("data.table", c("fread", "fwrite", "first", "last", "nafill", "rbindlist"))
options(datatable.fread.input.cmd.message = FALSE)

use("MASS")

use("dplyr", "ntile")

use("tools", "md5sum")

## use 'useplus' to add further



# use("Rcpp");
# use("inline")
# use("fst")
# use("sandwich")
# use("lmtest")

### would be nice to be able to use "install.packages("devtools"); devtools::install_github("hadley/strict")



################################################################################
## Section 4: Universal Common Function
################################################################################

assert <- function(cond, ...) {
  if (!cond) {
    cond_txt <- paste(deparse(substitute(cond)), collapse = " ")
    calls <- sys.calls()

    # Chase a symbol backward through call arguments
    chase <- function(sym) {
      for (cl in rev(calls)) {
        nms <- names(cl)
        if (!is.null(nms) && sym %in% nms && is.name(cl[[sym]]))
          sym <- as.character(cl[[sym]])
      }
      sym
    }

    # Find FUN arg in any apply-like call
    caller <- "unknown"
    apply_fns <- c("ave", "lapply", "sapply", "vapply", "mapply", "tapply",
                   "by", "aggregate", "apply", "Map", "mclapply", "parLapply")
    for (cl in calls) {
      fn <- as.character(cl[[1]])[1]
      if (fn %in% apply_fns && "FUN" %in% names(cl) && is.name(cl$FUN)) {
        caller <- chase(as.character(cl$FUN))
        break
      }
    }

    env <- parent.frame()
    prefix <- paste0("in ", caller, "() on condition (", cond_txt, "): ")
    stop(paste0(prefix, glue::glue(..., .envir = env)), call. = FALSE)
  }
}

################

msgboth <- function(..., msgcat = "") {
    cat("\n----------------")
    message("\n\033[43m ==> ", msgcat, "\033[0m", appendLF = FALSE)
    cat("\n", ..., "\n")
}

catln <- function(...) cat(..., "\n")

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


object.size <- function( x ) {
    format(utils::object.size(x), units="auto")
}

ARGV <- commandArgs(trailingOnly = TRUE)

## Detect if running under quarto (or similar tools that need pristine base::source)
.running_under_quarto <- any(grepl("quarto|knitr\\.R$", commandArgs()))

## Custom source() with auto-sinking - interactive or CLI, but not quarto
if (interactive() || !.running_under_quarto) {
    source <- function(file, ...) {
        if (grepl("\\.Rinclude$", file)) return(base::source(file, ...))

        ## Pass through if file doesn't exist (let base::source handle resolution)
        if (!file.exists(file)) {
            return(base::source(file, ...))
        }

        ## Normalize to absolute path for reliable checks
        file_abs <- normalizePath(file, mustWork = FALSE)

        ## Pass through to base::source for system directories
        if (grepl("^/(Applications|Library|usr|opt)/", file_abs)) {
            return(base::source(file, ...))
        }

        ## Also pass through if output directory is not writable
        outdir <- dirname(file_abs)
        if (!file.access(outdir, 2) == 0) {
            return(base::source(file, ...))
        }

        stopifnot(grepl("\\.R$", file))

        ## Detect nested sinking - check if already sinking
        if (sink.number() > 0) {
            stop("Sinking R source files cannot be nested. Use 'base::source(\"", file, "\")' instead.")
        }

        Routfilename <- paste0(file, "out")
        ## cannot be called before iaw$ has been created and read
        iaw$sink(Routfilename, split = TRUE)

        options(Rscriptname = normalizePath(file, mustWork = FALSE))  ## so programs can access it
        try(base::source(file, keep.source = TRUE, ...))
        ## Note: Rscriptname option persists so functions can access it

        iaw$sink(NULL)

        ## Remove empty output files (<=1 byte) - we don't need them
        if (file.exists(Routfilename) && file.info(Routfilename)$size <= 1) {
            file.remove(Routfilename)
        }
    }
}
rm(.running_under_quarto)

# if invoked via CMD BATCH:
# args <- commandArgs(trailingOnly = FALSE)
# script_path <- sub("--file=", "", args[grep("--file=", args)])
# If you just want the filename without the path:
# script_name <- basename(script_path)


if (!exists('%and%')) base::source(paste0(libdir, "/%and%.R"))
if (!exists('%or%'))  base::source(paste0(libdir, "/%or%.R"))
if (!exists('%inrange%')) base::source(paste0(libdir, "/%inrange%.R"))


################################################################################
## Section 5: TeX and Font Configuration
################################################################################

# Find most recent texlive installation
texlive_dirs <- list.files("/usr/local/texlive/", pattern = "^[0-9]{4}$", full.names = TRUE)
if (length(texlive_dirs) > 0) {
    texlive_year <- max(as.integer(basename(texlive_dirs)))
    options(texlive = paste0("/usr/local/texlive/", texlive_year, "/"))
    options(texfonts = paste0(getOption("texlive"), "/texmf-dist/fonts/"))
    options(pfbdir = paste0(getOption("texfonts"), "type1/public/bera/"))
} else {
    message("Warning: No texlive installation found in /usr/local/texlive/ --- plotting will suck")
}



################################################################################
## Section 6: Interactive vs Non-Interactive Setup
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
    message("[Non-Interactive: 1024 terminal widths]\n", file = stderr())

    ARGALL <- commandArgs()
    message("#CommandArgs: ", paste(ARGALL, collapse = " "), "\n")

    if (grepl("--file=", ARGALL[4])) {
        Rfilename <- substr(ARGALL[4], 8, 100)
        md5sumval <- md5sum(Rfilename)
        options(Rscriptname = normalizePath(Rfilename, mustWork = FALSE))
        Rscriptname <- paste("Script: ", R.home(), "//", Rfilename, " ", md5sumval, "\n")
        message("#     ", Rscriptname, "#----------------")
        print(file.info(Rfilename), file=stderr())
        message("#----------------\n")
    }
}


################################################################
## Section 7: try to improve error handling (depends on interactive)
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
                message("Current call stack locations:")
                message(srcrefs, sep = " ")
            }

            lineno()
            message("\nRun traceback() for full stack trace")
            message("Tip: Install rlang for better error traces: install.packages('rlang')")
            browser()
        })
    } else {
        options(error = traceback)
    }
}


################################################################################
## Section 8: Rebuild Riaw - Loading & Compilation of 'iaw$'
################################################################################

fname.lib.cached <- paste0(libdir, "/library.Rdata")
fname.rprofile <- paste0(libdir, "/Rprofile.R")
iawRsourcefilenames <- c(Sys.glob(paste0(libdir, "/*.R")))

cache.mtime <- file.info(fname.lib.cached)$mtime
rprofile.mtime <- file.info(fname.rprofile)$mtime
sources.mtime <- file.info(iawRsourcefilenames)$mtime

if (file.exists(fname.lib.cached) &&
    all(cache.mtime > sources.mtime) &&
    cache.mtime > rprofile.mtime) {

    ## Load from cache if up to date
    load(fname.lib.cached)   ## populates list iaw$
    message("[loaded cached library ", fname.lib.cached, "]")

} else {
    ## Recompile all the .R functions from source and store them"

    ## The functions can depend on one another, so first load all from source

    iaw <- new.env()

    for (Rfile in iawRsourcefilenames) {
        if (grepl("Rprofile.R", Rfile)) next  ## skip yourself!
        base::source(Rfile)  ## depends on iaw as list existing
    }

    message("[Compiling:", appendLF = FALSE)
    for (Rfunc in ls(envir = iaw)) {
        if (is.function(iaw[[Rfunc]])) {
            message(" '", Rfunc, "'", appendLF = FALSE)
            iaw[[Rfunc]] <- cmpfun(iaw[[Rfunc]])
        }
    }
    message("]")

    save(iaw, file = fname.lib.cached)
    message("[Compiled and Saved ", fname.lib.cached, "]\n")

    rm(Rfile, Rfunc)
}

rm(fname.lib.cached, fname.rprofile, iawRsourcefilenames, cache.mtime, rprofile.mtime, sources.mtime)


################################################################################
## Section 9: iaw Environment - Helper Functions
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


################################################################
## Section 10: Enhanced subset.data.frame with better error messages ---
################################################################


ns <- getNamespace("base")
unlockBinding("subset.data.frame", ns)
assign("subset.data.frame", iaw$subset.data.frame, envir = ns)
lockBinding("subset.data.frame", ns)



################################################################
## Section 11: Protection against common errors
################################################################

## Protect against accidental misuse of T for "time-end" vs. TRUE
T <- TRUE
lockBinding('T', .GlobalEnv)

## Clean up commonly misused variables
rmf <- function(n) {
    for (xxx in n) {
        if (exists(xxx, envir = .GlobalEnv)) rm(list = xxx, envir = .GlobalEnv)
    }
}

# shds <- ds <- data()$results[, "Item"]  ## preloaded data sets in R
suppressWarnings({ rmf( c("d", "lh", "lag", "rmf")) })


#################################################################
## Section 12: Cleanup & Final Message
#################################################################

message(libdir, "/Rprofile.R on ", getOption("hostname"), " completely loaded")


################################################################

## Tips & Reminders:
## new : my_url = config$url %||% "https://www.jumpingrivers.com/blog"
## sort_by(dfx, list(var1, var2), ...)
## mtcars |> lm(mpg ~ disp, data = _) |> _$coef
## r"literal string \textsf{hi}"
## try last_error(); last_trace()
