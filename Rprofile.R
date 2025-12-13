################################################################
## This is *not* a set of library functions, but an .Rprofile file.
################################################################

base::source(paste0(libdir, "/base.R"))

message(libdir, "/Rprofile.R on ", hostname())  ## libdir is usually "~/src/R"


T <- TRUE
lockBinding('T', .GlobalEnv)  ## protect the accidental misuse of T for "time-end" vs. TRUE


Sys.setenv('R_MAX_VSIZE' = 64 * 1024 * 1024 * 1024)


ARGV <- commandArgs(trailingOnly = TRUE)
ARGALL <- commandArgs()


options(show.error.locations = TRUE)
options(keep.source = TRUE, keep.source.pkgs = TRUE)  ## Keep source references
options(hostname = hostname())  ## for quick reference
options(scipen = 1000)  ## tilt away from scientific notation
options(digits = 4)
options(max.print = 500)
options(stringsAsFactors = FALSE)
options(editor = "emacs")
options(os= if (Sys.info()["sysname"] == "Darwin") "macos" else NULL ) ## (.Platform$OS.type == "unix")
options(datatable.fread.input.cmd.message = FALSE)
options(repos=c(CRAN="https://ftp.osuosl.org/pub/cran/"))


################################################################
## Loading all useful and necessary libraries
################################################################

addlibraries <- c("Cairo", "data.table", "R.utils", "inline", "knitr", "MASS", "sandwich", "bit64")


for (pkg in addlibraries) {
  if (!suppressWarnings(requireNamespace(pkg, quietly = TRUE))) {
    tryCatch(
      install.packages(pkg, dependencies = TRUE),
      error = function(e) message("Failed to install package: ", pkg)
    )
  }
}

# invisible(lapply(addlibraries, require, character.only = TRUE))

################################################################


stopifnot(file.exists("/opt/X11/include"))
Sys.setenv(C_INCLUDE_PATH = "/opt/X11/include")  ## for Cairo Installation

use("compiler", "cmpfun")
use("Cairo", c("CairoFonts", "CairoPDF"))  # NOTE: install fonts with `font_import(); loadfonts()`
use("data.table", c("fread", "fwrite", "first", "last", "nafill"))
use("MASS")

                                        # would be nice to be able to use "install.packages("devtools");
                                        # devtools::install_github("hadley/strict")

message("[loaded all libraries]")


################################################################
## Interactive vs. Non-Interactive Differences
################################################################

if (interactive()) {
    
    Rscriptname <- "Rscriptname unknown (interactive)"
    utils::loadhistory("~/.Rhistory")
    
    tty.setcols()

    .Last <- function() {
        if (interactive()) try(savehistory("~/.Rhistory"))
        try(system("rm -rf .RData ; echo '[removed any .RData]'"))
    }
    

} else {

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

    cat("[Non-Interactive: 1024 terminal widths]\n", file = stderr())
}



################################################################
## enhanced error handling, courtesy of rlang
################################################################


if (requireNamespace("rlang", quietly = TRUE)) {
    library(rlang)    ## helps with tidyverse, some better error support 
    options(
        error = rlang::entrace,
        rlang_backtrace_on_error = "branch"
    )
} else {
    options(
        error = function() {
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
        }
    )
}



################################################################
### Fonts courtesy of TeX
################################################################

texlive_dirs <- list.files("/usr/local/texlive/", pattern = "^[0-9]{4}$", full.names = TRUE)

if (length(texlive_dirs) == 1) {
    texlive_year <- max(as.integer(basename(texlive_dirs)))
    options(texlive = paste0("/usr/local/texlive/", texlive_year, "/"))
    options(texfonts = paste0(getOption("texlive"), "/texmf-dist/fonts/"))
    options(pfbdir = paste0(getOption("texfonts"), "type1/public/bera/"))
} else if (length(texlive_dirs) == 0) {
    message("Warning: No texlive installation found in /usr/local/texlive/ --- please install one for fonts")
} else {
    message("Warning: Toomany texlive installations found in /usr/local/texlive/ --- please remove older ones")
}


################################################################

library("parallel")             ## we need the whole thing, mostly in iaw::mclapply, rbind.mc.by, ...
options(mc.cores = max(1, detectCores() - 2))  ## computer becomes unresponsive if more (includes threads)


################################################################
## now precompile all the ~/src/R/*.R files into library.Rdata

## unfortunately, glob fails on this, we we just load them manually first
if (!exists('%and%')) base::source(paste0(libdir, "/%and%.R"))
if (!exists('%or%'))  base::source(paste0(libdir, "/%or%.R"))
if (!exists('%inrange%')) base::source(paste0(libdir, "/%inrange%.R"))

fname.lib.cached <- paste0(libdir, "/library.Rdata")

iawRsourcefilenames <- c(Sys.glob(paste0(libdir, "/*.R")), 
                         Sys.glob(paste0(libdir, "/plotsupport/*.R")))

if (!exists("docs")) docs <- NULL

if ( file.exists(fname.lib.cached) && 
     all(file.info(fname.lib.cached)$mtime > file.info(iawRsourcefilenames)$mtime) ) {
    
    load(fname.lib.cached)
    cat("Loaded already cached library ", fname.lib.cached, "\n", file = stderr())
    
} else {
    
    ## the funs can depend on one another, so first load all from source into the current environment
    iaw <- new.env()
    for (Rfile in iawRsourcefilenames) {
        if (grepl("Rprofile.R", Rfile)) next  ## skip this file itself!
        base::source(Rfile)
    }
    
    message("NOW COMPILING ALL FILES IN ", iawRsourcefilenames)
    
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

################################################################
## Clean up commonly misused and init variables no longer needed
for (n in c("d", "lh", "lag", "fname.lib.cached", "iawRsourcefilenames")) {
    if (exists(n, envir = .GlobalEnv)) rm(list = n, envir = .GlobalEnv)
}
## suppressWarnings({ rmf("d"); rmf("lh"); rmf("lag") })

################################################################

message(libdir, "/Rprofile.R on ", hostname(), " completely loaded")
