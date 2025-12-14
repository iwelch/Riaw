################################################################
## Riaw .Rprofile Configuration
##
## This is *not* a set of library functions, but an .Rprofile file.
## It initializes the R session with preferred settings and loads
## the Riaw library functions.
##
## @details
## To use, add to your ~/.Rprofile:
##   libdir <- "~/src/Riaw"
##   source(paste0(libdir, "/Rprofile.R"))
##
################################################################

base::source(paste0(libdir, "/base.R"))

message(libdir, "/Rprofile.R on ", hostname())

# Protect T from accidental misuse (e.g., as loop variable)
T <- TRUE
lockBinding('T', .GlobalEnv)

Sys.setenv('R_MAX_VSIZE' = 64 * 1024 * 1024 * 1024)

ARGV <- commandArgs(trailingOnly = TRUE)
ARGALL <- commandArgs()

################################################################
## Session Options
################################################################

options(show.error.locations = TRUE)
options(keep.source = TRUE, keep.source.pkgs = TRUE)
options(hostname = hostname())
options(scipen = 1000)
options(digits = 4)
options(max.print = 500)
options(stringsAsFactors = FALSE)
options(editor = "emacs")
options(os = if (Sys.info()["sysname"] == "Darwin") "macos" else "linux")
options(datatable.fread.input.cmd.message = FALSE)
options(repos = c(CRAN = "https://ftp.osuosl.org/pub/cran/"))

################################################################
## Loading useful libraries
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

################################################################
## Platform-specific setup (FIX: conditional check for macOS X11)
################################################################

# Only set X11 paths on macOS with XQuartz installed
if (Sys.info()["sysname"] == "Darwin" && file.exists("/opt/X11/include")) {
    Sys.setenv(C_INCLUDE_PATH = "/opt/X11/include")  # for Cairo Installation
}

# Load packages conditionally based on R version
if (getRversion() >= "4.5.0") {
    use("compiler", "cmpfun")
    use("Cairo", c("CairoFonts", "CairoPDF"))
    use("data.table", c("fread", "fwrite", "first", "last", "nafill"))
    use("MASS")
} else {
    library(compiler)
    cmpfun <- compiler::cmpfun
    if (requireNamespace("Cairo", quietly = TRUE)) {
        library(Cairo)
    }
    library(data.table)
    library(MASS)
}

message("[loaded all libraries]")

################################################################
## Interactive vs. Non-Interactive Differences
################################################################

if (interactive()) {
    
    Rscriptname <- "Rscriptname unknown (interactive)"
    utils::loadhistory("~/.Rhistory")
    
    if (exists("tty.setcols", mode = "function")) tty.setcols()
    
    .Last <- function() {
        if (interactive()) try(savehistory("~/.Rhistory"))
        try(system("rm -rf .RData ; echo '[removed any .RData]'"))
    }
    
} else {
    
    options(width = 1024)
    if (getRversion() >= "4.5.0") {
        use("tools", "md5sum")
    } else {
        library(tools)
    }
    cat("#CommandArgs: ", paste(ARGALL, collapse = " "), "\n")
    
    if (length(ARGALL) >= 4 && grepl("--file=", ARGALL[4])) {
        Rfilename <- substr(ARGALL[4], 8, 100)
        md5sumval <- tools::md5sum(Rfilename)
        Rscriptname <- paste("Script: ", R.home(), "//", Rfilename, " ", md5sumval, "\n")
        cat("#     ", Rscriptname)
        cat("#----------------\n")
        print(file.info(Rfilename))
        cat("#----------------\n")
    }
    
    cat("[Non-Interactive: 1024 terminal widths]\n", file = stderr())
}

################################################################
## Enhanced error handling
################################################################

if (requireNamespace("rlang", quietly = TRUE)) {
    library(rlang)
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
## TeX Fonts
################################################################

texlive_dirs <- list.files("/usr/local/texlive/", pattern = "^[0-9]{4}$", full.names = TRUE)

if (length(texlive_dirs) == 1) {
    texlive_year <- max(as.integer(basename(texlive_dirs)))
    options(texlive = paste0("/usr/local/texlive/", texlive_year, "/"))
    options(texfonts = paste0(getOption("texlive"), "/texmf-dist/fonts/"))
    options(pfbdir = paste0(getOption("texfonts"), "type1/public/bera/"))
} else if (length(texlive_dirs) == 0) {
    message("Note: No texlive installation found in /usr/local/texlive/")
} else {
    message("Note: Multiple texlive installations found; using newest")
    texlive_year <- max(as.integer(basename(texlive_dirs)))
    options(texlive = paste0("/usr/local/texlive/", texlive_year, "/"))
}

################################################################
## Parallel processing
################################################################

library("parallel")
options(mc.cores = max(1, detectCores() - 2))

################################################################
## Load and compile Riaw library functions
################################################################

# Load operators first (they're needed by other files)
if (!exists('%and%')) base::source(paste0(libdir, "/%and%.R"))
if (!exists('%or%'))  base::source(paste0(libdir, "/%or%.R"))
if (!exists('%inrange%')) base::source(paste0(libdir, "/%inrange%.R"))

fname.lib.cached <- paste0(libdir, "/library.Rdata")

iawRsourcefilenames <- c(
    Sys.glob(paste0(libdir, "/*.R")),
    Sys.glob(paste0(libdir, "/plotsupport/*.R"))
)

if (!exists("docs")) docs <- NULL

if (file.exists(fname.lib.cached) &&
    all(file.info(fname.lib.cached)$mtime > file.info(iawRsourcefilenames)$mtime)) {
    
    load(fname.lib.cached)
    cat("Loaded cached library ", fname.lib.cached, "\n", file = stderr())
    
} else {
    
    iaw <- new.env()
    for (Rfile in iawRsourcefilenames) {
        if (grepl("Rprofile.R", Rfile)) next
        base::source(Rfile)
    }
    
    message("Compiling all files in ", libdir)
    
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
## Clean up
################################################################

for (n in c("d", "lh", "lag", "fname.lib.cached", "iawRsourcefilenames")) {
    if (exists(n, envir = .GlobalEnv)) rm(list = n, envir = .GlobalEnv)
}

message(libdir, "/Rprofile.R on ", hostname(), " completely loaded")
