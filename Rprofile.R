#' R Profile Setup
#'
#' @name Rprofile
#'
#' Initializes the iaw environment and loads library functions.
#'
#' @details
#' This file should be sourced at R startup to initialize the iaw namespace
#' and load all utility functions.
#'
#' @family utilities
#' @keywords internal

# Create iaw environment
if (!exists("iaw")) {
    iaw <- new.env(parent = globalenv())
}

# Set options
options(
    stringsAsFactors = FALSE,
    warn = 1,
    width = 120
)

# Darwin-specific settings
if (Sys.info()["sysname"] == "Darwin") {
    if (file.exists("/opt/X11/include")) {
        Sys.setenv(C_INCLUDE_PATH = "/opt/X11/include")
    }
}

# Helper function for package loading
iaw$quietly.library <- function(pkg) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

# Print startup message
message("iaw environment initialized: ", date())
