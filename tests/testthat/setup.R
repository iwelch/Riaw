# Setup file for Riaw tests
# This file is run before tests

# Create iaw environment if it doesn't exist
if (!exists("iaw")) {
    iaw <- new.env(parent = globalenv())
}

# Source all R files from the package
# In actual package testing, this would be handled by library(Riaw)

# Set seed for reproducibility where needed
set.seed(12345)

# Set options for testing
options(warn = 1)
