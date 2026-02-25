library(testthat)

if (requireNamespace("iaw", quietly = TRUE)) {
    library(iaw)
    test_check("iaw")
} else {
    source("testthat/setup.R")
    test_dir("testthat")
}
