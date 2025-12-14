# Helper functions for testing

# Helper to check if a result is invisible
expect_invisible <- function(expr) {
    result <- withVisible(expr)
    testthat::expect_false(result$visible, 
        info = "Expected invisible return, but got visible")
    invisible(result$value)
}

# Helper to skip tests on Windows for multicore functions
skip_on_windows <- function() {
    if (.Platform$OS.type == "windows") {
        testthat::skip("Not supported on Windows")
    }
}

# Helper for approximate equality
expect_approx <- function(actual, expected, tolerance = 0.01) {
    testthat::expect_equal(actual, expected, tolerance = tolerance)
}
