# Tests for miscellaneous functions: strcat, tabular, normalize, standardize, colN, colSds, rowSds, etc.

# strcat tests
test_that("iaw$strcat concatenates strings", {
    result <- iaw$strcat("Hello", "World")
    expect_equal(result, "HelloWorld")
})

test_that("iaw$strcat handles single string", {
    result <- iaw$strcat("Hello")
    expect_equal(result, "Hello")
})

test_that("iaw$strcat handles multiple strings", {
    result <- iaw$strcat("a", "b", "c", "d")
    expect_equal(result, "abcd")
})

test_that("iaw$strcat handles numbers", {
    result <- iaw$strcat("x", 1, "y", 2)
    expect_equal(result, "x1y2")
})

test_that("iaw$strcat handles empty string", {
    result <- iaw$strcat("a", "", "b")
    expect_equal(result, "ab")
})

test_that("iaw$strcat returns character", {
    expect_type(iaw$strcat("a", "b"), "character")
})

test_that("iaw$strcat length is 1", {
    result <- iaw$strcat("a", "b", "c")
    expect_length(result, 1)
})

test_that("iaw$strcat handles whitespace", {
    result <- iaw$strcat("Hello", " ", "World")
    expect_equal(result, "Hello World")
})

test_that("iaw$strcat handles special characters", {
    result <- iaw$strcat("a", "\n", "b")
    expect_equal(result, "a\nb")
})

test_that("iaw$strcat handles vectors", {
    result <- iaw$strcat(c("a", "b"), c("1", "2"))
    expect_length(result, 2)
})

# tabular tests
test_that("iaw$tabularsummary returns data frame", {
    x <- c("a", "b", "a", "c")
    result <- iaw$tabularsummary(x)
    expect_s3_class(result, "data.frame")
})

test_that("iaw$tabularsummary counts correctly", {
    x <- c("a", "a", "b")
    result <- iaw$tabularsummary(x)
    expect_equal(result$count[result$value == "a"], 2)
})

test_that("iaw$tabularsummary includes percentages", {
    x <- c("a", "a", "b", "b")
    result <- iaw$tabularsummary(x)
    expect_true("pct" %in% names(result))
})

test_that("iaw$tabularsummary sorts by count", {
    x <- c("a", "b", "b", "b", "a")
    result <- iaw$tabularsummary(x, sort = TRUE)
    expect_equal(result$value[1], "b")
})

test_that("iaw$tabularsummary unsorted option", {
    x <- c("a", "b", "b", "a")
    result <- iaw$tabularsummary(x, sort = FALSE)
    expect_s3_class(result, "data.frame")
})

test_that("iaw$tabularsummary handles NA", {
    x <- c("a", NA, "b")
    result <- iaw$tabularsummary(x)
    expect_true(nrow(result) >= 2)
})

test_that("iaw$tabularsummary has value column", {
    x <- c("a", "b")
    result <- iaw$tabularsummary(x)
    expect_true("value" %in% names(result))
})

test_that("iaw$tabularsummary has count column", {
    x <- c("a", "b")
    result <- iaw$tabularsummary(x)
    expect_true("count" %in% names(result))
})

test_that("iaw$tabularsummary percentages sum to 100", {
    x <- c("a", "b", "c", "d")
    result <- iaw$tabularsummary(x)
    expect_equal(sum(result$pct), 100)
})

test_that("iaw$tabularsummary handles numeric input", {
    x <- c(1, 2, 2, 3)
    result <- iaw$tabularsummary(x)
    expect_s3_class(result, "data.frame")
})

# normalize tests
test_that("iaw$normalize returns values in [0,1]", {
    x <- c(10, 20, 30, 40, 50)
    result <- iaw$normalize(x)
    expect_true(all(result >= 0 & result <= 1))
})

test_that("iaw$normalize min is 0", {
    x <- c(1, 2, 3, 4, 5)
    result <- iaw$normalize(x)
    expect_equal(min(result), 0)
})

test_that("iaw$normalize max is 1", {
    x <- c(1, 2, 3, 4, 5)
    result <- iaw$normalize(x)
    expect_equal(max(result), 1)
})

test_that("iaw$normalize preserves length", {
    x <- 1:100
    expect_equal(length(iaw$normalize(x)), 100)
})

test_that("iaw$normalize handles negative values", {
    x <- c(-10, 0, 10)
    result <- iaw$normalize(x)
    expect_equal(result, c(0, 0.5, 1))
})

test_that("iaw$normalize returns numeric", {
    expect_type(iaw$normalize(1:5), "double")
})

test_that("iaw$normalize handles NA", {
    x <- c(1, NA, 3)
    result <- iaw$normalize(x)
    expect_true(is.na(result[2]))
})

# Failing tests
test_that("iaw$normalize rejects non-numeric", {
    expect_error(iaw$normalize(c("a", "b")))
})

test_that("iaw$normalize rejects character", {
    expect_error(iaw$normalize(letters))
})

test_that("iaw$normalize rejects list", {
    expect_error(iaw$normalize(list(1, 2, 3)))
})

# standardize tests
test_that("iaw$standardize returns mean 0", {
    x <- rnorm(100, mean = 50, sd = 10)
    result <- iaw$standardize(x)
    expect_equal(mean(result), 0, tolerance = 1e-10)
})

test_that("iaw$standardize returns sd 1", {
    x <- rnorm(100, mean = 50, sd = 10)
    result <- iaw$standardize(x)
    expect_equal(sd(result), 1, tolerance = 1e-10)
})

test_that("iaw$standardize preserves length", {
    x <- 1:100
    expect_equal(length(iaw$standardize(x)), 100)
})

test_that("iaw$standardize handles constant", {
    x <- rep(5, 10)
    result <- iaw$standardize(x)
    expect_true(all(is.nan(result)))
})

test_that("iaw$standardize returns numeric", {
    expect_type(iaw$standardize(1:10), "double")
})

test_that("iaw$standardize handles NA", {
    x <- c(1, NA, 3, 4, 5)
    result <- iaw$standardize(x)
    expect_true(is.na(result[2]))
})

test_that("iaw$standardize handles negative values", {
    x <- c(-10, -5, 0, 5, 10)
    result <- iaw$standardize(x)
    expect_equal(mean(result), 0, tolerance = 1e-10)
})

# Failing tests
test_that("iaw$standardize rejects non-numeric", {
    expect_error(iaw$standardize(c("a", "b")))
})

test_that("iaw$standardize rejects character", {
    expect_error(iaw$standardize(letters))
})

test_that("iaw$standardize rejects list", {
    expect_error(iaw$standardize(list(1, 2, 3)))
})

# colN tests
test_that("iaw$colN counts non-NA", {
    df <- data.frame(a = c(1, NA, 3), b = c(1, 2, 3))
    result <- iaw$colN(df)
    expect_equal(result[["a"]], c(a = 2))
})

test_that("iaw$colN returns named vector", {
    df <- data.frame(x = 1:5, y = 1:5)
    result <- iaw$colN(df)
    expect_true(!is.null(names(result)))
})

test_that("iaw$colN handles all NA", {
    df <- data.frame(a = c(NA, NA), b = c(1, 2))
    result <- iaw$colN(df)
    expect_equal(result["a"], c(a = 0))
})

test_that("iaw$colN handles no NA", {
    df <- data.frame(a = 1:5)
    result <- iaw$colN(df)
    expect_equal(result["a"], c(a = 5))
})

test_that("iaw$colN works on matrix", {
    m <- matrix(c(1, NA, 3, 4), nrow = 2)
    result <- iaw$colN(m)
    expect_length(result, 2)
})

test_that("iaw$colN multiple columns", {
    df <- data.frame(a = 1:3, b = 1:3, c = 1:3)
    result <- iaw$colN(df)
    expect_length(result, 3)
})

test_that("iaw$colN returns integer", {
    df <- data.frame(a = 1:5)
    result <- iaw$colN(df)
    expect_type(result, "integer")
})

# colSds tests
test_that("iaw$colSds computes column SDs", {
    df <- data.frame(a = c(1, 2, 3), b = c(10, 20, 30))
    result <- iaw$colSds(df)
    expect_equal(unname(result[1]), sd(c(1, 2, 3)))
})

test_that("iaw$colSds returns named vector", {
    df <- data.frame(x = 1:5, y = 1:5)
    result <- iaw$colSds(df)
    expect_true(!is.null(names(result)))
})

test_that("iaw$colSds handles NA", {
    df <- data.frame(a = c(1, NA, 3))
    result <- iaw$colSds(df, na.rm = TRUE)
    expect_true(!is.na(result[1]))
})

test_that("iaw$colSds constant column", {
    df <- data.frame(a = c(5, 5, 5))
    result <- iaw$colSds(df)
    expect_equal(result[1], 0)
})

test_that("iaw$colSds multiple columns", {
    df <- data.frame(a = 1:5, b = 1:5, c = 1:5)
    result <- iaw$colSds(df)
    expect_length(result, 3)
})

test_that("iaw$colSds returns numeric", {
    df <- data.frame(a = 1:5)
    result <- iaw$colSds(df)
    expect_type(result, "double")
})

test_that("iaw$colSds works on matrix", {
    m <- matrix(1:6, nrow = 3)
    result <- iaw$colSds(m)
    expect_length(result, 2)
})

# rowSds tests
test_that("iaw$rowSds computes row SDs", {
    m <- matrix(c(1, 2, 3, 10, 20, 30), nrow = 2, byrow = TRUE)
    result <- iaw$rowSds(m)
    expect_equal(result[1], sd(c(1, 2, 3)))
})

test_that("iaw$rowSds preserves row names", {
    m <- matrix(1:6, nrow = 2)
    rownames(m) <- c("r1", "r2")
    result <- iaw$rowSds(m)
    expect_true(!is.null(names(result)))
})

test_that("iaw$rowSds handles NA", {
    m <- matrix(c(1, NA, 3, 4, 5, 6), nrow = 2)
    result <- iaw$rowSds(m, na.rm = TRUE)
    expect_true(!is.na(result[1]))
})

test_that("iaw$rowSds constant row", {
    m <- matrix(c(5, 5, 5, 1, 2, 3), nrow = 2, byrow = TRUE)
    result <- iaw$rowSds(m)
    expect_equal(result[1], 0)
})

test_that("iaw$rowSds multiple rows", {
    m <- matrix(1:12, nrow = 4)
    result <- iaw$rowSds(m)
    expect_length(result, 4)
})

test_that("iaw$rowSds returns numeric", {
    m <- matrix(1:6, nrow = 2)
    result <- iaw$rowSds(m)
    expect_type(result, "double")
})

test_that("iaw$rowSds works on data frame", {
    df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
    result <- iaw$rowSds(df)
    expect_length(result, 3)
})

# sdp, varp, covp tests (population statistics)
test_that("iaw$varp computes population variance", {
    x <- 1:5
    result <- iaw$varp(x)
    n <- length(x)
    expected <- var(x) * (n - 1) / n
    expect_equal(result, expected)
})

test_that("iaw$sdp computes population SD", {
    x <- 1:5
    result <- iaw$sdp(x)
    expect_equal(result, sqrt(iaw$varp(x)))
})

test_that("iaw$covp computes population covariance", {
    x <- 1:5
    y <- 2:6
    result <- iaw$covp(x, y)
    n <- length(x)
    expected <- cov(x, y) * (n - 1) / n
    expect_equal(result, expected)
})

test_that("iaw$varp smaller than sample variance", {
    x <- 1:10
    expect_true(iaw$varp(x) < var(x))
})

test_that("iaw$sdp smaller than sample SD", {
    x <- 1:10
    expect_true(iaw$sdp(x) < sd(x))
})

test_that("iaw$varp handles NA with na.rm", {
    x <- c(1, NA, 3, 4, 5)
    result <- iaw$varp(x, na.rm = TRUE)
    expect_true(!is.na(result))
})

test_that("iaw$covp returns numeric", {
    x <- 1:5
    y <- 1:5
    expect_type(iaw$covp(x, y), "double")
})

# Failing tests
test_that("iaw$varp rejects non-numeric", {
    expect_error(iaw$varp(letters))
})

test_that("iaw$sdp rejects non-numeric", {
    expect_error(iaw$sdp(letters))
})

test_that("iaw$covp rejects mismatched lengths", {
    expect_error(iaw$covp(1:5, 1:3))
})
