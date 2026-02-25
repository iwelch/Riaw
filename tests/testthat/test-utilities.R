# Tests for utility functions

# --- iaw$strcat ---

test_that("strcat concatenates without separator by default", {
    expect_equal(iaw$strcat("Hello", "World"), "HelloWorld")
})

test_that("strcat uses custom separator", {
    expect_equal(iaw$strcat("a", "b", "c", sep = "-"), "a-b-c")
})

test_that("strcat handles single argument", {
    expect_equal(iaw$strcat("only"), "only")
})

# --- iaw$ascii ---

test_that("ascii converts character to ASCII", {
    result <- iaw$ascii("hello")
    expect_type(result, "character")
    expect_equal(result, "hello")
})

test_that("ascii converts non-character via as.character", {
    expect_equal(iaw$ascii(42), "42")
    expect_equal(iaw$ascii(TRUE), "TRUE")
})

# --- iaw$abort ---

test_that("abort throws error with message", {
    expect_error(iaw$abort("something broke"), "something broke")
})

test_that("abort rejects non-scalar-character input", {
    expect_error(iaw$abort(123))
    expect_error(iaw$abort(c("a", "b")))
})

# --- iaw$dict.lookup ---

test_that("dict.lookup finds existing keys", {
    d <- c(a = 1, b = 2, c = 3)
    result <- iaw$dict.lookup(c("a", "c"), d)
    expect_equal(as.numeric(result), c(1, 3))
})

test_that("dict.lookup returns default for missing keys", {
    d <- c(a = 1, b = 2)
    result <- iaw$dict.lookup(c("a", "z"), d, default = -1)
    expect_equal(as.numeric(result[1]), 1)
    expect_equal(result[2], -1)
})

test_that("dict.lookup returns NA by default for missing", {
    d <- c(a = 1)
    result <- iaw$dict.lookup("z", d)
    expect_true(is.na(result))
})

# --- iaw$grepcolname ---

test_that("grepcolname finds matching columns", {
    df <- data.frame(price_usd = 1, price_eur = 2, volume = 3)
    result <- iaw$grepcolname("price", df)
    expect_equal(sort(result), c("price_eur", "price_usd"))
})

test_that("grepcolname returns empty when no match", {
    df <- data.frame(a = 1, b = 2)
    result <- iaw$grepcolname("zzz", df)
    expect_length(result, 0)
})

# --- iaw$index.of.variable ---

test_that("index.of.variable returns correct column index", {
    df <- data.frame(x = 1, y = 2, z = 3)
    expect_equal(iaw$index.of.variable("y", df), 2)
})

test_that("index.of.variable returns empty for missing name", {
    df <- data.frame(x = 1, y = 2)
    result <- iaw$index.of.variable("missing", df)
    expect_length(result, 0)
})

# --- iaw$bisection ---

test_that("bisection finds sqrt(2)", {
    result <- iaw$bisection(function(x) x^2 - 2, 0, 2)
    expect_equal(result$mid, sqrt(2), tolerance = 1e-3)
})

test_that("bisection finds root of linear function", {
    result <- iaw$bisection(function(x) x - 5, 0, 10)
    expect_equal(result$mid, 5, tolerance = 1e-3)
})

test_that("bisection errors when same sign at bounds", {
    expect_error(iaw$bisection(function(x) x^2 + 1, 0, 2), "same sign")
})

test_that("bisection supports vectorized inputs", {
    result <- iaw$bisection(function(x) x^2 - c(1, 4, 9), c(0, 0, 0), c(2, 3, 4))
    expect_equal(result$mid, c(1, 2, 3), tolerance = 1e-3)
})

# --- iaw$nearest ---

test_that("nearest finds closest value", {
    expect_equal(iaw$nearest(c(1, 5, 10), 7), 5)
    expect_equal(iaw$nearest(c(1, 5, 10), 9), 10)
})

test_that("nearest finds exact match", {
    expect_equal(iaw$nearest(c(1, 5, 10), 5), 5)
})

# --- iaw$which.nearest ---

test_that("which.nearest returns correct index", {
    expect_equal(iaw$which.nearest(c(1, 5, 10), 7), 2)
    expect_equal(iaw$which.nearest(c(1, 5, 10), 9), 3)
})

test_that("which.nearest returns index of exact match", {
    expect_equal(iaw$which.nearest(c(10, 20, 30), 20), 2)
})

# --- iaw$summary ---

test_that("summary returns numeric matrix for data frame", {
    df <- data.frame(x = rnorm(100), y = rnorm(100))
    result <- iaw$summary(df, "p")
    expect_true(is.matrix(result))
    expect_equal(nrow(result), 2)
    expect_true("mean" %in% colnames(result))
})

test_that("summary works with a vector input", {
    result <- iaw$summary(rnorm(50), "p")
    expect_true(is.matrix(result))
    expect_equal(nrow(result), 1)
})

test_that("summary errors on non-numeric data frame", {
    df <- data.frame(a = letters[1:5])
    expect_error(iaw$summary(df))
})

# --- iaw$sprint.data.frame ---

test_that("sprint.data.frame returns string representation", {
    df <- data.frame(a = 1:3, b = c("x", "y", "z"))
    result <- iaw$sprint.data.frame(df)
    expect_type(result, "character")
    expect_match(result, "a")
    expect_match(result, "b")
})

test_that("sprint.data.frame respects n parameter", {
    df <- data.frame(x = 1:100)
    full <- iaw$sprint.data.frame(df)
    short <- iaw$sprint.data.frame(df, n = 2)
    expect_true(nchar(short) < nchar(full))
})

# --- iaw$instrumentR ---

test_that("instrumentR produces parseable output", {
    infile  <- file.path(tempdir(), "test_input.R")
    outfile <- file.path(tempdir(), "test_output.R")
    writeLines(c(
        "x <- 1",
        "y <- x + 2",
        "# a comment",
        "z <- x * y"
    ), infile)
    on.exit(unlink(c(infile, outfile)), add = TRUE)

    result <- iaw$instrumentR(infile, outfile)
    expect_equal(result, outfile)
    expect_true(file.exists(outfile))

    # Output must parse without error
    parsed <- parse(outfile)
    expect_true(length(parsed) > 0)
})

test_that("instrumentR rejects non-R file", {
    expect_error(iaw$instrumentR("data.csv"))
})
