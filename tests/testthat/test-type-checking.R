# Tests for type-checking functions

# --- iaw$is.character ---

test_that("is.character returns TRUE for character vector", {
    expect_true(iaw$is.character("hello"))
    expect_true(iaw$is.character(c("a", "b")))
})

test_that("is.character returns FALSE for non-character", {
    expect_false(iaw$is.character(42))
    expect_false(iaw$is.character(factor("a")))
})

test_that("is.character checks length when specified", {
    expect_true(iaw$is.character("hello", 1))
    expect_true(iaw$is.character(c("a", "b"), 2))
    expect_false(iaw$is.character(c("a", "b"), 1))
})

test_that("is.character rejects invalid length parameter", {
    expect_error(iaw$is.character("a", "one"))
    expect_error(iaw$is.character("a", c(1, 2)))
})

# --- iaw$is.numeric ---

test_that("is.numeric returns TRUE for numeric vector", {
    expect_true(iaw$is.numeric(5))
    expect_true(iaw$is.numeric(1:3))
})

test_that("is.numeric returns FALSE for non-numeric", {
    expect_false(iaw$is.numeric("hello"))
    expect_false(iaw$is.numeric(TRUE))
})

test_that("is.numeric checks length when specified", {
    expect_true(iaw$is.numeric(5, 1))
    expect_true(iaw$is.numeric(c(1, 2, 3), 3))
    expect_false(iaw$is.numeric(c(1, 2, 3), 1))
})

test_that("is.numeric rejects invalid length parameter", {
    expect_error(iaw$is.numeric(5, "one"))
    expect_error(iaw$is.numeric(5, c(1, 2)))
})

# --- iaw$is.scalar ---

test_that("is.scalar returns TRUE for single atomic values", {
    expect_true(iaw$is.scalar(5))
    expect_true(iaw$is.scalar("a"))
    expect_true(iaw$is.scalar(TRUE))
    expect_true(iaw$is.scalar(NA))
})

test_that("is.scalar returns FALSE for non-scalars", {
    expect_false(iaw$is.scalar(c(1, 2)))
    expect_false(iaw$is.scalar(NULL))
    expect_false(iaw$is.scalar(list(a = 1)))
    expect_false(iaw$is.scalar(numeric(0)))
})

# --- iaw$is.instring ---

test_that("is.instring finds substring", {
    expect_true(iaw$is.instring("ab", "abcdef"))
    expect_false(iaw$is.instring("xy", "abcdef"))
})

test_that("is.instring returns logical vector for vector input", {
    result <- iaw$is.instring("ab", c("abc", "def", "abab"))
    expect_equal(result, c(TRUE, FALSE, TRUE))
})

test_that("is.instring rejects non-character or vector needle", {
    expect_error(iaw$is.instring(123, "abc"))
    expect_error(iaw$is.instring(c("a", "b"), "abc"))
})

# --- iaw$assert ---

test_that("assert passes silently on TRUE", {
    expect_invisible(iaw$assert(TRUE))
    expect_null(iaw$assert(TRUE, "should not fire"))
})

test_that("assert errors on FALSE with message", {
    expect_error(iaw$assert(FALSE, "bad value"), "bad value")
})

test_that("assert rejects non-logical or vector condition", {
    expect_error(iaw$assert("yes"))
    expect_error(iaw$assert(c(TRUE, FALSE)))
})

# --- iaw$check.names ---

test_that("check.names passes when names exist", {
    df <- data.frame(a = 1, b = 2, c = 3)
    expect_true(iaw$check.names(c("a", "b"), df))
})

test_that("check.names errors on missing names", {
    df <- data.frame(a = 1, b = 2)
    expect_error(iaw$check.names(c("a", "z"), df), "z")
})

# --- iaw$require.variables ---

test_that("require.variables passes when all present", {
    df <- data.frame(x = 1, y = 2)
    expect_true(iaw$require.variables(c("x", "y"), df))
})

test_that("require.variables errors on missing variable", {
    df <- data.frame(x = 1)
    expect_error(iaw$require.variables(c("x", "missing_col"), df), "missing_col")
})

# --- iaw$whatis ---

test_that("whatis describes numeric vector", {
    result <- iaw$whatis(1:10)
    expect_match(result, "integer")
    expect_match(result, "10")
})

test_that("whatis describes data frame with dimensions", {
    df <- data.frame(a = 1:5, b = 6:10)
    result <- iaw$whatis(df)
    expect_match(result, "data.frame")
    expect_match(result, "5x2")
})

test_that("whatis handles NULL", {
    result <- iaw$whatis(NULL)
    expect_match(result, "NULL")
})
