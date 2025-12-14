# Tests for is.character, is.numeric, is.scalar, is.inrange, is.instring

# is.character tests
test_that("iaw$is.character returns TRUE for character of correct length", {
    expect_true(iaw$is.character("hello", 1))
})

test_that("iaw$is.character returns FALSE for wrong length", {
    expect_false(iaw$is.character(c("a", "b"), 1))
})

test_that("iaw$is.character returns FALSE for non-character", {
    expect_false(iaw$is.character(123, 1))
})

test_that("iaw$is.character handles empty string", {
    expect_true(iaw$is.character("", 1))
})

test_that("iaw$is.character handles character vector", {
    expect_true(iaw$is.character(c("a", "b", "c"), 3))
})

test_that("iaw$is.character returns FALSE for factor", {
    expect_false(iaw$is.character(factor("a"), 1))
})

test_that("iaw$is.character default length is 0", {
    expect_false(iaw$is.character("a"))  # length 1 != 0
})

# Failing tests
test_that("iaw$is.character rejects non-numeric length", {
    expect_error(iaw$is.character("a", "one"))
})

test_that("iaw$is.character rejects vector length parameter", {
    expect_error(iaw$is.character("a", c(1, 2)))
})

test_that("iaw$is.character rejects NULL length", {
    expect_error(iaw$is.character("a", NULL))
})

# is.numeric tests
test_that("iaw$is.numeric returns TRUE for numeric of correct length", {
    expect_true(iaw$is.numeric(5, 1))
})

test_that("iaw$is.numeric returns FALSE for wrong length", {
    expect_false(iaw$is.numeric(c(1, 2, 3), 1))
})

test_that("iaw$is.numeric returns FALSE for non-numeric", {
    expect_false(iaw$is.numeric("hello", 1))
})

test_that("iaw$is.numeric handles integer", {
    expect_true(iaw$is.numeric(5L, 1))
})

test_that("iaw$is.numeric handles vector", {
    expect_true(iaw$is.numeric(1:5, 5))
})

test_that("iaw$is.numeric returns FALSE for logical", {
    expect_false(iaw$is.numeric(TRUE, 1))
})

test_that("iaw$is.numeric default length is 0", {
    expect_false(iaw$is.numeric(5))
})

# Failing tests
test_that("iaw$is.numeric rejects non-numeric length", {
    expect_error(iaw$is.numeric(5, "one"))
})

test_that("iaw$is.numeric rejects vector length", {
    expect_error(iaw$is.numeric(5, c(1, 2)))
})

test_that("iaw$is.numeric rejects NULL length", {
    expect_error(iaw$is.numeric(5, NULL))
})

# is.scalar tests
test_that("iaw$is.scalar returns TRUE for single numeric", {
    expect_true(iaw$is.scalar(5))
})

test_that("iaw$is.scalar returns TRUE for single character", {
    expect_true(iaw$is.scalar("a"))
})

test_that("iaw$is.scalar returns TRUE for single logical", {
    expect_true(iaw$is.scalar(TRUE))
})

test_that("iaw$is.scalar returns FALSE for vector", {
    expect_false(iaw$is.scalar(c(1, 2)))
})

test_that("iaw$is.scalar returns FALSE for list", {
    expect_false(iaw$is.scalar(list(a = 1)))
})

test_that("iaw$is.scalar returns FALSE for NULL", {
    expect_false(iaw$is.scalar(NULL))
})

test_that("iaw$is.scalar returns FALSE for data frame", {
    expect_false(iaw$is.scalar(data.frame(a = 1)))
})

test_that("iaw$is.scalar handles NA", {
    expect_true(iaw$is.scalar(NA))
})

test_that("iaw$is.scalar handles empty vector", {
    expect_false(iaw$is.scalar(numeric(0)))
})

test_that("iaw$is.scalar handles complex", {
    expect_true(iaw$is.scalar(1+2i))
})


# is.instring tests
test_that("iaw$is.instring finds pattern", {
    expect_true(iaw$is.instring("ab", "abcdef"))
})

test_that("iaw$is.instring returns FALSE when not found", {
    expect_false(iaw$is.instring("xy", "abcdef"))
})

test_that("iaw$is.instring works with vector", {
    result <- iaw$is.instring("ab", c("abc", "def", "abab"))
    expect_equal(result, c(TRUE, FALSE, TRUE))
})

test_that("iaw$is.instring is case sensitive", {
    expect_false(iaw$is.instring("AB", "abcdef"))
})

test_that("iaw$is.instring finds at start", {
    expect_true(iaw$is.instring("abc", "abcdef"))
})

test_that("iaw$is.instring finds at end", {
    expect_true(iaw$is.instring("def", "abcdef"))
})

test_that("iaw$is.instring returns logical vector", {
    expect_type(iaw$is.instring("a", c("a", "b")), "logical")
})

# Failing tests
test_that("iaw$is.instring rejects non-character needle", {
    expect_error(iaw$is.instring(123, "abc"))
})

test_that("iaw$is.instring rejects vector needle", {
    expect_error(iaw$is.instring(c("a", "b"), "abc"))
})

test_that("iaw$is.instring rejects non-character haystack", {
    expect_error(iaw$is.instring("a", 123))
})
