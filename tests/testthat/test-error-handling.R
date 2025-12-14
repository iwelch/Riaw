# Tests for abort, assert, estring

test_that("iaw$abort stops execution", {
    expect_error(iaw$abort("test error"))
})

test_that("iaw$abort requires character input", {
    expect_error(iaw$abort(123))
})

test_that("iaw$abort requires single string", {
    expect_error(iaw$abort(c("a", "b")))
})

test_that("iaw$abort works with simple message", {
    expect_error(iaw$abort("simple message"), "debug advice")
})

test_that("iaw$abort handles empty string", {
    expect_error(iaw$abort(""))
})

test_that("iaw$abort rejects NULL", {
    expect_error(iaw$abort(NULL))
})

test_that("iaw$abort rejects list", {
    expect_error(iaw$abort(list(a = 1)))
})

# assert tests
test_that("iaw$assert passes on TRUE", {
    expect_silent(iaw$assert(TRUE, "should not error"))
})

test_that("iaw$assert fails on FALSE", {
    expect_error(iaw$assert(FALSE, "assertion failed"))
})

test_that("iaw$assert returns invisible NULL on success", {
    expect_invisible(iaw$assert(TRUE, "test"))
})

test_that("iaw$assert works with expressions", {
    x <- 5
    expect_silent(iaw$assert(x > 0, "x must be positive"))
})

test_that("iaw$assert catches failed conditions", {
    x <- -5
    expect_error(iaw$assert(x > 0, "x must be positive"))
})

test_that("iaw$assert handles numeric comparison", {
    expect_silent(iaw$assert(1 == 1, "equality"))
})

test_that("iaw$assert handles string comparison", {
    expect_silent(iaw$assert("a" == "a", "string equality"))
})

# Failing tests
test_that("iaw$assert rejects non-logical condition", {
    expect_error(iaw$assert("not logical", "test"))
})

test_that("iaw$assert rejects vector condition", {
    expect_error(iaw$assert(c(TRUE, TRUE), "test"))
})

test_that("iaw$assert rejects NA condition", {
    expect_error(iaw$assert(NA, "test"))
})

# estring tests
test_that("iaw$estring returns unchanged string without placeholders", {
    expect_equal(iaw$estring("hello world"), "hello world")
})

test_that("iaw$estring interpolates variables", {
    x <- 42
    result <- iaw$estring("x={{x}}")
    expect_match(result, "42")
})

test_that("iaw$estring handles multiple placeholders", {
    a <- 1
    b <- 2
    result <- iaw$estring("a={{a}}, b={{b}}")
    expect_match(result, "1")
    expect_match(result, "2")
})

test_that("iaw$estring evaluates expressions", {
    result <- iaw$estring("sum={{1+2+3}}")
    expect_match(result, "6")
})

test_that("iaw$estring handles class()", {
    x <- "hello"
    result <- iaw$estring("type={{class(x)}}")
    expect_match(result, "character")
})

test_that("iaw$estring returns string type", {
    expect_type(iaw$estring("test"), "character")
})

test_that("iaw$estring handles empty string", {
    expect_equal(iaw$estring(""), "")
})

# Failing tests
test_that("iaw$estring rejects non-character", {
    expect_error(iaw$estring(123))
})

test_that("iaw$estring rejects vector", {
    expect_error(iaw$estring(c("a", "b")))
})

test_that("iaw$estring rejects NULL", {
    expect_error(iaw$estring(NULL))
})
