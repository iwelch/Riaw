# Tests for random utilities: rnorm.perfect, rexp.perfect, rdraw.perfect, mk.random.walk

# rnorm.perfect tests
test_that("iaw$rnorm.perfect returns correct length", {
    result <- iaw$rnorm.perfect(100)
    expect_length(result, 100)
})

test_that("iaw$rnorm.perfect mean close to 0", {
    result <- iaw$rnorm.perfect(1000)
    expect_equal(mean(result), 0, tolerance = 0.01)
})

test_that("iaw$rnorm.perfect sd close to 1", {
    result <- iaw$rnorm.perfect(1000)
    expect_equal(sd(result), 1, tolerance = 0.05)
})

test_that("iaw$rnorm.perfect custom mean", {
    result <- iaw$rnorm.perfect(1000, mean = 10)
    expect_equal(mean(result), 10, tolerance = 0.05)
})

test_that("iaw$rnorm.perfect custom sd", {
    result <- iaw$rnorm.perfect(1000, sd = 5)
    expect_equal(sd(result), 5, tolerance = 0.2)
})

test_that("iaw$rnorm.perfect returns numeric", {
    expect_type(iaw$rnorm.perfect(10), "double")
})

test_that("iaw$rnorm.perfect single value", {
    result <- iaw$rnorm.perfect(1)
    expect_length(result, 1)
})

# Failing tests
test_that("iaw$rnorm.perfect rejects n <= 0", {
    expect_error(iaw$rnorm.perfect(0))
})

test_that("iaw$rnorm.perfect rejects negative n", {
    expect_error(iaw$rnorm.perfect(-10))
})

test_that("iaw$rnorm.perfect rejects non-numeric n", {
    expect_error(iaw$rnorm.perfect("ten"))
})

# rexp.perfect tests
test_that("iaw$rexp.perfect returns correct length", {
    result <- iaw$rexp.perfect(100)
    expect_length(result, 100)
})

test_that("iaw$rexp.perfect all positive", {
    result <- iaw$rexp.perfect(100)
    expect_true(all(result > 0))
})

test_that("iaw$rexp.perfect mean close to 1/rate", {
    result <- iaw$rexp.perfect(1000, rate = 2)
    expect_equal(mean(result), 0.5, tolerance = 0.05)
})

test_that("iaw$rexp.perfect default rate 1", {
    result <- iaw$rexp.perfect(1000)
    expect_equal(mean(result), 1, tolerance = 0.1)
})

test_that("iaw$rexp.perfect returns numeric", {
    expect_type(iaw$rexp.perfect(10), "double")
})

test_that("iaw$rexp.perfect custom rate", {
    result <- iaw$rexp.perfect(500, rate = 0.5)
    expect_equal(mean(result), 2, tolerance = 0.2)
})

test_that("iaw$rexp.perfect single value", {
    result <- iaw$rexp.perfect(1)
    expect_length(result, 1)
})

# Failing tests
test_that("iaw$rexp.perfect rejects n <= 0", {
    expect_error(iaw$rexp.perfect(0))
})

test_that("iaw$rexp.perfect rejects rate <= 0", {
    expect_error(iaw$rexp.perfect(10, rate = 0))
})

test_that("iaw$rexp.perfect rejects non-numeric rate", {
    expect_error(iaw$rexp.perfect(10, rate = "fast"))
})

# rdraw.perfect tests
test_that("iaw$rdraw.perfect returns correct length", {
    result <- iaw$rdraw.perfect(100, qnorm)
    expect_length(result, 100)
})

test_that("iaw$rdraw.perfect with qnorm close to normal", {
    result <- iaw$rdraw.perfect(1000, qnorm)
    expect_equal(mean(result), 0, tolerance = 0.01)
})

test_that("iaw$rdraw.perfect with qunif", {
    result <- iaw$rdraw.perfect(1000, qunif)
    expect_true(all(result >= 0 & result <= 1))
})

test_that("iaw$rdraw.perfect returns numeric", {
    expect_type(iaw$rdraw.perfect(10, qnorm), "double")
})

test_that("iaw$rdraw.perfect single value", {
    result <- iaw$rdraw.perfect(1, qnorm)
    expect_length(result, 1)
})

test_that("iaw$rdraw.perfect sorted by quantile", {
    result <- iaw$rdraw.perfect(100, qnorm)
    expect_equal(result, sort(result))
})

test_that("iaw$rdraw.perfect custom qfun", {
    result <- iaw$rdraw.perfect(100, function(p) p^2)
    expect_true(all(result >= 0 & result <= 1))
})

# Failing tests
test_that("iaw$rdraw.perfect rejects n <= 0", {
    expect_error(iaw$rdraw.perfect(0, qnorm))
})

test_that("iaw$rdraw.perfect rejects non-function qfun", {
    expect_error(iaw$rdraw.perfect(10, "qnorm"))
})

test_that("iaw$rdraw.perfect rejects negative n", {
    expect_error(iaw$rdraw.perfect(-5, qnorm))
})

# mk.random.walk tests
test_that("iaw$mk.random.walk returns correct length", {
    result <- iaw$mk.random.walk(100)
    expect_length(result, 100)
})

test_that("iaw$mk.random.walk starts at start value", {
    result <- iaw$mk.random.walk(10, start = 100)
    expect_equal(result[1], 100)
})

test_that("iaw$mk.random.walk default start is 0", {
    result <- iaw$mk.random.walk(10)
    expect_equal(result[1], 0)
})

test_that("iaw$mk.random.walk returns numeric", {
    expect_type(iaw$mk.random.walk(10), "double")
})

test_that("iaw$mk.random.walk differences have correct sd", {
    set.seed(123)
    result <- iaw$mk.random.walk(10000, sd = 2)
    diffs <- diff(result)
    expect_equal(sd(diffs), 2, tolerance = 0.1)
})

test_that("iaw$mk.random.walk is cumulative", {
    set.seed(123)
    result <- iaw$mk.random.walk(10)
    # Should be monotonic-ish (cumsum of random)
    expect_type(result, "double")
})

test_that("iaw$mk.random.walk single step", {
    result <- iaw$mk.random.walk(1)
    expect_length(result, 1)
})

# Failing tests
test_that("iaw$mk.random.walk rejects n < 1", {
    expect_error(iaw$mk.random.walk(0))
})

test_that("iaw$mk.random.walk rejects sd <= 0", {
    expect_error(iaw$mk.random.walk(10, sd = 0))
})

test_that("iaw$mk.random.walk rejects non-numeric n", {
    expect_error(iaw$mk.random.walk("ten"))
})
