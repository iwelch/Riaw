# Tests for random/perfect-draw utilities

# --- rdraw.perfect ---
test_that("rdraw.perfect returns correct length", {
    result <- iaw$rdraw.perfect(50, qnorm)
    expect_length(result, 50)
})

test_that("rdraw.perfect with qunif stays in [0,1]", {
    result <- iaw$rdraw.perfect(200, qunif)
    expect_true(all(result >= 0 & result <= 1))
})

test_that("rdraw.perfect rejects non-function qfun", {
    expect_error(iaw$rdraw.perfect(10, "qnorm"))
})

# --- rnorm.perfect ---
test_that("rnorm.perfect mean is near 0 for large n", {
    x <- iaw$rnorm.perfect(1000)
    expect_equal(mean(x), 0, tolerance = 0.01)
})

test_that("rnorm.perfect respects custom mean and sd", {
    x <- iaw$rnorm.perfect(1000, mean = 5, sd = 2)
    expect_equal(mean(x), 5, tolerance = 0.05)
    expect_equal(sd(x), 2, tolerance = 0.2)
})

test_that("rnorm.perfect rejects n=0", {
    expect_error(iaw$rnorm.perfect(0))
})

# --- rexp.perfect ---
test_that("rexp.perfect all values positive", {
    x <- iaw$rexp.perfect(100)
    expect_true(all(x > 0))
})

test_that("rexp.perfect mean near 1/rate", {
    x <- iaw$rexp.perfect(1000, rate = 2)
    expect_equal(mean(x), 0.5, tolerance = 0.05)
})

test_that("rexp.perfect rejects rate=0", {
    expect_error(iaw$rexp.perfect(10, rate = 0))
})

# --- mk.random.walk ---
test_that("mk.random.walk returns correct length", {
    w <- iaw$mk.random.walk(100)
    expect_length(w, 100)
})

test_that("mk.random.walk starts at the given start value", {
    w <- iaw$mk.random.walk(10, start = 42)
    expect_equal(w[1], 42)
})

test_that("mk.random.walk step sd matches request", {
    set.seed(7)
    w <- iaw$mk.random.walk(10000, sd = 3)
    expect_equal(sd(diff(w)), 3, tolerance = 0.15)
})

test_that("mk.random.walk rejects n<1", {
    expect_error(iaw$mk.random.walk(0))
})
