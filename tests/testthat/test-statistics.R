# Tests for summary, autocorrel, autopcorrel, winsorize, pctrank, rank, etc.

# summary tests
test_that("iaw$summary returns matrix", {
    df <- data.frame(x = rnorm(100), y = rnorm(100))
    result <- iaw$summary(df, "p")
    expect_true(is.matrix(result))
})

test_that("iaw$summary includes mean", {
    df <- data.frame(x = rnorm(100))
    result <- iaw$summary(df, "p")
    expect_true("mean" %in% colnames(result))
})

test_that("iaw$summary includes sd", {
    df <- data.frame(x = rnorm(100))
    result <- iaw$summary(df, "p")
    expect_true("sd" %in% colnames(result))
})

test_that("iaw$summary handles multiple columns", {
    df <- data.frame(x = rnorm(100), y = rnorm(100), z = rnorm(100))
    result <- iaw$summary(df, "p")
    expect_equal(nrow(result), 3)
})

test_that("iaw$summary X profile includes auto", {
    df <- data.frame(x = rnorm(100))
    result <- iaw$summary(df, "X")
    expect_true("auto" %in% colnames(result))
})

test_that("iaw$summary sr profile includes sharpe", {
    df <- data.frame(x = rnorm(100))
    result <- iaw$summary(df, "sr")
    expect_true("sharpe" %in% colnames(result))
})

test_that("iaw$summary handles vector input", {
    x <- rnorm(100)
    result <- iaw$summary(x, "p")
    expect_true(is.matrix(result))
})

# Failing tests
test_that("iaw$summary rejects invalid profile", {
    df <- data.frame(x = rnorm(100))
    expect_error(iaw$summary(df, "invalid"))
})

test_that("iaw$summary rejects non-numeric data", {
    df <- data.frame(x = letters[1:10])
    expect_error(iaw$summary(df, "p"))
})

test_that("iaw$summary rejects empty data frame", {
    df <- data.frame()
    expect_error(iaw$summary(df, "p"))
})

# autocorrel tests
test_that("iaw$autocorrel returns named vector", {
    x <- sin(1:100)
    y <- sin(2:101)
    result <- iaw$autocorrel(x, y, leadlags = 3)
    expect_true(is.numeric(result))
    expect_true(!is.null(names(result)))
})

test_that("iaw$autocorrel has correct length", {
    x <- rnorm(100)
    y <- rnorm(100)
    result <- iaw$autocorrel(x, y, leadlags = 5)
    expect_equal(length(result), 11)  # -5 to +5
})

test_that("iaw$autocorrel highest at lag 0 for identical series", {
    x <- rnorm(100)
    result <- iaw$autocorrel(x, x, leadlags = 3)
    expect_equal(which.max(unname(result)), 4)  # cor0 position
})

test_that("iaw$autocorrel detects shift", {
    x <- sin(1:100)
    y <- sin(2:101)  # y leads x
    result <- iaw$autocorrel(x, y, leadlags = 3)
    expect_true(result["cor-1"] > result["cor1"])
})

test_that("iaw$autocorrel values in [-1, 1]", {
    x <- rnorm(100)
    y <- rnorm(100)
    result <- iaw$autocorrel(x, y, leadlags = 3)
    expect_true(all(result >= -1 & result <= 1))
})

test_that("iaw$autocorrel handles NA", {
    x <- c(rnorm(50), NA, rnorm(49))
    y <- rnorm(100)
    result <- iaw$autocorrel(x, y, leadlags = 2)
    expect_true(is.numeric(result))
})

test_that("iaw$autocorrel returns correlation values", {
    x <- 1:100
    y <- 1:100
    result <- iaw$autocorrel(x, y, leadlags = 1)
    expect_equal(result[["cor0"]], 1)
})

# Failing tests
test_that("iaw$autocorrel rejects non-numeric x", {
    expect_error(iaw$autocorrel(letters, rnorm(26), leadlags = 3))
})

test_that("iaw$autocorrel rejects length 1 vector", {
    expect_error(iaw$autocorrel(5, 5, leadlags = 3))
})

test_that("iaw$autocorrel rejects non-numeric leadlags", {
    expect_error(iaw$autocorrel(rnorm(100), rnorm(100), leadlags = "five"))
})

# winsorize.level tests
test_that("iaw$winsorize.level clips values", {
    x <- c(-100, 1, 2, 3, 100)
    result <- iaw$winsorize.level(x, c(0, 10))
    expect_equal(result, c(0, 1, 2, 3, 10))
})

test_that("iaw$winsorize.level preserves middle values", {
    x <- c(1, 2, 3, 4, 5)
    result <- iaw$winsorize.level(x, c(0, 10))
    expect_equal(result, x)
})

test_that("iaw$winsorize.level handles range vector", {
    x <- c(-100, 5, 100)
    result <- iaw$winsorize.level(x, c(0, 10))
    expect_equal(result, c(0, 5, 10))
})

test_that("iaw$winsorize.level handles NA", {
    x <- c(NA, 1, 100)
    result <- iaw$winsorize.level(x, c(0, 10))
    expect_true(is.na(result[1]))
})

test_that("iaw$winsorize.level preserves length", {
    x <- rnorm(100)
    result <- iaw$winsorize.level(x, c(-2, 2))
    expect_equal(length(result), 100)
})

test_that("iaw$winsorize.level handles negative bounds", {
    x <- c(-5, 0, 5)
    result <- iaw$winsorize.level(x, c(-3, 3))
    expect_equal(result, c(-3, 0, 3))
})

test_that("iaw$winsorize.level returns numeric", {
    x <- c(-100, 0, 100)
    expect_type(iaw$winsorize.level(x, c(-10, 10)), "double")
})

# Failing tests
test_that("iaw$winsorize.level rejects non-numeric x", {
    expect_error(iaw$winsorize.level(c("a", "b"), c(0, 10)))
})

test_that("iaw$winsorize.level rejects missing xmax", {
    expect_error(iaw$winsorize.level(1:10, 0))
})

test_that("iaw$winsorize.level rejects inverted bounds", {
    expect_error(iaw$winsorize.level(1:10, c(10, 0)))
})

# winsorize.percentile tests
test_that("iaw$winsorize.percentile clips at percentiles", {
    set.seed(123)
    x <- c(rnorm(98), -100, 100)
    result <- iaw$winsorize.percentile(x, c(0.01, 0.99))
    expect_true(max(result) < (100))
    expect_true(min(result) > (-100))
})

test_that("iaw$winsorize.percentile preserves length", {
    x <- rnorm(100)
    result <- iaw$winsorize.percentile(x)
    expect_equal(length(result), 100)
})

test_that("iaw$winsorize.percentile handles range vector", {
    x <- rnorm(100)
    result <- iaw$winsorize.percentile(x, c(0.05, 0.95))
    expect_equal(length(result), 100)
})

test_that("iaw$winsorize.percentile handles NA", {
    x <- c(NA, rnorm(99))
    result <- iaw$winsorize.percentile(x, c(0.05, 0.95))
    expect_true(is.na(result[1]))
})

test_that("iaw$winsorize.percentile clips extreme values", {
    x <- c(-1000, rnorm(98), 1000)
    result <- iaw$winsorize.percentile(x, c(0.01, 0.99))
    expect_true((result[1]) > (-1000))
    expect_true((result[100]) < 1000)
})

test_that("iaw$winsorize.percentile returns numeric", {
    x <- rnorm(100)
    expect_type(iaw$winsorize.percentile(x), "double")
})

test_that("iaw$winsorize.percentile default is 1%/99%", {
    x <- rnorm(100)
    expect_silent(iaw$winsorize.percentile(x))
})

# Failing tests
test_that("iaw$winsorize.percentile rejects non-numeric", {
    expect_error(iaw$winsorize.percentile(letters))
})

test_that("iaw$winsorize.percentile rejects inverted percentiles", {
    expect_error(iaw$winsorize.percentile(rnorm(100), c(0.99, 0.01)))
})

test_that("iaw$winsorize.percentile rejects extreme percentiles", {
    expect_error(iaw$winsorize.percentile(rnorm(100), c(0.99, 0.999)))
})

# pctrank tests
test_that("iaw$pctrank returns values in [0,1]", {
    x <- rnorm(100)
    result <- iaw$pctrank(x)
    expect_true(all(result >= 0 & result <= 1, na.rm = TRUE))
})

test_that("iaw$pctrank preserves length", {
    x <- rnorm(100)
    expect_equal(length(iaw$pctrank(x)), 100)
})

test_that("iaw$pctrank max value is 1", {
    x <- 1:10
    result <- iaw$pctrank(x)
    expect_equal(max(result), 1)
})

test_that("iaw$pctrank min value is 0", {
    x <- 1:10
    result <- iaw$pctrank(x)
    expect_equal(min(result), 0)
})

test_that("iaw$pctrank handles NA", {
    x <- c(1, NA, 3, 4, 5)
    result <- iaw$pctrank(x)
    expect_true(is.na(result[2]))
})

test_that("iaw$pctrank handles ties", {
    x <- c(1, 1, 2, 3, 3)
    result <- iaw$pctrank(x)
    expect_equal(result[1], result[2])
})

test_that("iaw$pctrank returns numeric", {
    expect_type(iaw$pctrank(1:10), "double")
})

# Failing tests
test_that("iaw$pctrank rejects non-numeric", {
    expect_error(iaw$pctrank(letters))
})

test_that("iaw$pctrank rejects character", {
    expect_error(iaw$pctrank(c("a", "b", "c")))
})

test_that("iaw$pctrank rejects list", {
    expect_error(iaw$pctrank(list(1, 2, 3)))
})

# cmpsum tests
test_that("iaw$cmpsum compounds returns", {
    r <- c(0.1, 0.1)
    expect_equal(iaw$cmpsum(r), 0.21)
})

test_that("iaw$cmpsum handles zero return", {
    r <- c(0.1, 0)
    expect_equal(iaw$cmpsum(r), 0.1)
})

test_that("iaw$cmpsum handles negative returns", {
    r <- c(0.1, -0.1)
    result <- iaw$cmpsum(r)
    expect_true(result < 0.1)
})

test_that("iaw$cmpsum single value", {
    expect_equal(iaw$cmpsum(0.1), 0.1)
})

test_that("iaw$cmpsum empty vector", {
    expect_equal(iaw$cmpsum(numeric(0)), 0)
})

test_that("iaw$cmpsum returns numeric", {
    expect_type(iaw$cmpsum(c(0.1, 0.2)), "double")
})

test_that("iaw$cmpsum handles small returns", {
    r <- c(0.001, 0.001, 0.001)
    result <- iaw$cmpsum(r)
    expect_true(result > 0.003)
})

# Failing tests
test_that("iaw$cmpsum rejects non-numeric", {
    expect_error(iaw$cmpsum(c("a", "b")))
})

test_that("iaw$cmpsum rejects character", {
    expect_error(iaw$cmpsum("0.1"))
})

test_that("iaw$cmpsum rejects list", {
    expect_error(iaw$cmpsum(list(0.1, 0.2)))
})
