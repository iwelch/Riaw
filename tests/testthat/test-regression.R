# Tests for ols, ols.nona, olm, printolm, famamacbeth, neweywest, oosreg, fitted, residuals

# ols tests
test_that("iaw$ols returns coefficients", {
    d <- data.frame(y = 1:10, x = 1:10)
    result <- iaw$ols(y ~ x, data = d)
    expect_true(is.numeric(result))
})

test_that("iaw$ols finds perfect fit", {
    d <- data.frame(y = 1:10, x = 1:10)
    result <- iaw$ols(y ~ x, data = d, detail = 2)
    expect_equal(result$rsq, 1, tolerance = 0.001)
})

test_that("iaw$ols includes intercept by default", {
    d <- data.frame(y = rnorm(100), x = rnorm(100))
    result <- iaw$ols(y ~ x, data = d)
    expect_equal(length(result), 2)  # intercept + x
})

test_that("iaw$ols handles no intercept", {
    d <- data.frame(y = 1:10, x = 1:10)
    result <- iaw$ols(y ~ x - 1, data = d)
    expect_equal(length(result), 1)
})

test_that("iaw$ols handles multiple regressors", {
    d <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100))
    result <- iaw$ols(y ~ x1 + x2, data = d)
    expect_equal(length(result), 3)
})

test_that("iaw$ols handles NA values", {
    d <- data.frame(y = c(1, NA, 3, 4, 5), x = 1:5)
    expect_silent(iaw$ols(y ~ x, data = d))
})

test_that("iaw$ols detail 2 returns rsq", {
    d <- data.frame(y = rnorm(50), x = rnorm(50))
    result <- iaw$ols(y ~ x, data = d, detail = 2)
    expect_true("rsq" %in% names(result))
})

# Failing tests
test_that("iaw$ols rejects invalid detail", {
    d <- data.frame(y = 1:10, x = 1:10)
    expect_error(iaw$ols(y ~ x, data = d, detail = 5))
})

test_that("iaw$ols rejects non-numeric detail", {
    d <- data.frame(y = 1:10, x = 1:10)
    expect_error(iaw$ols(y ~ x, data = d, detail = "high"))
})

test_that("iaw$ols rejects invalid formula", {
    d <- data.frame(y = 1:10, x = 1:10)
    expect_error(iaw$ols("not a formula", data = d))
})

# ols.nona tests
test_that("iaw$ols.nona returns coefficients", {
    X <- cbind(1, rnorm(50))
    y <- X %*% c(1, 2) + rnorm(50, sd = 0.1)
    result <- iaw$ols.nona(y, X)
    expect_true(is.numeric(result))
})

test_that("iaw$ols.nona recovers true coefficients", {
    set.seed(123)
    X <- cbind(1, rnorm(1000))
    y <- X %*% c(1, 2) + rnorm(1000, sd = 0.1)
    result <- iaw$ols.nona(y, X)
    expect_equal(result[1], 1, tolerance = 0.1)
    expect_equal(result[2], 2, tolerance = 0.1)
})

test_that("iaw$ols.nona detail 1 returns SE", {
    X <- cbind(1, rnorm(50))
    y <- rnorm(50)
    result <- iaw$ols.nona(y, X, detail = 1)
    expect_true("sigma" %in% names(result))
})

test_that("iaw$ols.nona handles weights", {
    X <- cbind(1, 1:10)
    y <- 1:10
    w <- rep(1, 10)
    expect_silent(iaw$ols.nona(y, X, w = w))
})

test_that("iaw$ols.nona checkna removes NA rows", {
    X <- cbind(1, c(1, NA, 3, 4, 5))
    y <- c(1, 2, 3, 4, 5)
    result <- iaw$ols.nona(y, X, checkna = TRUE)
    expect_true(is.numeric(result))
})

test_that("iaw$ols.nona detail 3 returns rsq and rmse", {
    X <- cbind(1, rnorm(50))
    y <- rnorm(50)
    result <- iaw$ols.nona(y, X, detail = 3)
    expect_true("rsq" %in% names(result))
    expect_true("rmse" %in% names(result))
})

test_that("iaw$ols.nona detail 4 returns residuals", {
    X <- cbind(1, rnorm(50))
    y <- rnorm(50)
    result <- iaw$ols.nona(y, X, detail = 4)
    expect_true("err" %in% names(result))
})

# Failing tests
test_that("iaw$ols.nona rejects non-matrix X", {
    expect_error(iaw$ols.nona(1:10, 1:10))
})

test_that("iaw$ols.nona rejects mismatched dimensions", {
    X <- cbind(1, rnorm(10))
    y <- rnorm(20)
    expect_error(iaw$ols.nona(y, X))
})

test_that("iaw$ols.nona rejects non-numeric y", {
    X <- cbind(1, rnorm(10))
    y <- letters[1:10]
    expect_error(iaw$ols.nona(y, X))
})

# olm tests (requires lmtest and sandwich packages)
test_that("iaw$olm returns summary.lm object", {
    skip_if_not_installed("lmtest")
    skip_if_not_installed("sandwich")
    df <- data.frame(y = rnorm(100), x = rnorm(100))
    result <- iaw$olm(y ~ x, data = df)
    expect_s3_class(result, "summary.lm")
})

test_that("iaw$olm includes Newey-West SE", {
    skip_if_not_installed("lmtest")
    skip_if_not_installed("sandwich")
    df <- data.frame(y = rnorm(100), x = rnorm(100))
    result <- iaw$olm(y ~ x, data = df)
    expect_true(any(grepl("nw", colnames(coef(result)))))
})

test_that("iaw$olm includes standardized coefficients", {
    skip_if_not_installed("lmtest")
    skip_if_not_installed("sandwich")
    df <- data.frame(y = rnorm(100), x = rnorm(100))
    result <- iaw$olm(y ~ x, data = df)
    expect_true("stdcoefs" %in% colnames(coef(result)))
})

test_that("iaw$olm can disable Newey-West", {
    skip_if_not_installed("lmtest")
    skip_if_not_installed("sandwich")
    df <- data.frame(y = rnorm(100), x = rnorm(100))
    result <- iaw$olm(y ~ x, data = df, newey.west = -1)
    expect_false(any(grepl("nw", colnames(coef(result)))))
})

test_that("iaw$olm respects digits parameter", {
    skip_if_not_installed("lmtest")
    skip_if_not_installed("sandwich")
    df <- data.frame(y = rnorm(100), x = rnorm(100))
    result <- iaw$olm(y ~ x, data = df, digits = 2)
    expect_true(is.numeric(coef(result)))
})

test_that("iaw$olm handles multiple regressors", {
    skip_if_not_installed("lmtest")
    skip_if_not_installed("sandwich")
    df <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100))
    result <- iaw$olm(y ~ x1 + x2, data = df)
    expect_equal(nrow(coef(result)), 3)
})

test_that("iaw$olm can keep p-values", {
    skip_if_not_installed("lmtest")
    skip_if_not_installed("sandwich")
    df <- data.frame(y = rnorm(100), x = rnorm(100))
    result <- iaw$olm(y ~ x, data = df, keep.pval = TRUE)
    expect_true(any(grepl("pval", colnames(coef(result)))))
})

# Failing tests
test_that("iaw$olm rejects invalid newey.west", {
    skip_if_not_installed("lmtest")
    skip_if_not_installed("sandwich")
    df <- data.frame(y = rnorm(100), x = rnorm(100))
    expect_error(iaw$olm(y ~ x, data = df, newey.west = "invalid"))
})

test_that("iaw$olm rejects invalid stdcoefs", {
    skip_if_not_installed("lmtest")
    skip_if_not_installed("sandwich")
    df <- data.frame(y = rnorm(100), x = rnorm(100))
    expect_error(iaw$olm(y ~ x, data = df, stdcoefs = "yes"))
})

test_that("iaw$olm rejects vector digits", {
    skip_if_not_installed("lmtest")
    skip_if_not_installed("sandwich")
    df <- data.frame(y = rnorm(100), x = rnorm(100))
    expect_error(iaw$olm(y ~ x, data = df, digits = c(2, 3)))
})

# oosreg tests
test_that("iaw$oosreg returns list with oos_rmse", {
    df <- data.frame(y = rnorm(100), x = rnorm(100))
    result <- iaw$oosreg(y ~ x, data = df)
    expect_true("oos_rmse" %in% names(result))
})

test_that("iaw$oosreg returns model", {
    df <- data.frame(y = rnorm(100), x = rnorm(100))
    result <- iaw$oosreg(y ~ x, data = df)
    expect_true("model" %in% names(result))
})

test_that("iaw$oosreg returns oos_rsq", {
    df <- data.frame(y = rnorm(100), x = rnorm(100))
    result <- iaw$oosreg(y ~ x, data = df)
    expect_true("oos_rsq" %in% names(result))
})

test_that("iaw$oosreg respects train_frac", {
    df <- data.frame(y = 1:100, x = 1:100)
    result <- iaw$oosreg(y ~ x, data = df, train_frac = 0.5)
    expect_true(is.numeric(result$oos_rmse))
})

test_that("iaw$oosreg handles perfect fit", {
    df <- data.frame(y = 1:100, x = 1:100)
    result <- iaw$oosreg(y ~ x, data = df)
    expect_equal(result$oos_rsq, 1, tolerance = 0.01)
})

test_that("iaw$oosreg handles multiple regressors", {
    df <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100))
    result <- iaw$oosreg(y ~ x1 + x2, data = df)
    expect_true(is.numeric(result$oos_rmse))
})

test_that("iaw$oosreg returns numeric oos_rsq", {
    df <- data.frame(y = rnorm(100), x = rnorm(100))
    result <- iaw$oosreg(y ~ x, data = df)
    expect_type(result$oos_rsq, "double")
})

# Failing tests
test_that("iaw$oosreg rejects train_frac >= 1", {
    df <- data.frame(y = rnorm(100), x = rnorm(100))
    expect_error(iaw$oosreg(y ~ x, data = df, train_frac = 1.0))
})

test_that("iaw$oosreg rejects train_frac <= 0", {
    df <- data.frame(y = rnorm(100), x = rnorm(100))
    expect_error(iaw$oosreg(y ~ x, data = df, train_frac = 0))
})

test_that("iaw$oosreg rejects non-data.frame", {
    expect_error(iaw$oosreg(y ~ x, data = "not a df"))
})

# fitted and residuals tests
test_that("iaw$fitted extracts fitted values", {
    model <- lm(mpg ~ wt, data = mtcars)
    result <- iaw$fitted(model)
    expect_equal(length(result), nrow(mtcars))
})

test_that("iaw$residuals extracts residuals", {
    model <- lm(mpg ~ wt, data = mtcars)
    result <- iaw$residuals(model)
    expect_equal(length(result), nrow(mtcars))
})

test_that("iaw$fitted returns numeric", {
    model <- lm(mpg ~ wt, data = mtcars)
    expect_type(iaw$fitted(model), "double")
})

test_that("iaw$residuals returns numeric", {
    model <- lm(mpg ~ wt, data = mtcars)
    expect_type(iaw$residuals(model), "double")
})

test_that("fitted + residuals = y", {
    model <- lm(mpg ~ wt, data = mtcars)
    expect_equal(iaw$fitted(model) + iaw$residuals(model), mtcars$mpg)
})

test_that("residuals sum to approximately zero", {
    model <- lm(mpg ~ wt, data = mtcars)
    expect_equal(sum(iaw$residuals(model)), 0, tolerance = 1e-10)
})

test_that("iaw$fitted works with multiple regressors", {
    model <- lm(mpg ~ wt + hp, data = mtcars)
    expect_equal(length(iaw$fitted(model)), nrow(mtcars))
})
