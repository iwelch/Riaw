# Tests for ols, ols.nona, olm, printolm, famamacbeth, neweywest, oosreg, fitted, residuals

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
    expect_equal( unname(iaw$fitted(model) + iaw$residuals(model)), mtcars$mpg)
})

test_that("residuals sum to approximately zero", {
    model <- lm(mpg ~ wt, data = mtcars)
    expect_equal(sum(iaw$residuals(model)), 0, tolerance = 1e-10)
})

test_that("iaw$fitted works with multiple regressors", {
    model <- lm(mpg ~ wt + hp, data = mtcars)
    expect_equal(length(iaw$fitted(model)), nrow(mtcars))
})
