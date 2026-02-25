# Tests for regression functions: olm, residuals, neweywest, ooslm, famamacbeth

# --- olm ---

test_that("olm returns olm class with expected structure", {
    skip_if_not_installed("lmtest")
    skip_if_not_installed("sandwich")
    set.seed(42)
    df <- data.frame(y = rnorm(50), x = rnorm(50))
    result <- iaw$olm(y ~ x, data = df)
    expect_s3_class(result, "olm")
    expect_s3_class(result, "summary.lm")
    cnames <- colnames(coef(result))
    expect_true("coefest" %in% cnames)
    expect_true("T.ols" %in% cnames)
    expect_true(any(grepl("nw", cnames)))
    expect_true(any(grepl("stdcoefs", cnames)))
    expect_true(!is.null(result$r.squared))
    expect_true(!is.null(result$sigma))
})

test_that("olm rejects non-numeric newey.west", {
    skip_if_not_installed("lmtest")
    skip_if_not_installed("sandwich")
    df <- data.frame(y = 1:10, x = 1:10)
    expect_error(iaw$olm(y ~ x, data = df, newey.west = "bad"))
})

test_that("olm handles multiple regressors and includes pctsumsq", {
    skip_if_not_installed("lmtest")
    skip_if_not_installed("sandwich")
    set.seed(1)
    df <- data.frame(y = rnorm(60), x1 = rnorm(60), x2 = rnorm(60))
    result <- iaw$olm(y ~ x1 + x2, data = df)
    expect_equal(nrow(coef(result)), 3)
    expect_true("pctsumsq" %in% colnames(coef(result)))
})

test_that("olm can disable Newey-West with negative lag", {
    skip_if_not_installed("lmtest")
    skip_if_not_installed("sandwich")
    set.seed(2)
    df <- data.frame(y = rnorm(30), x = rnorm(30))
    result <- iaw$olm(y ~ x, data = df, newey.west = -1)
    expect_false(any(grepl("nw", colnames(coef(result)))))
})

test_that("olm keep.pval adds fourth standard column", {
    skip_if_not_installed("lmtest")
    skip_if_not_installed("sandwich")
    set.seed(3)
    df <- data.frame(y = rnorm(40), x = rnorm(40))
    result <- iaw$olm(y ~ x, data = df, keep.pval = TRUE)
    expect_true(ncol(coef(result)) > 3)
})

# --- residuals ---

test_that("residuals extracts correct-length vector from lm model", {
    model <- lm(mpg ~ wt, data = mtcars, na.action = na.exclude)
    result <- iaw$residuals(model)
    expect_type(result, "double")
    expect_equal(length(result), nrow(mtcars))
    expect_equal(sum(result), 0, tolerance = 1e-10)
})

test_that("residuals warns when na.exclude not used", {
    model <- lm(mpg ~ wt, data = mtcars)
    expect_message(iaw$residuals(model), "na\\.exclude")
})

# --- neweywest ---

test_that("neweywest returns covariance matrix", {
    skip_if_not_installed("sandwich")
    set.seed(10)
    df <- data.frame(y = rnorm(50), x = rnorm(50))
    model <- lm(y ~ x, data = df)
    vcov <- iaw$neweywest(model, lag = 2)
    expect_true(is.matrix(vcov))
    expect_equal(nrow(vcov), 2)
    expect_equal(ncol(vcov), 2)
    # Diagonal entries should be positive (variances)
    expect_true(all(diag(vcov) > 0))
})

test_that("neweywest rejects non-lm object", {
    skip_if_not_installed("sandwich")
    expect_error(iaw$neweywest("not a model", lag = 0))
})

# --- ooslm ---

test_that("ooslm returns data frame of correct size", {
    skip_if_not_installed("strucchange")
    set.seed(20)
    N <- 30
    df <- data.frame(y = cumsum(rnorm(N)), x = 1:N)
    result <- iaw$ooslm(y ~ x, df)
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), N)
    expect_true(all(c("delta.abserr", "uncondesterr", "condesterr", "to.predict") %in% names(result)))
})

test_that("ooslm has NAs for initial rows where recursion cannot run", {
    skip_if_not_installed("strucchange")
    set.seed(21)
    N <- 20
    df <- data.frame(y = rnorm(N), x = rnorm(N))
    result <- iaw$ooslm(y ~ x, df)
    # First row of uncondesterr is always NA; first K rows of condesterr are NA
    expect_true(is.na(result$uncondesterr[1]))
    expect_true(is.na(result$condesterr[1]))
})

# --- famamacbeth ---

test_that("famamacbeth returns summary matrix", {
    set.seed(30)
    panel <- data.frame(
        ret    = rnorm(200),
        beta   = rnorm(200),
        yyyymm = rep(1:20, each = 10)
    )
    result <- suppressMessages(iaw$famamacbeth(ret ~ beta, data = panel,
                                                timeid = "yyyymm", printn = FALSE))
    expect_true(is.matrix(result) || is.data.frame(result))
    # Should have rows for timeid, df, intercept, beta
    expect_true(nrow(result) >= 2)
})

test_that("famamacbeth.gammas returns per-period coefficients", {
    set.seed(31)
    panel <- data.frame(
        ret    = rnorm(100),
        beta   = rnorm(100),
        yyyymm = rep(1:10, each = 10)
    )
    gs <- iaw$famamacbeth.gammas(ret ~ beta, data = panel, timeid = "yyyymm")
    expect_s3_class(gs, "data.frame")
    expect_equal(nrow(gs), 10)
})

test_that("famamacbeth rejects missing timeid column", {
    df <- data.frame(ret = 1:10, beta = 1:10)
    expect_error(iaw$famamacbeth(ret ~ beta, data = df, timeid = "yyyymm"))
})
