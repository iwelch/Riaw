# Tests for statistics functions

# --- cmpsum ---
test_that("cmpsum compounds returns correctly", {
    r <- c(0.1, 0.1)
    expect_equal(iaw$cmpsum(r), prod(1 + r) - 1)
})

test_that("cmpsum with MPY>0 annualizes", {
    r <- c(0.01, 0.02, 0.03)
    result <- iaw$cmpsum(r, MPY = 12)
    expected <- exp(12 / 3 * sum(log(1 + r))) - 1
    expect_equal(result, expected)
})

test_that("cmpsum of empty vector is 0", {
    expect_equal(iaw$cmpsum(numeric(0)), 0)
})

test_that("cmpsum rejects non-numeric", {
    expect_error(iaw$cmpsum(c("a", "b")))
})

# --- geomean ---
test_that("geomean is cmpsum with MPY=1", {
    r <- c(0.05, 0.10, -0.03)
    expect_equal(iaw$geomean(r), iaw$cmpsum(r, MPY = 1))
})

test_that("geomean of constant returns returns that constant", {
    r <- rep(0.05, 10)
    expect_equal(iaw$geomean(r), 0.05)
})

# --- covp ---
test_that("covp returns population covariance", {
    x <- c(1, 2, 3, 4, 5)
    y <- c(2, 4, 6, 8, 10)
    expect_equal(iaw$covp(x, y), cov(x, y) * 4 / 5)
})

test_that("covp with NA removes incomplete pairs", {
    x <- c(1, 2, NA, 4, 5)
    y <- c(2, 4, 6, 8, 10)
    result <- iaw$covp(x, y)
    xc <- c(1, 2, 4, 5)
    yc <- c(2, 4, 8, 10)
    expect_equal(result, cov(xc, yc) * 3 / 4)
})

test_that("covp of single observation is 0", {
    expect_equal(iaw$covp(1, 1), 0)
})

# --- varp ---
test_that("varp returns population variance", {
    x <- c(2, 4, 4, 4, 5, 5, 7, 9)
    expect_equal(iaw$varp(x), var(x) * 7 / 8)
})

test_that("varp of constant vector is 0", {
    expect_equal(iaw$varp(rep(5, 10)), 0)
})

test_that("varp of single element is 0", {
    expect_equal(iaw$varp(7), 0)
})

test_that("varp with all NA returns NA", {
    expect_true(is.na(iaw$varp(c(NA_real_, NA_real_))))
})

# --- sdp ---
test_that("sdp is sqrt of varp", {
    x <- c(2, 4, 4, 4, 5, 5, 7, 9)
    expect_equal(iaw$sdp(x), sqrt(iaw$varp(x)))
})

test_that("sdp of constant is 0", {
    expect_equal(iaw$sdp(rep(3, 20)), 0)
})

# --- normalize ---
test_that("normalize rescales to 0-1", {
    x <- c(10, 20, 30, 40, 50)
    result <- iaw$normalize(x)
    expect_equal(min(result), 0)
    expect_equal(max(result), 1)
})

test_that("normalize of constant vector returns 0.5", {
    result <- iaw$normalize(rep(7, 5))
    expect_true(all(result == 0.5))
})

test_that("normalize preserves NA positions", {
    result <- iaw$normalize(c(1, NA, 3))
    expect_true(is.na(result[2]))
    expect_equal(result[1], 0)
    expect_equal(result[3], 1)
})

# --- standardize ---
test_that("standardize produces mean~0 sd~1", {
    x <- c(10, 20, 30, 40, 50)
    z <- iaw$standardize(x)
    expect_equal(mean(z), 0, tolerance = 1e-10)
    expect_equal(sd(z), 1, tolerance = 1e-10)
})

test_that("standardize rejects non-numeric", {
    expect_error(iaw$standardize(letters))
})

# --- winsorize.level ---
test_that("winsorize.level clips extremes", {
    x <- c(-100, 1, 2, 3, 100)
    expect_equal(iaw$winsorize.level(x, c(0, 10)), c(0, 1, 2, 3, 10))
})

test_that("winsorize.level preserves NA", {
    x <- c(NA, 5, 200)
    result <- iaw$winsorize.level(x, c(0, 10))
    expect_true(is.na(result[1]))
    expect_equal(result[3], 10)
})

test_that("winsorize.level rejects inverted bounds", {
    expect_error(iaw$winsorize.level(1:5, c(10, 0)))
})

# --- winsorize.percentile ---
test_that("winsorize.percentile tames outliers", {
    set.seed(42)
    x <- c(rnorm(98), -100, 100)
    result <- iaw$winsorize.percentile(x, c(0.01, 0.99))
    expect_true(max(result) < 100)
    expect_true(min(result) > -100)
})

test_that("winsorize.percentile preserves NA", {
    x <- c(NA, rnorm(99))
    result <- iaw$winsorize.percentile(x)
    expect_true(is.na(result[1]))
    expect_equal(length(result), 100)
})

# --- sse ---
test_that("sse computes sum of squared errors", {
    actual    <- c(1, 2, 3)
    predicted <- c(1, 2, 3)
    expect_equal(iaw$sse(actual, predicted), 0)
})

test_that("sse with known residuals", {
    actual    <- c(3, 5, 7)
    predicted <- c(2, 4, 6)
    expect_equal(iaw$sse(actual, predicted), 3)
})

test_that("sse rejects length mismatch", {
    expect_error(iaw$sse(1:3, 1:4))
})

# --- sst ---
test_that("sst computes total sum of squares", {
    x <- c(2, 4, 4, 4, 5, 5, 7, 9)
    expect_equal(iaw$sst(x), sum((x - mean(x))^2))
})

test_that("sst of constant is 0", {
    expect_equal(iaw$sst(rep(3, 10)), 0)
})

# --- rank ---
test_that("rank returns standard ranks by default", {
    x <- c(3, 1, 2)
    expect_equal(iaw$rank(x), c(3, 1, 2))
})

test_that("rank with ngroups returns group assignments", {
    x <- 1:100
    result <- iaw$rank(x, ngroups = 5)
    expect_true(all(result %in% 1:5))
})

test_that("rank with ngroups=0 returns quantile ranks in (0,1]", {
    x <- 1:10
    result <- iaw$rank(x, ngroups = 0)
    expect_true(all(result > 0 & result <= 1))
})

test_that("rank preserves NA", {
    x <- c(1, NA, 3)
    result <- iaw$rank(x)
    expect_true(is.na(result[2]))
})

# --- tabularsummary ---
test_that("tabularsummary counts frequencies", {
    x <- c("a", "b", "a", "c", "a")
    result <- iaw$tabularsummary(x)
    expect_s3_class(result, "data.frame")
    expect_true("count" %in% names(result))
    expect_equal(result$count[result$value == "a"], 3)
})

test_that("tabularsummary sorted by default", {
    x <- c("z", "a", "a", "z", "z")
    result <- iaw$tabularsummary(x)
    expect_equal(result$value[1], "z")
})

test_that("tabularsummary unsorted preserves natural order", {
    x <- c("b", "a", "a")
    result <- iaw$tabularsummary(x, sort = FALSE)
    expect_equal(result$value, c("a", "b"))
})

# --- dependent.rank ---
test_that("dependent.rank without groups is plain rank", {
    x <- c(3, 1, 2)
    expect_equal(iaw$dependent.rank(x), rank(x))
})

test_that("dependent.rank ranks within groups", {
    x      <- c(10, 20, 30, 5, 15, 25)
    groups <- c("A","A","A","B","B","B")
    result <- iaw$dependent.rank(x, groups)
    expect_equal(result[1:3], c(1, 2, 3))
    expect_equal(result[4:6], c(1, 2, 3))
})

# --- print.cor ---
test_that("print.cor runs without error on data frame", {
    df <- data.frame(x = 1:10, y = 10:1, z = rnorm(10))
    expect_no_error(invisible(capture.output(iaw$print.cor(df))))
})

test_that("print.cor returns invisible correlation matrix", {
    df <- data.frame(a = 1:20, b = 20:1)
    cm <- invisible(capture.output(result <- iaw$print.cor(df)))
    expect_true(is.matrix(result))
    expect_equal(nrow(result), 2)
})

# --- autocorrel ---
test_that("autocorrel returns correct length", {
    x <- sin(1:100)
    y <- sin(2:101)
    result <- iaw$autocorrel(x, y, leadlags = 3)
    expect_equal(length(result), 7)
})

test_that("autocorrel cor0=1 for identical series", {
    x <- rnorm(200)
    result <- iaw$autocorrel(x, x, leadlags = 2)
    expect_equal(result[["cor0"]], 1, tolerance = 1e-10)
})

test_that("autocorrel rejects non-numeric", {
    expect_error(iaw$autocorrel(letters, 1:26, leadlags = 1))
})

# --- autopcorrel ---
test_that("autopcorrel returns named coefficients", {
    set.seed(1)
    x <- sin(1:50) + rnorm(50, sd = 0.1)
    y <- sin(2:51) + rnorm(50, sd = 0.1)
    result <- iaw$autopcorrel(x, y, around = 2)
    expect_true("pcor0" %in% names(result))
    expect_equal(length(result), 2 * 2 + 1 + 1)
})

test_that("autopcorrel rejects non-numeric series", {
    expect_error(iaw$autopcorrel(letters, 1:26, around = 1))
})
