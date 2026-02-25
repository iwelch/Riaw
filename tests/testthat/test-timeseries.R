# Tests for time-series functions: lagseries, leadseries, chgseries, pchgseries,
# compoundseries, cumulateseries, ma

## ---------------------------------------------------------------------------
## lagseries
## ---------------------------------------------------------------------------

test_that("lagseries shifts values back by 1", {
    expect_equal(iaw$lagseries(c(10, 20, 30, 40, 50)), c(NA, 10, 20, 30, 40))
})

test_that("lagseries with numlags=2 prepends two NAs", {
    expect_equal(iaw$lagseries(1:6, 2), c(NA, NA, 1L, 2L, 3L, 4L))
})

test_that("lagseries with numlags=0 returns input unchanged", {
    x <- c(5, 10, 15)
    expect_equal(iaw$lagseries(x, 0), x)
})

test_that("lagseries negative lag delegates to leadseries", {
    x <- c(1, 2, 3, 4, 5)
    expect_equal(iaw$lagseries(x, -1), iaw$leadseries(x, 1))
})

test_that("lagseries respects panel boundaries", {
    series  <- c(1, 2, 3, 10, 20, 30)
    panelid <- c(1, 1, 1,  2,  2,  2)
    timeid  <- c(1, 2, 3,  1,  2,  3)
    result  <- iaw$lagseries(series, 1, panelid, timeid)
    # first obs in each panel should be NA
    expect_true(is.na(result[1]))
    expect_true(is.na(result[4]))
    expect_equal(result[2], 1)
    expect_equal(result[5], 10)
})

test_that("lagseries rejects character input", {
    expect_error(iaw$lagseries(c("a", "b", "c")))
})

## ---------------------------------------------------------------------------
## leadseries
## ---------------------------------------------------------------------------

test_that("leadseries shifts values forward by 1", {
    expect_equal(iaw$leadseries(c(10, 20, 30, 40, 50)), c(20, 30, 40, 50, NA))
})

test_that("leadseries with numleads=2 appends two NAs", {
    expect_equal(iaw$leadseries(1:6, 2), c(3L, 4L, 5L, 6L, NA, NA))
})

test_that("leadseries with numleads=0 returns input unchanged", {
    x <- c(5, 10, 15)
    expect_equal(iaw$leadseries(x, 0), x)
})

test_that("leadseries negative lead delegates to lagseries", {
    x <- c(1, 2, 3, 4, 5)
    expect_equal(iaw$leadseries(x, -2), iaw$lagseries(x, 2))
})

test_that("leadseries respects panel boundaries", {
    series  <- c(1, 2, 3, 10, 20, 30)
    panelid <- c(1, 1, 1,  2,  2,  2)
    timeid  <- c(1, 2, 3,  1,  2,  3)
    result  <- iaw$leadseries(series, 1, panelid, timeid)
    # last obs in each panel should be NA
    expect_true(is.na(result[3]))
    expect_true(is.na(result[6]))
    expect_equal(result[1], 2)
    expect_equal(result[4], 20)
})

test_that("leadseries rejects non-scalar numleads", {
    expect_error(iaw$leadseries(1:5, c(1, 2)))
})

## ---------------------------------------------------------------------------
## chgseries
## ---------------------------------------------------------------------------

test_that("chgseries computes first difference", {
    x <- c(100, 102, 105, 103, 108)
    expect_equal(iaw$chgseries(x), c(NA, 2, 3, -2, 5))
})

test_that("chgseries with numlags=2 differences by 2 periods", {
    x <- c(100, 102, 105, 103, 108)
    expect_equal(iaw$chgseries(x, 2), c(NA, NA, 5, 1, 3))
})

test_that("chgseries constant series yields zeros (except first NA)", {
    x <- rep(7, 5)
    result <- iaw$chgseries(x)
    expect_true(is.na(result[1]))
    expect_equal(result[2:5], rep(0, 4))
})

test_that("dchgseries alias is identical to chgseries", {
    x <- c(10, 20, 15, 25)
    expect_equal(iaw$dchgseries(x), iaw$chgseries(x))
})

test_that("chgseries rejects length-1 input", {
    expect_error(iaw$chgseries(42))
})

## ---------------------------------------------------------------------------
## pchgseries
## ---------------------------------------------------------------------------

test_that("pchgseries computes percent change", {
    x <- c(100, 110, 99)
    result <- iaw$pchgseries(x)
    expect_true(is.na(result[1]))
    expect_equal(result[2], 0.10)
    expect_equal(result[3], 99/110 - 1)
})

test_that("pchgseries with numlags=2", {
    x <- c(100, 110, 121)
    result <- iaw$pchgseries(x, 2)
    expect_equal(result[3], 0.21)
})

test_that("pchgseries constant series yields zeros", {
    x <- rep(50, 4)
    result <- iaw$pchgseries(x)
    expect_equal(result[2:4], rep(0, 3))
})

test_that("pchgseries rejects non-numeric", {
    expect_error(iaw$pchgseries(letters[1:5]))
})

## ---------------------------------------------------------------------------
## compoundseries
## ---------------------------------------------------------------------------

test_that("compoundseries cumulative compound from start", {
    r <- c(0.10, 0.05, -0.02)
    result <- iaw$compoundseries(r)
    # (1.10)-1, (1.10*1.05)-1, (1.10*1.05*0.98)-1
    expect_equal(result[1], 0.10)
    expect_equal(result[2], 1.10 * 1.05 - 1, tolerance = 1e-10)
    expect_equal(result[3], 1.10 * 1.05 * 0.98 - 1, tolerance = 1e-10)
})

test_that("compoundseries geomean returns annualized geometric mean", {
    r <- c(0.10, 0.10, 0.10)
    result <- iaw$compoundseries(r, geomean = TRUE)
    # geomean at each point: exp(cumlogsum / t) - 1
    expect_equal(result[3], 0.10, tolerance = 1e-10)
})

test_that("compoundseries rolling window", {
    r <- c(0.01, 0.02, 0.03, 0.04)
    result <- iaw$compoundseries(r, window = 2)
    # window=2: compound of last 2 returns at each point
    # At t=3: (1.02)*(1.03)-1; at t=4: (1.03)*(1.04)-1
    expect_equal(result[3], 1.02 * 1.03 - 1, tolerance = 1e-10)
    expect_equal(result[4], 1.03 * 1.04 - 1, tolerance = 1e-10)
})

test_that("compoundseries na.is.zero treats NA as zero return", {
    r <- c(0.05, NA, 0.10)
    result <- iaw$compoundseries(r, na.is.zero = TRUE)
    # NA replaced by 0, so (1.05)*(1.0)*(1.10)-1
    expect_equal(result[3], 1.05 * 1.0 * 1.10 - 1, tolerance = 1e-10)
    # NA positions remain NA in output
    expect_true(is.na(result[2]))
})

test_that("compoundseries errors on NA when na.is.zero=FALSE", {
    expect_error(iaw$compoundseries(c(0.01, NA, 0.02)))
})

## ---------------------------------------------------------------------------
## cumulateseries
## ---------------------------------------------------------------------------

test_that("cumulateseries computes cumulative sum", {
    expect_equal(iaw$cumulateseries(c(1, 2, 3, 4)), c(1, 3, 6, 10))
})

test_that("cumulateseries single element", {
    expect_equal(iaw$cumulateseries(7), 7)
})

test_that("cumulateseries handles negative values", {
    expect_equal(iaw$cumulateseries(c(5, -3, 2)), c(5, 2, 4))
})

test_that("cumulateseries rejects empty input", {
    expect_error(iaw$cumulateseries(numeric(0)))
})

## ---------------------------------------------------------------------------
## ma (moving average)
## ---------------------------------------------------------------------------

test_that("ma right-aligned moving average", {
    x <- c(1, 2, 3, 4, 5)
    result <- iaw$ma(x, 3)
    # right-aligned: result[3] = mean(1,2,3)=2, result[4]=mean(2,3,4)=3
    expect_equal(result[3], 2.0)
    expect_equal(result[4], 3.0)
    expect_true(is.na(result[1]))
    expect_true(is.na(result[2]))
})

test_that("ma center-aligned with around=TRUE", {
    x <- c(1, 2, 3, 4, 5)
    result <- iaw$ma(x, 3, around = TRUE)
    # center-aligned: result[2] = mean(1,2,3)=2, result[4]=mean(3,4,5)=4
    expect_equal(result[2], 2.0)
    expect_equal(result[4], 4.0)
    expect_true(is.na(result[1]))
    expect_true(is.na(result[5]))
})

test_that("ma with window=1 returns original values", {
    x <- c(3.5, 7.2, 1.1)
    result <- iaw$ma(x, 1)
    expect_equal(as.numeric(result), x)
})

test_that("ma rejects window < 1", {
    expect_error(iaw$ma(1:10, 0))
})
