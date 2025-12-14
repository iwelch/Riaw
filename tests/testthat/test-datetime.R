# Tests for datetime functions: epoch2POSIXct, epoch2yyyymmdd, epoch2nyc, yyyymmdd2int, diffdays, tmdiffsec

# epoch2POSIXct tests
test_that("iaw$epoch2POSIXct converts correctly", {
    result <- iaw$epoch2POSIXct(0)
    expect_equal(as.numeric(result), 0)
})

test_that("iaw$epoch2POSIXct returns POSIXct", {
    result <- iaw$epoch2POSIXct(1609459200)
    expect_s3_class(result, "POSIXct")
})

test_that("iaw$epoch2POSIXct handles vector", {
    result <- iaw$epoch2POSIXct(c(0, 86400))
    expect_length(result, 2)
})

test_that("iaw$epoch2POSIXct respects timezone", {
    result <- iaw$epoch2POSIXct(0, tz = "UTC")
    expect_equal(attr(result, "tzone"), "UTC")
})

test_that("iaw$epoch2POSIXct known date", {
    # 2021-01-01 00:00:00 UTC
    result <- iaw$epoch2POSIXct(1609459200, tz = "UTC")
    expect_match(format(result, "%Y-%m-%d"), "2021-01-01")
})

test_that("iaw$epoch2POSIXct handles negative epoch", {
    result <- iaw$epoch2POSIXct(-86400)  # 1969-12-31
    expect_s3_class(result, "POSIXct")
})

test_that("iaw$epoch2POSIXct default is UTC", {
    result <- iaw$epoch2POSIXct(0)
    expect_equal(attr(result, "tzone"), "UTC")
})

# Failing tests
test_that("iaw$epoch2POSIXct rejects non-numeric", {
    expect_error(iaw$epoch2POSIXct("not a number"))
})

test_that("iaw$epoch2POSIXct rejects character", {
    expect_error(iaw$epoch2POSIXct("1609459200"))
})

test_that("iaw$epoch2POSIXct rejects NULL", {
    expect_error(iaw$epoch2POSIXct(NULL))
})

# epoch2yyyymmdd tests
test_that("iaw$epoch2yyyymmdd converts correctly", {
    result <- iaw$epoch2yyyymmdd(1609459200)
    expect_equal(result, 20210101L)
})

test_that("iaw$epoch2yyyymmdd returns integer", {
    result <- iaw$epoch2yyyymmdd(1609459200)
    expect_type(result, "integer")
})

test_that("iaw$epoch2yyyymmdd handles vector", {
    result <- iaw$epoch2yyyymmdd(c(1609459200, 1609545600))
    expect_length(result, 2)
})

test_that("iaw$epoch2yyyymmdd respects timezone", {
    result <- iaw$epoch2yyyymmdd(1609459200, tz = "UTC")
    expect_equal(result, 20210101L)
})

test_that("iaw$epoch2yyyymmdd correct format", {
    result <- iaw$epoch2yyyymmdd(1609459200)
    expect_true(result > 20000000)  # After year 2000
    expect_true(result < 30000000)  # Before year 3000
})

test_that("iaw$epoch2yyyymmdd handles epoch 0", {
    result <- iaw$epoch2yyyymmdd(0)
    expect_equal(result, 19700101L)
})

test_that("iaw$epoch2yyyymmdd 8 digits", {
    result <- iaw$epoch2yyyymmdd(1609459200)
    expect_equal(nchar(as.character(result)), 8)
})

# Failing tests
test_that("iaw$epoch2yyyymmdd rejects non-numeric", {
    expect_error(iaw$epoch2yyyymmdd("not a number"))
})

test_that("iaw$epoch2yyyymmdd rejects character", {
    expect_error(iaw$epoch2yyyymmdd("1609459200"))
})

test_that("iaw$epoch2yyyymmdd rejects NULL", {
    expect_error(iaw$epoch2yyyymmdd(NULL))
})

# epoch2nyc tests
test_that("iaw$epoch2nyc returns POSIXct", {
    result <- iaw$epoch2nyc(1609459200)
    expect_s3_class(result, "POSIXct")
})

test_that("iaw$epoch2nyc uses NYC timezone", {
    result <- iaw$epoch2nyc(1609459200)
    expect_equal(attr(result, "tzone"), "America/New_York")
})

test_that("iaw$epoch2nyc handles vector", {
    result <- iaw$epoch2nyc(c(0, 86400))
    expect_length(result, 2)
})

test_that("iaw$epoch2nyc different from UTC", {
    utc <- iaw$epoch2POSIXct(1609459200, tz = "UTC")
    nyc <- iaw$epoch2nyc(1609459200)
    # Hour should differ
    expect_true(format(utc, "%H") != format(nyc, "%H") || format(utc, "%d") != format(nyc, "%d"))
})

test_that("iaw$epoch2nyc converts epoch 0", {
    result <- iaw$epoch2nyc(0)
    expect_s3_class(result, "POSIXct")
})

test_that("iaw$epoch2nyc handles negative epoch", {
    result <- iaw$epoch2nyc(-86400)
    expect_s3_class(result, "POSIXct")
})

test_that("iaw$epoch2nyc correct class", {
    result <- iaw$epoch2nyc(1609459200)
    expect_true(inherits(result, "POSIXct"))
})

# yyyymmdd2int tests
test_that("iaw$yyyymmdd2int converts Date", {
    result <- iaw$yyyymmdd2int(as.Date("2021-01-15"))
    expect_equal(result, 20210115L)
})

test_that("iaw$yyyymmdd2int returns integer", {
    result <- iaw$yyyymmdd2int(as.Date("2021-01-15"))
    expect_type(result, "integer")
})

test_that("iaw$yyyymmdd2int handles character date", {
    result <- iaw$yyyymmdd2int("2021-01-15")
    expect_equal(result, 20210115L)
})

test_that("iaw$yyyymmdd2int handles vector", {
    result <- iaw$yyyymmdd2int(c(as.Date("2021-01-01"), as.Date("2021-12-31")))
    expect_length(result, 2)
})

test_that("iaw$yyyymmdd2int correct format", {
    result <- iaw$yyyymmdd2int(as.Date("2021-06-15"))
    expect_equal(nchar(as.character(result)), 8)
})

test_that("iaw$yyyymmdd2int alias works", {
    result <- iaw$yyyymmdd.to.int(as.Date("2021-01-15"))
    expect_equal(result, 20210115L)
})

test_that("iaw$yyyymmdd2int preserves day", {
    result <- iaw$yyyymmdd2int(as.Date("2021-01-31"))
    expect_equal(result %% 100, 31)
})

# Failing tests
test_that("iaw$yyyymmdd2int rejects numeric", {
    expect_error(iaw$yyyymmdd2int(20210115))
})

test_that("iaw$yyyymmdd2int rejects invalid date string", {
    expect_error(iaw$yyyymmdd2int("not a date"))
})

test_that("iaw$yyyymmdd2int rejects NULL", {
    expect_error(iaw$yyyymmdd2int(NULL))
})

# diffdays tests
test_that("iaw$diffdays calculates difference", {
    d1 <- as.Date("2021-01-01")
    d2 <- as.Date("2021-01-15")
    result <- iaw$diffdays(d1, d2)
    expect_equal(result, 14)
})

test_that("iaw$diffdays returns numeric", {
    d1 <- as.Date("2021-01-01")
    d2 <- as.Date("2021-01-02")
    expect_type(iaw$diffdays(d1, d2), "double")
})

test_that("iaw$diffdays handles negative", {
    d1 <- as.Date("2021-01-15")
    d2 <- as.Date("2021-01-01")
    result <- iaw$diffdays(d1, d2)
    expect_equal(result, -14)
})

test_that("iaw$diffdays same date is zero", {
    d <- as.Date("2021-01-01")
    result <- iaw$diffdays(d, d)
    expect_equal(result, 0)
})

test_that("iaw$diffdays one day", {
    d1 <- as.Date("2021-01-01")
    d2 <- as.Date("2021-01-02")
    result <- iaw$diffdays(d1, d2)
    expect_equal(result, 1)
})

test_that("iaw$diffdays year difference", {
    d1 <- as.Date("2021-01-01")
    d2 <- as.Date("2022-01-01")
    result <- iaw$diffdays(d1, d2)
    expect_equal(result, 365)
})

test_that("iaw$diffdays handles POSIXct", {
    t1 <- as.POSIXct("2021-01-01")
    t2 <- as.POSIXct("2021-01-02")
    result <- iaw$diffdays(t1, t2)
    expect_equal(result, 1)
})

# tmdiffsec tests
test_that("iaw$tmdiffsec calculates seconds", {
    t1 <- as.POSIXct("2021-01-01 00:00:00")
    t2 <- as.POSIXct("2021-01-01 00:01:00")
    result <- iaw$tmdiffsec(t1, t2)
    expect_equal(result, 60)
})

test_that("iaw$tmdiffsec returns numeric", {
    t1 <- as.POSIXct("2021-01-01 00:00:00")
    t2 <- as.POSIXct("2021-01-01 00:00:01")
    expect_type(iaw$tmdiffsec(t1, t2), "double")
})

test_that("iaw$tmdiffsec handles negative", {
    t1 <- as.POSIXct("2021-01-01 00:01:00")
    t2 <- as.POSIXct("2021-01-01 00:00:00")
    result <- iaw$tmdiffsec(t1, t2)
    expect_equal(result, -60)
})

test_that("iaw$tmdiffsec same time is zero", {
    t <- as.POSIXct("2021-01-01 00:00:00")
    result <- iaw$tmdiffsec(t, t)
    expect_equal(result, 0)
})

test_that("iaw$tmdiffsec one hour", {
    t1 <- as.POSIXct("2021-01-01 00:00:00")
    t2 <- as.POSIXct("2021-01-01 01:00:00")
    result <- iaw$tmdiffsec(t1, t2)
    expect_equal(result, 3600)
})

test_that("iaw$tmdiffsec one day in seconds", {
    t1 <- as.POSIXct("2021-01-01 00:00:00")
    t2 <- as.POSIXct("2021-01-02 00:00:00")
    result <- iaw$tmdiffsec(t1, t2)
    expect_equal(result, 86400)
})

test_that("iaw$tmdiffsec precise to second", {
    t1 <- as.POSIXct("2021-01-01 00:00:00")
    t2 <- as.POSIXct("2021-01-01 00:00:05")
    result <- iaw$tmdiffsec(t1, t2)
    expect_equal(result, 5)
})
