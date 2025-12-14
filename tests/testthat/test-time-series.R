# Tests for lagseries, leadseries, chgseries, pchgseries, compoundseries, cumulateseries, ma

# lagseries tests
test_that("iaw$lagseries shifts values correctly", {
    x <- c(1, 2, 3, 4, 5)
    result <- iaw$lagseries(x, 1)
    expect_equal(result, c(NA, 1, 2, 3, 4))
})

test_that("iaw$lagseries handles lag of 2", {
    x <- c(1, 2, 3, 4, 5)
    result <- iaw$lagseries(x, 2)
    expect_equal(result, c(NA, NA, 1, 2, 3))
})

test_that("iaw$lagseries with lag 0 returns original", {
    x <- c(1, 2, 3, 4, 5)
    result <- iaw$lagseries(x, 0)
    expect_equal(result, x)
})

test_that("iaw$lagseries preserves length", {
    x <- 1:10
    expect_equal(length(iaw$lagseries(x, 3)), length(x))
})

test_that("iaw$lagseries handles NA in series", {
    x <- c(1, NA, 3, 4, 5)
    result <- iaw$lagseries(x, 1)
    expect_equal(result[2], 1)
})

test_that("iaw$lagseries negative lag calls leadseries", {
    x <- c(1, 2, 3, 4, 5)
    result <- iaw$lagseries(x, -1)
    expect_equal(result, c(2, 3, 4, 5, NA))
})

test_that("iaw$lagseries returns numeric", {
    expect_type(iaw$lagseries(1:5, 1), "integer")
})

# Failing tests
test_that("iaw$lagseries rejects non-numeric series", {
    expect_error(iaw$lagseries(c("a", "b", "c"), 1))
})

test_that("iaw$lagseries rejects non-numeric lag", {
    expect_error(iaw$lagseries(1:5, "one"))
})

test_that("iaw$lagseries rejects vector lag", {
    expect_error(iaw$lagseries(1:5, c(1, 2)))
})

# leadseries tests
test_that("iaw$leadseries shifts values correctly", {
    x <- c(1, 2, 3, 4, 5)
    result <- iaw$leadseries(x, 1)
    expect_equal(result, c(2, 3, 4, 5, NA))
})

test_that("iaw$leadseries handles lead of 2", {
    x <- c(1, 2, 3, 4, 5)
    result <- iaw$leadseries(x, 2)
    expect_equal(result, c(3, 4, 5, NA, NA))
})

test_that("iaw$leadseries with lead 0 returns original", {
    x <- c(1, 2, 3, 4, 5)
    result <- iaw$leadseries(x, 0)
    expect_equal(result, x)
})

test_that("iaw$leadseries preserves length", {
    x <- 1:10
    expect_equal(length(iaw$leadseries(x, 3)), length(x))
})

test_that("iaw$leadseries negative lead calls lagseries", {
    x <- c(1, 2, 3, 4, 5)
    result <- iaw$leadseries(x, -1)
    expect_equal(result, c(NA, 1, 2, 3, 4))
})

test_that("iaw$leadseries handles NA", {
    x <- c(1, NA, 3, 4, 5)
    result <- iaw$leadseries(x, 1)
    expect_true(is.na(result[1]) || result[1] == x[2])
})

test_that("iaw$leadseries returns numeric", {
    expect_type(iaw$leadseries(1:5, 1), "integer")
})

# Failing tests
test_that("iaw$leadseries rejects non-numeric", {
    expect_error(iaw$leadseries(c("a", "b", "c"), 1))
})

test_that("iaw$leadseries rejects non-numeric lead", {
    expect_error(iaw$leadseries(1:5, "one"))
})

test_that("iaw$leadseries rejects vector lead", {
    expect_error(iaw$leadseries(1:5, c(1, 2)))
})

# chgseries tests
test_that("iaw$chgseries computes first difference", {
    x <- c(100, 102, 105, 103, 108)
    result <- iaw$chgseries(x)
    expect_equal(result, c(NA, 2, 3, -2, 5))
})

test_that("iaw$chgseries handles lag of 2", {
    x <- c(100, 102, 105, 103, 108)
    result <- iaw$chgseries(x, 2)
    expect_equal(result, c(NA, NA, 5, 1, 3))
})

test_that("iaw$chgseries preserves length", {
    x <- 1:10
    expect_equal(length(iaw$chgseries(x)), length(x))
})

test_that("iaw$chgseries first value is NA", {
    x <- 1:5
    expect_true(is.na(iaw$chgseries(x)[1]))
})

test_that("iaw$chgseries handles constant series", {
    x <- rep(5, 5)
    result <- iaw$chgseries(x)
    expect_equal(result[2:5], rep(0, 4))
})

test_that("iaw$chgseries alias dchgseries works", {
    x <- 1:5
    expect_equal(iaw$dchgseries(x), iaw$chgseries(x))
})

test_that("iaw$chgseries returns numeric", {
    expect_type(iaw$chgseries(c(1.0, 2.0, 3.0)), "double")
})

# Failing tests
test_that("iaw$chgseries rejects non-numeric", {
    expect_error(iaw$chgseries(c("a", "b", "c")))
})

test_that("iaw$chgseries rejects length 1", {
    expect_error(iaw$chgseries(5))
})

test_that("iaw$chgseries rejects character series", {
    expect_error(iaw$chgseries(letters[1:5]))
})

# pchgseries tests
test_that("iaw$pchgseries computes percent change", {
    x <- c(100, 105, 103, 110)
    result <- iaw$pchgseries(x)
    expect_equal(result[2], 0.05)
})

test_that("iaw$pchgseries first value is NA", {
    x <- c(100, 110)
    expect_true(is.na(iaw$pchgseries(x)[1]))
})

test_that("iaw$pchgseries handles negative returns", {
    x <- c(100, 90)
    expect_equal(iaw$pchgseries(x)[2], -0.1)
})

test_that("iaw$pchgseries preserves length", {
    x <- 1:10
    expect_equal(length(iaw$pchgseries(x)), length(x))
})

test_that("iaw$pchgseries with lag 2", {
    x <- c(100, 110, 121)
    result <- iaw$pchgseries(x, 2)
    expect_equal(result[3], 0.21)
})

test_that("iaw$pchgseries handles constant series", {
    x <- rep(100, 5)
    result <- iaw$pchgseries(x)
    expect_equal(result[2:5], rep(0, 4))
})

test_that("iaw$pchgseries returns numeric", {
    expect_type(iaw$pchgseries(c(100, 110, 120)), "double")
})

# Failing tests
test_that("iaw$pchgseries rejects non-numeric", {
    expect_error(iaw$pchgseries(c("a", "b", "c")))
})

test_that("iaw$pchgseries rejects length 1", {
    expect_error(iaw$pchgseries(100))
})

test_that("iaw$pchgseries rejects character", {
    expect_error(iaw$pchgseries(letters))
})

# cumulateseries tests
test_that("iaw$cumulateseries computes cumsum", {
    x <- c(1, 2, 3, 4, 5)
    expect_equal(iaw$cumulateseries(x), c(1, 3, 6, 10, 15))
})

test_that("iaw$cumulateseries handles single value", {
    expect_equal(iaw$cumulateseries(5), 5)
})

test_that("iaw$cumulateseries handles negative values", {
    x <- c(1, -2, 3, -4, 5)
    expect_equal(iaw$cumulateseries(x), c(1, -1, 2, -2, 3))
})

test_that("iaw$cumulateseries preserves length", {
    x <- 1:100
    expect_equal(length(iaw$cumulateseries(x)), 100)
})

test_that("iaw$cumulateseries handles zeros", {
    x <- c(0, 0, 1, 0, 2)
    expect_equal(iaw$cumulateseries(x), c(0, 0, 1, 1, 3))
})

test_that("iaw$cumulateseries handles decimals", {
    x <- c(0.1, 0.2, 0.3)
    result <- iaw$cumulateseries(x)
    expect_equal(result[3], 0.6, tolerance = 1e-10)
})

test_that("iaw$cumulateseries returns numeric", {
    expect_type(iaw$cumulateseries(1:5), "integer")
})

# Failing tests
test_that("iaw$cumulateseries rejects non-numeric", {
    expect_error(iaw$cumulateseries(c("a", "b")))
})

test_that("iaw$cumulateseries rejects empty vector", {
    expect_error(iaw$cumulateseries(numeric(0)))
})

test_that("iaw$cumulateseries rejects character", {
    expect_error(iaw$cumulateseries(letters))
})

# ma (moving average) tests
test_that("iaw$ma computes moving average", {
    x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    result <- iaw$ma(x, 3)
    expect_equal(result[3], 2, tolerance = 0.01)
})

test_that("iaw$ma with window 1 returns original", {
    x <- 1:5
    result <- iaw$ma(x, 1)
    expect_equal(as.numeric(result), as.numeric(x))
})

test_that("iaw$ma preserves length", {
    x <- 1:20
    expect_equal(length(iaw$ma(x, 5)), length(x))
})

test_that("iaw$ma first values are NA", {
    x <- 1:10
    result <- iaw$ma(x, 3)
    expect_true(is.na(result[1]))
    expect_true(is.na(result[2]))
})

test_that("iaw$ma handles constant series", {
    x <- rep(5, 10)
    result <- iaw$ma(x, 3)
    expect_equal(result[10], 5)
})

test_that("iaw$ma returns numeric", {
    result <- iaw$ma(1:10, 3)
    expect_true(is.numeric(result))
})

test_that("iaw$ma handles decimals", {
    x <- c(1.5, 2.5, 3.5, 4.5, 5.5)
    result <- iaw$ma(x, 2)
    expect_true(is.numeric(result))
})

# Failing tests
test_that("iaw$ma rejects non-numeric x", {
    expect_error(iaw$ma(c("a", "b", "c"), 2))
})

test_that("iaw$ma rejects non-numeric window", {
    expect_error(iaw$ma(1:10, "three"))
})

test_that("iaw$ma rejects window less than 1", {
    expect_error(iaw$ma(1:10, 0))
})
