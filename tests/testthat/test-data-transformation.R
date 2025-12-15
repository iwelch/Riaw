# Tests for data transformation: kill.useless.cols, completeobs, doublesort, lagdataframe, etc.

# kill.useless.cols tests
test_that("iaw$kill.useless.cols removes constant columns", {
    df <- data.frame(a = 1:3, b = c(1, 1, 1), c = c("x", "y", "z"))
    result <- iaw$kill.useless.cols(df)
    expect_false("b" %in% names(result))
})

test_that("iaw$kill.useless.cols keeps varying columns", {
    df <- data.frame(a = 1:3, b = c(1, 1, 1))
    result <- iaw$kill.useless.cols(df)
    expect_true("a" %in% names(result))
})

test_that("iaw$kill.useless.cols returns data frame", {
    df <- data.frame(a = 1:3, b = 1:3)
    result <- iaw$kill.useless.cols(df)
    expect_s3_class(result, "data.frame")
})

test_that("iaw$kill.useless.cols handles all varying", {
    df <- data.frame(a = 1:3, b = 4:6)
    result <- iaw$kill.useless.cols(df)
    expect_equal(ncol(result), 2)
})

test_that("iaw$kill.useless.cols handles NA in varying column", {
    df <- data.frame(a = c(1, NA, 3), b = c(1, 1, 1))
    result <- iaw$kill.useless.cols(df)
    expect_true("a" %in% names(result))
})

test_that("iaw$kill.useless.cols handles character columns", {
    df <- data.frame(a = c("x", "x", "x"), b = c("a", "b", "c"))
    result <- iaw$kill.useless.cols(df)
    expect_true("b" %in% names(result))
    expect_false("a" %in% names(result))
})

test_that("iaw$kill.useless.cols preserves row count", {
    df <- data.frame(a = 1:10, b = rep(1, 10))
    result <- iaw$kill.useless.cols(df)
    expect_equal(nrow(result), 10)
})

# Failing tests
test_that("iaw$kill.useless.cols rejects non-data.frame", {
    expect_error(iaw$kill.useless.cols(1:10))
})

test_that("iaw$kill.useless.cols rejects matrix", {
    expect_error(iaw$kill.useless.cols(matrix(1:6, nrow = 2)))
})

test_that("iaw$kill.useless.cols rejects NULL", {
    expect_error(iaw$kill.useless.cols(NULL))
})

# doublesort tests
test_that("iaw$doublesort adds portfolio columns", {
    df <- data.frame(var1 = runif(100), var2 = runif(100))
    result <- iaw$doublesort(df, "var1", "var2")
    expect_true("port1" %in% names(result))
    expect_true("port2" %in% names(result))
})

test_that("iaw$doublesort returns data frame", {
    df <- data.frame(var1 = 1:20, var2 = 1:20)
    result <- iaw$doublesort(df, "var1", "var2")
    expect_s3_class(result, "data.frame")
})

test_that("iaw$doublesort default is 5 groups", {
    df <- data.frame(var1 = 1:100, var2 = 1:100)
    result <- iaw$doublesort(df, "var1", "var2")
    expect_true(max(result$port1, na.rm = TRUE) <= 5)
})

test_that("iaw$doublesort custom n1", {
    df <- data.frame(var1 = 1:100, var2 = 1:100)
    result <- iaw$doublesort(df, "var1", "var2", n1 = 10)
    expect_true(max(result$port1, na.rm = TRUE) <= 10)
})

test_that("iaw$doublesort custom n2", {
    df <- data.frame(var1 = 1:100, var2 = 1:100)
    result <- iaw$doublesort(df, "var1", "var2", n2 = 3)
    expect_true(max(result$port2, na.rm = TRUE) <= 3)
})

test_that("iaw$doublesort preserves original columns", {
    df <- data.frame(var1 = 1:20, var2 = 1:20, other = letters[1:20])
    result <- iaw$doublesort(df, "var1", "var2")
    expect_true("other" %in% names(result))
})

test_that("iaw$doublesort port values are integers", {
    df <- data.frame(var1 = runif(50), var2 = runif(50))
    result <- iaw$doublesort(df, "var1", "var2")
    expect_true(all(result$port1 == floor(result$port1), na.rm = TRUE))
})

# Failing tests
test_that("iaw$doublesort rejects non-data.frame", {
    expect_error(iaw$doublesort(1:10, "a", "b"))
})

test_that("iaw$doublesort rejects missing column", {
    df <- data.frame(var1 = 1:10)
    expect_error(iaw$doublesort(df, "var1", "missing"))
})

test_that("iaw$doublesort rejects invalid column names", {
    df <- data.frame(a = 1:10, b = 1:10)
    expect_error(iaw$doublesort(df, "x", "y"))
})

# lagdataframe tests
test_that("iaw$lagdataframe creates lagged column", {
    df <- data.frame(x = 1:5)
    result <- iaw$lagdataframe(df, "x", 1)
    expect_true("x.L1" %in% names(result))
})

test_that("iaw$lagdataframe lag values correct", {
    df <- data.frame(x = 1:5)
    result <- iaw$lagdataframe(df, "x", 1)
    expect_equal(result$x.L1, c(NA, 1, 2, 3, 4))
})

test_that("iaw$lagdataframe preserves original", {
    df <- data.frame(x = 1:5)
    result <- iaw$lagdataframe(df, "x", 1)
    expect_equal(result$x, 1:5)
})

test_that("iaw$lagdataframe multiple variables", {
    df <- data.frame(x = 1:5, y = 6:10)
    result <- iaw$lagdataframe(df, c("x", "y"), 1)
    expect_true("x.L1" %in% names(result))
    expect_true("y.L1" %in% names(result))
})

test_that("iaw$lagdataframe returns data frame", {
    df <- data.frame(x = 1:5)
    result <- iaw$lagdataframe(df, "x", 1)
    expect_s3_class(result, "data.frame")
})

test_that("iaw$lagdataframe lag of 2", {
    df <- data.frame(x = 1:5)
    result <- iaw$lagdataframe(df, "x", 2)
    expect_true("x.L2" %in% names(result))
})

test_that("iaw$lagdataframe preserves nrow", {
    df <- data.frame(x = 1:10)
    result <- iaw$lagdataframe(df, "x", 1)
    expect_equal(nrow(result), 10)
})

# Failing tests
test_that("iaw$lagdataframe rejects non-data.frame", {
    expect_error(iaw$lagdataframe(1:10, "x", 1))
})

test_that("iaw$lagdataframe rejects missing variable", {
    df <- data.frame(x = 1:5)
    expect_error(iaw$lagdataframe(df, "missing", 1))
})

test_that("iaw$lagdataframe rejects non-character vars", {
    df <- data.frame(x = 1:5)
    expect_error(iaw$lagdataframe(df, 1, 1))
})

# nearest and which.nearest tests
test_that("iaw$nearest finds closest value", {
    expect_equal(iaw$nearest(c(1, 5, 10), 7), 5)
})

test_that("iaw$nearest handles exact match", {
    expect_equal(iaw$nearest(c(1, 5, 10), 5), 5)
})

test_that("iaw$nearest handles first element", {
    expect_equal(iaw$nearest(c(1, 5, 10), 0), 1)
})

test_that("iaw$nearest handles last element", {
    expect_equal(iaw$nearest(c(1, 5, 10), 100), 10)
})

test_that("iaw$nearest returns single value", {
    result <- iaw$nearest(1:100, 50.5)
    expect_length(result, 1)
})

test_that("iaw$nearest handles negative values", {
    expect_equal(iaw$nearest(c(-10, 0, 10), -7), -10)
})

test_that("iaw$nearest handles decimals", {
    expect_equal(iaw$nearest(c(1.1, 1.5, 1.9), 1.4), 1.5)
})

# Failing tests
test_that("iaw$nearest rejects non-numeric vector", {
    expect_error(iaw$nearest(c("a", "b"), "a"))
})

test_that("iaw$nearest rejects non-numeric target", {
    expect_error(iaw$nearest(1:10, "five"))
})

test_that("iaw$nearest rejects vector target", {
    expect_error(iaw$nearest(1:10, c(5, 6)))
})

# which.nearest tests
test_that("iaw$which.nearest returns index", {
    expect_equal(iaw$which.nearest(c(1, 5, 10), 7), 2)
})

test_that("iaw$which.nearest handles exact match", {
    expect_equal(iaw$which.nearest(c(1, 5, 10), 5), 2)
})

test_that("iaw$which.nearest first element", {
    expect_equal(iaw$which.nearest(c(1, 5, 10), 0), 1)
})

test_that("iaw$which.nearest last element", {
    expect_equal(iaw$which.nearest(c(1, 5, 10), 100), 3)
})

test_that("iaw$which.nearest returns integer", {
    result <- iaw$which.nearest(1:10, 5.5)
    expect_type(result, "integer")
})

test_that("iaw$which.nearest single element vector", {
    expect_equal(iaw$which.nearest(5, 10), 1)
})

test_that("iaw$which.nearest handles negative", {
    expect_equal(iaw$which.nearest(c(-5, 0, 5), -3), 1)
})

# Failing tests
test_that("iaw$which.nearest rejects non-numeric", {
    expect_error(iaw$which.nearest(letters, "a"))
})

test_that("iaw$which.nearest rejects non-numeric target", {
    expect_error(iaw$which.nearest(1:10, "five"))
})

test_that("iaw$which.nearest rejects vector target", {
    expect_error(iaw$which.nearest(1:10, c(5, 6)))
})
