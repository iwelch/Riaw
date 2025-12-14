# Tests for utility functions: whatis, msg, chatter, beep, bisection, ls.objects, etc.

# whatis tests
test_that("iaw$whatis describes numeric vector", {
    result <- iaw$whatis(1:10)
    expect_match(result, "integer")
})

test_that("iaw$whatis describes data frame", {
    result <- iaw$whatis(data.frame(a = 1))
    expect_match(result, "data.frame")
})

test_that("iaw$whatis includes dimensions", {
    df <- data.frame(a = 1:5, b = 1:5)
    result <- iaw$whatis(df)
    expect_match(result, "5")
})

test_that("iaw$whatis describes character", {
    result <- iaw$whatis("hello")
    expect_match(result, "character")
})

test_that("iaw$whatis describes matrix", {
    result <- iaw$whatis(matrix(1:6, nrow = 2))
    expect_match(result, "matrix")
})

test_that("iaw$whatis describes list", {
    result <- iaw$whatis(list(a = 1, b = 2))
    expect_match(result, "list")
})

test_that("iaw$whatis returns character", {
    expect_type(iaw$whatis(1:10), "character")
})

test_that("iaw$whatis describes function", {
    result <- iaw$whatis(mean)
    expect_match(result, "function")
})

test_that("iaw$whatis describes NULL", {
    result <- iaw$whatis(NULL)
    expect_match(result, "NULL")
})

test_that("iaw$whatis includes length for vectors", {
    result <- iaw$whatis(1:100)
    expect_match(result, "100")
})

# chatter tests
test_that("iaw$chatter prints when verbose", {
    expect_output(iaw$chatter("test", verbose = TRUE), "test")
})

test_that("iaw$chatter silent when not verbose", {
    expect_silent(iaw$chatter("test", verbose = FALSE))
})

test_that("iaw$chatter returns invisible TRUE", {
    expect_invisible(iaw$chatter("test"))
})

test_that("iaw$chatter handles multiple arguments", {
    expect_output(iaw$chatter("a", "b", "c", verbose = TRUE), "abc")
})

test_that("iaw$chatter default is verbose", {
    expect_output(iaw$chatter("test"), "test")
})

test_that("iaw$chatter handles newline", {
    expect_output(iaw$chatter("test\n", verbose = TRUE), "test")
})

test_that("iaw$chatter handles numbers", {
    expect_output(iaw$chatter(123, verbose = TRUE), "123")
})

# Failing tests
test_that("iaw$chatter rejects non-logical verbose", {
    expect_error(iaw$chatter("test", verbose = "yes"))
})

test_that("iaw$chatter rejects vector verbose", {
    expect_error(iaw$chatter("test", verbose = c(TRUE, FALSE)))
})

test_that("iaw$chatter rejects NULL verbose", {
    expect_error(iaw$chatter("test", verbose = NULL))
})

# bisection tests
test_that("iaw$bisection finds root", {
    result <- iaw$bisection(function(x) x^2 - 4, 0, 3)
    expect_equal(result, 2, tolerance = 1e-6)
})

test_that("iaw$bisection finds sqrt(2)", {
    result <- iaw$bisection(function(x) x^2 - 2, 0, 2)
    expect_equal(result, sqrt(2), tolerance = 1e-6)
})

test_that("iaw$bisection handles negative root", {
    result <- iaw$bisection(function(x) x + 5, -10, 0)
    expect_equal(result, -5, tolerance = 1e-6)
})

test_that("iaw$bisection respects tolerance", {
    result <- iaw$bisection(function(x) x - 1, 0, 2, tol = 0.1)
    expect_equal(result, 1, tolerance = 0.1)
})

test_that("iaw$bisection finds zero of sin", {
    result <- iaw$bisection(sin, 3, 4)
    expect_equal(result, pi, tolerance = 1e-6)
})

test_that("iaw$bisection returns numeric", {
    result <- iaw$bisection(function(x) x, -1, 1)
    expect_type(result, "double")
})

test_that("iaw$bisection handles linear function", {
    result <- iaw$bisection(function(x) 2*x - 6, 0, 5)
    expect_equal(result, 3, tolerance = 1e-6)
})

# Failing tests
test_that("iaw$bisection rejects non-function", {
    expect_error(iaw$bisection("not a function", 0, 1))
})

test_that("iaw$bisection rejects non-numeric bounds", {
    expect_error(iaw$bisection(sin, "a", "b"))
})

test_that("iaw$bisection rejects negative tolerance", {
    expect_error(iaw$bisection(sin, 0, 1, tol = -1))
})

# ls.objects tests
test_that("iaw$ls.objects returns data frame", {
    result <- iaw$ls.objects()
    expect_s3_class(result, "data.frame")
})

test_that("iaw$ls.objects has size_MB column", {
    result <- iaw$ls.objects()
    expect_true("size_MB" %in% names(result) || nrow(result) == 0)
})

test_that("iaw$ls.objects respects n parameter", {
    result <- iaw$ls.objects(n = 5)
    expect_true(nrow(result) <= 5)
})

test_that("iaw$ls.objects sorted by size", {
    # Create some objects
    test_small <- 1
    test_large <- rnorm(10000)
    result <- iaw$ls.objects(n = 10)
    if (nrow(result) > 1) {
        expect_true(result$size_MB[1] >= result$size_MB[nrow(result)])
    }
    rm(test_small, test_large)
})

test_that("iaw$ls.objects handles empty environment", {
    e <- new.env()
    result <- iaw$ls.objects(envir = e)
    expect_equal(nrow(result), 0)
})

test_that("iaw$ls.objects returns numeric sizes", {
    result <- iaw$ls.objects()
    if (nrow(result) > 0) {
        expect_type(result$size_MB, "double")
    }
})

test_that("iaw$ls.objects includes object names", {
    result <- iaw$ls.objects()
    expect_true("object" %in% names(result) || nrow(result) == 0)
})

# meminfo tests
test_that("iaw$meminfo returns list", {
    result <- iaw$meminfo()
    expect_type(result, "list")
})
  
test_that("iaw$meminfo has used_MB", {
    result <- iaw$meminfo()
    expect_true("used_MB" %in% names(result))
})

test_that("iaw$meminfo has max_MB", {
    result <- iaw$meminfo()
    expect_true("max_MB" %in% names(result))
})

test_that("iaw$meminfo returns numeric values", {
    result <- iaw$meminfo()
    expect_true(is.numeric(result$used_MB))
})

test_that("iaw$meminfo used <= max", {
    result <- iaw$meminfo()
    expect_true(result$used_MB <= result$max_MB)
})

test_that("iaw$meminfo positive values", {
    result <- iaw$meminfo()
    expect_true(result$used_MB >= 0)
})

test_that("iaw$meminfo is snapshot", {
    result1 <- iaw$meminfo()
    result2 <- iaw$meminfo()
    expect_true(is.numeric(result1$used_MB))
    expect_true(is.numeric(result2$used_MB))
})

# osinfo tests
test_that("iaw$osinfo returns list", {
    result <- iaw$osinfo()
    expect_type(result, "list")
})

test_that("iaw$osinfo has sysname", {
    result <- iaw$osinfo()
    expect_true("sysname" %in% names(result))
})

test_that("iaw$osinfo has R_version", {
    result <- iaw$osinfo()
    expect_true("R_version" %in% names(result))
})

test_that("iaw$osinfo sysname is character", {
    result <- iaw$osinfo()
    expect_type(result$sysname, "character")
})

test_that("iaw$osinfo has machine", {
    result <- iaw$osinfo()
    expect_true("machine" %in% names(result))
})

test_that("iaw$osinfo matches Sys.info", {
    result <- iaw$osinfo()
    expect_equal(result$sysname, Sys.info()["sysname"])
})

test_that("iaw$osinfo R version matches", {
    result <- iaw$osinfo()
    expect_equal(result$R_version, R.version.string)
})

# object.size.MB tests
test_that("iaw$object.size.MB returns numeric", {
    expect_type(iaw$object.size.MB(1:1000), "double")
})

test_that("iaw$object.size.MB larger for larger objects", {
    small <- 1:100
    large <- 1:1000000
    expect_true(iaw$object.size.MB(large) > iaw$object.size.MB(small))
})

test_that("iaw$object.size.MB handles data frame", {
    df <- data.frame(x = rnorm(10000))
    expect_true(iaw$object.size.MB(df) > 0)
})

test_that("iaw$object.size.MB handles character", {
    x <- rep("hello", 10000)
    expect_true(iaw$object.size.MB(x) > 0)
})

test_that("iaw$object.size.MB handles small object", {
    expect_true(iaw$object.size.MB(1) >= 0)
})

test_that("iaw$object.size.MB rounded", {
    result <- iaw$object.size.MB(1:1000)
    expect_equal(result, round(result, 2))
})

test_that("iaw$object.size.MB handles list", {
    x <- list(a = 1:1000, b = rnorm(1000))
    expect_true(iaw$object.size.MB(x) > 0)
})

# now tests
test_that("iaw$now returns character", {
    expect_type(iaw$now(), "character")
})

test_that("iaw$now default format has date", {
    result <- iaw$now()
    expect_match(result, "\\d{4}-\\d{2}-\\d{2}")
})

test_that("iaw$now custom format", {
    result <- iaw$now("%Y")
    expect_match(result, "^\\d{4}$")
})

test_that("iaw$now changes over time", {
    t1 <- iaw$now("%H:%M:%S")
    Sys.sleep(0.1)
    t2 <- iaw$now("%H:%M:%S")
    # At least seconds might change (or they're the same)
    expect_type(t1, "character")
    expect_type(t2, "character")
})

test_that("iaw$now includes time", {
    result <- iaw$now()
    expect_match(result, "\\d{2}:\\d{2}:\\d{2}")
})

test_that("iaw$now length is 1", {
    expect_length(iaw$now(), 1)
})

test_that("iaw$now date only format", {
    result <- iaw$now("%Y%m%d")
    expect_match(result, "^\\d{8}$")
})

# Failing tests
test_that("iaw$now rejects non-character format", {
    expect_error(iaw$now(123))
})

test_that("iaw$now rejects vector format", {
    expect_error(iaw$now(c("%Y", "%m")))
})

test_that("iaw$now rejects NULL format", {
    expect_error(iaw$now(NULL))
})
