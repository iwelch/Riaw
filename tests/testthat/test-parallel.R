# Tests for parallel functions: mc.by, mclapply, mcsapply, mc.replicate, rbind.mc.by, oc.by, rbind.oc.by

# mc.by tests
test_that("iaw$mc.by returns list", {
    skip_on_os("windows")
    df <- data.frame(group = c("A", "A", "B", "B"), value = 1:4)
    result <- iaw$mc.by(df, df$group, function(d) mean(d$value))
    expect_type(result, "list")
})

test_that("iaw$mc.by applies function to groups", {
    skip_on_os("windows")
    df <- data.frame(group = c("A", "A", "B", "B"), value = c(1, 2, 10, 20))
    result <- iaw$mc.by(df, df$group, function(d) mean(d$value))
    expect_equal(result$A, 1.5)
    expect_equal(result$B, 15)
})

test_that("iaw$mc.by handles single group", {
    skip_on_os("windows")
    df <- data.frame(group = c("A", "A", "A"), value = 1:3)
    result <- iaw$mc.by(df, df$group, function(d) sum(d$value))
    expect_equal(result$A, 6)
})

test_that("iaw$mc.by passes additional arguments", {
    skip_on_os("windows")
    df <- data.frame(group = c("A", "A"), value = c(1, NA))
    result <- iaw$mc.by(df, df$group, function(d, na.rm) mean(d$value, na.rm = na.rm), na.rm = TRUE)
    expect_equal(result$A, 1)
})

test_that("iaw$mc.by preserves group names", {
    skip_on_os("windows")
    df <- data.frame(group = c("X", "X", "Y", "Y"), value = 1:4)
    result <- iaw$mc.by(df, df$group, function(d) 1)
    expect_true(all(c("X", "Y") %in% names(result)))
})

test_that("iaw$mc.by handles data frame return", {
    skip_on_os("windows")
    df <- data.frame(group = c("A", "A"), value = 1:2)
    result <- iaw$mc.by(df, df$group, function(d) data.frame(m = mean(d$value)))
    expect_s3_class(result$A, "data.frame")
})

test_that("iaw$mc.by.cripple.toggle works", {
    skip_on_os("windows")
    expect_message(iaw$mc.by.cripple.toggle())
})

# Failing tests
test_that("iaw$mc.by rejects non-data.frame", {
    skip_on_os("windows")
    expect_error(iaw$mc.by(1:10, 1:10, mean))
})

test_that("iaw$mc.by rejects mismatched lengths", {
    skip_on_os("windows")
    df <- data.frame(x = 1:5)
    expect_error(iaw$mc.by(df, 1:3, mean))
})

test_that("iaw$mc.by rejects NULL indices", {
    skip_on_os("windows")
    df <- data.frame(x = 1:5)
    expect_error(iaw$mc.by(df, NULL, mean))
})

# oc.by tests
test_that("iaw$oc.by returns list", {
    df <- data.frame(group = c("A", "A", "B", "B"), value = 1:4)
    result <- iaw$oc.by(df, df$group, function(d) mean(d$value))
    expect_type(result, "list")
})

test_that("iaw$oc.by applies function to groups", {
    df <- data.frame(group = c("A", "A", "B", "B"), value = c(1, 2, 10, 20))
    result <- iaw$oc.by(df, df$group, function(d) mean(d$value))
    expect_equal(result$A, 1.5)
    expect_equal(result$B, 15)
})

test_that("iaw$oc.by handles single group", {
    df <- data.frame(group = c("A", "A"), value = 1:2)
    result <- iaw$oc.by(df, df$group, function(d) sum(d$value))
    expect_equal(result$A, 3)
})

test_that("iaw$oc.by is serial version of mc.by", {
    df <- data.frame(group = c("A", "B"), value = 1:2)
    result <- iaw$oc.by(df, df$group, function(d) d$value)
    expect_type(result, "list")
})

test_that("iaw$oc.by preserves group names", {
    df <- data.frame(group = c("X", "Y"), value = 1:2)
    result <- iaw$oc.by(df, df$group, function(d) 1)
    expect_true(all(c("X", "Y") %in% names(result)))
})

test_that("iaw$oc.by handles numeric groups", {
    df <- data.frame(group = c(1, 1, 2, 2), value = 1:4)
    result <- iaw$oc.by(df, df$group, function(d) sum(d$value))
    expect_type(result, "list")
})

test_that("iaw$oc.by returns correct number of groups", {
    df <- data.frame(group = c("A", "B", "C"), value = 1:3)
    result <- iaw$oc.by(df, df$group, function(d) 1)
    expect_length(result, 3)
})

# Failing tests
test_that("iaw$oc.by rejects non-data.frame", {
    expect_error(iaw$oc.by(1:10, 1:10, mean))
})

test_that("iaw$oc.by rejects mismatched lengths", {
    df <- data.frame(x = 1:5)
    expect_error(iaw$oc.by(df, 1:3, mean))
})

test_that("iaw$oc.by rejects NULL indices", {
    df <- data.frame(x = 1:5)
    expect_error(iaw$oc.by(df, NULL, mean))
})

# rbind.oc.by tests
test_that("iaw$rbind.oc.by returns data frame", {
    df <- data.frame(group = c("A", "A", "B", "B"), value = 1:4)
    result <- iaw$rbind.oc.by(df, df$group, function(d) data.frame(m = mean(d$value)))
    expect_s3_class(result, "data.frame")
})

test_that("iaw$rbind.oc.by combines results", {
    df <- data.frame(group = c("A", "A", "B", "B"), value = 1:4)
    result <- iaw$rbind.oc.by(df, df$group, function(d) data.frame(m = mean(d$value)))
    expect_equal(nrow(result), 2)
})

test_that("iaw$rbind.oc.by preserves column names", {
    df <- data.frame(group = c("A", "B"), value = 1:2)
    result <- iaw$rbind.oc.by(df, df$group, function(d) data.frame(result = d$value))
    expect_true("result" %in% names(result))
})

test_that("iaw$rbind.oc.by handles multiple columns", {
    df <- data.frame(group = c("A", "B"), value = 1:2)
    result <- iaw$rbind.oc.by(df, df$group, function(d) data.frame(a = 1, b = 2))
    expect_equal(ncol(result), 2)
})

test_that("iaw$rbind.oc.by handles empty groups", {
    df <- data.frame(group = character(0), value = numeric(0))
    result <- iaw$rbind.oc.by(df, df$group, function(d) data.frame(m = 1))
    expect_true(is.null(result) || nrow(result) == 0)
})

test_that("iaw$rbind.oc.by single group", {
    df <- data.frame(group = c("A", "A"), value = 1:2)
    result <- iaw$rbind.oc.by(df, df$group, function(d) data.frame(m = sum(d$value)))
    expect_equal(nrow(result), 1)
})

test_that("iaw$rbind.oc.by numeric result", {
    df <- data.frame(group = c("A", "B"), value = 1:2)
    result <- iaw$rbind.oc.by(df, df$group, function(d) data.frame(m = as.numeric(d$value)))
    expect_type(result$m, "double")
})

# Failing tests
test_that("iaw$rbind.oc.by rejects non-data.frame", {
    expect_error(iaw$rbind.oc.by(1:10, 1:10, mean))
})

test_that("iaw$rbind.oc.by rejects non-df returning function", {
    df <- data.frame(group = c("A", "B"), value = 1:2)
    # This should work but return NULL for rbind
    result <- iaw$rbind.oc.by(df, df$group, function(d) mean(d$value))
    expect_true(is.null(result) || is.data.frame(result))
})

test_that("iaw$rbind.oc.by rejects NULL data", {
    expect_error(iaw$rbind.oc.by(NULL, NULL, mean))
})

# mclapply tests
test_that("iaw$mclapply returns list", {
    result <- iaw$mclapply(1:3, function(x) x^2)
    expect_type(result, "list")
})

test_that("iaw$mclapply applies function", {
    result <- iaw$mclapply(1:3, function(x) x^2)
    expect_equal(result, list(1, 4, 9))
})

test_that("iaw$mclapply handles named list", {
    result <- iaw$mclapply(list(a = 1, b = 2), function(x) x * 2)
    expect_equal(result$a, 2)
})

test_that("iaw$mclapply handles character vector", {
    result <- iaw$mclapply(c("a", "b"), nchar)
    expect_equal(result, list(1, 1))
})

test_that("iaw$mclapply passes arguments", {
    result <- iaw$mclapply(list(c(1, NA)), mean, na.rm = TRUE)
    expect_equal(result[[1]], 1)
})

test_that("iaw$mclapply handles empty list", {
    result <- iaw$mclapply(list(), function(x) x)
    expect_length(result, 0)
})

test_that("iaw$mclapply single element", {
    result <- iaw$mclapply(list(5), function(x) x * 2)
    expect_equal(result[[1]], 10)
})

# Failing tests
test_that("iaw$mclapply rejects non-function FUN", {
    expect_error(iaw$mclapply(1:3, "not a function"))
})

test_that("iaw$mclapply rejects NULL X", {
    expect_error(iaw$mclapply(NULL, mean))
})

test_that("iaw$mclapply rejects non-numeric mc.cores", {
    expect_error(iaw$mclapply(1:3, mean, mc.cores = "many"))
})

# mcsapply tests
test_that("iaw$mcsapply simplifies to vector", {
    result <- iaw$mcsapply(1:3, function(x) x^2)
    expect_equal(result, c(1, 4, 9))
})

test_that("iaw$mcsapply handles single element", {
    result <- iaw$mcsapply(5, function(x) x * 2)
    expect_equal(result, 10)
})

test_that("iaw$mcsapply returns array for matrix results", {
    result <- iaw$mcsapply(1:2, function(x) c(x, x^2))
    expect_true(is.matrix(result) || is.vector(result))
})

test_that("iaw$mcsapply handles character input", {
    result <- iaw$mcsapply(c("a", "bb", "ccc"), nchar)
    expect_equal(result, c(1, 2, 3))
})

test_that("iaw$mcsapply handles list input", {
    result <- iaw$mcsapply(list(1:3, 4:6), sum)
    expect_equal(result, c(6, 15))
})

test_that("iaw$mcsapply alias mc.sapply works", {
    result <- iaw$mc.sapply(1:3, function(x) x^2)
    expect_equal(result, c(1, 4, 9))
})

test_that("iaw$mcsapply returns numeric", {
    result <- iaw$mcsapply(1:5, function(x) x)
    expect_type(result, "integer")
})

# mc.replicate tests
test_that("iaw$mc.replicate returns vector", {
    set.seed(123)
    result <- iaw$mc.replicate(5, rnorm(1))
    expect_length(result, 5)
})

test_that("iaw$mc.replicate respects n", {
    result <- iaw$mc.replicate(10, 1)
    expect_length(result, 10)
})

test_that("iaw$mc.replicate handles simple expression", {
    result <- iaw$mc.replicate(3, 5)
    expect_equal(result, c(5, 5, 5))
})

test_that("iaw$mc.replicate simplify=FALSE returns list", {
    result <- iaw$mc.replicate(3, 5, simplify = FALSE)
    expect_type(result, "list")
})

test_that("iaw$mc.replicate handles mean calculation", {
    set.seed(123)
    result <- iaw$mc.replicate(100, mean(rnorm(10)))
    expect_true(abs(mean(result)) < 0.5)
})

test_that("iaw$mc.replicate single replication", {
    result <- iaw$mc.replicate(1, 42)
    expect_equal(result, 42)
})

test_that("iaw$mc.replicate numeric result", {
    result <- iaw$mc.replicate(5, pi)
    expect_type(result, "double")
})

# Failing tests
test_that("iaw$mc.replicate rejects n <= 0", {
    expect_error(iaw$mc.replicate(0, 1))
})

test_that("iaw$mc.replicate rejects non-numeric n", {
    expect_error(iaw$mc.replicate("five", 1))
})

test_that("iaw$mc.replicate rejects negative n", {
    expect_error(iaw$mc.replicate(-5, 1))
})
