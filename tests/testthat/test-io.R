# Tests for I/O functions: read.csv, write.csv, lock, unlock, touch

# --- write.csv / read.csv round-trip ---

test_that("write.csv then read.csv round-trips a data frame", {
    skip_if_not_installed("data.table")
    df <- data.frame(a = 1:5, b = c(1.1, 2.2, 3.3, 4.4, 5.5),
                     c = c("x", "y", "z", "w", "v"),
                     stringsAsFactors = FALSE)
    tmpfile <- tempfile(fileext = ".csv")
    on.exit(unlink(tmpfile), add = TRUE)
    iaw$write.csv(df, tmpfile, quiet = TRUE)
    expect_true(file.exists(tmpfile))
    result <- iaw$read.csv(tmpfile, quiet = TRUE)
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 5)
    expect_equal(ncol(result), 3)
    expect_equal(result$a, df$a)
    expect_equal(result$b, df$b, tolerance = 1e-10)
    expect_equal(result$c, df$c)
})

test_that("write.csv rejects non-data.frame input", {
    skip_if_not_installed("data.table")
    expect_error(iaw$write.csv(1:10, tempfile(fileext = ".csv")))
})

test_that("write.csv rejects wrong file extension", {
    skip_if_not_installed("data.table")
    df <- data.frame(x = 1)
    expect_error(iaw$write.csv(df, tempfile(fileext = ".txt")))
})

test_that("write.csv abort.on.overwrite prevents overwriting", {
    skip_if_not_installed("data.table")
    df <- data.frame(x = 1:3)
    tmpfile <- tempfile(fileext = ".csv")
    on.exit(unlink(tmpfile), add = TRUE)
    iaw$write.csv(df, tmpfile, quiet = TRUE)
    expect_error(iaw$write.csv(df, tmpfile, abort.on.overwrite = TRUE, quiet = TRUE))
})

test_that("write.csv handles csv.gz extension", {
    skip_if_not_installed("data.table")
    df <- data.frame(x = 1:3)
    tmpfile <- tempfile(fileext = ".csv.gz")
    on.exit(unlink(tmpfile), add = TRUE)
    iaw$write.csv(df, tmpfile, quiet = TRUE)
    expect_true(file.exists(tmpfile))
})

test_that("read.csv rejects non-existent file", {
    skip_if_not_installed("data.table")
    expect_error(iaw$read.csv("/no/such/file.csv", quiet = TRUE))
})

test_that("read.csv rejects wrong file extension", {
    skip_if_not_installed("data.table")
    tmpfile <- tempfile(fileext = ".txt")
    on.exit(unlink(tmpfile), add = TRUE)
    writeLines("hello", tmpfile)
    expect_error(iaw$read.csv(tmpfile, quiet = TRUE))
})

# --- lock / unlock ---

test_that("lock acquires and unlock releases", {
    lockfile <- tempfile(pattern = "locktest")
    on.exit(unlink(lockfile), add = TRUE)
    result <- iaw$lock(lockfile, timeout = 2)
    expect_true(result)
    expect_true(file.exists(lockfile))
    # Lock file should contain the PID
    content <- readLines(lockfile)
    expect_match(content, "^\\d+$")
    iaw$unlock(lockfile)
    expect_false(file.exists(lockfile))
})

test_that("lock times out when lock already held", {
    lockfile <- tempfile(pattern = "locktest")
    on.exit(unlink(lockfile), add = TRUE)
    writeLines("other-pid", lockfile)
    result <- iaw$lock(lockfile, timeout = 0.2)
    expect_false(result)
})

test_that("unlock on non-existent file is silent", {
    expect_silent(iaw$unlock(tempfile()))
})

test_that("lock rejects non-character lockfile", {
    expect_error(iaw$lock(123))
})

# --- touch ---

test_that("touch creates new file and updates existing timestamp", {
    tmpfile <- tempfile(pattern = "touchtest")
    on.exit(unlink(tmpfile), add = TRUE)
    expect_false(file.exists(tmpfile))
    result <- iaw$touch(tmpfile)
    expect_true(result)
    expect_true(file.exists(tmpfile))
    expect_equal(file.info(tmpfile)$size, 0)
    # Touch again updates mtime
    time1 <- file.mtime(tmpfile)
    Sys.sleep(0.1)
    iaw$touch(tmpfile)
    time2 <- file.mtime(tmpfile)
    expect_true(time2 >= time1)
})

test_that("touch rejects non-character filename", {
    expect_error(iaw$touch(42))
})
