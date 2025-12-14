# Tests for I/O functions: read.csv, write.csv, source, sink, locate, touch, lock

# write.csv tests (need temp files)
test_that("iaw$write.csv creates file", {
    skip_if_not_installed("data.table")
    df <- data.frame(x = 1:3, y = 4:6)
    tmpfile <- tempfile(fileext = ".csv")
    iaw$write.csv(df, tmpfile, verbose = FALSE)
    expect_true(file.exists(tmpfile))
    unlink(tmpfile)
})

test_that("iaw$write.csv returns invisibly", {
    skip_if_not_installed("data.table")
    df <- data.frame(x = 1:3)
    tmpfile <- tempfile(fileext = ".csv")
    expect_invisible(iaw$write.csv(df, tmpfile, verbose = FALSE))
    unlink(tmpfile)
})

test_that("iaw$write.csv handles csv.gz", {
    skip_if_not_installed("data.table")
    df <- data.frame(x = 1:3)
    tmpfile <- tempfile(fileext = ".csv.gz")
    iaw$write.csv(df, tmpfile, verbose = FALSE)
    expect_true(file.exists(tmpfile))
    unlink(tmpfile)
})

test_that("iaw$write.csv respects allow.overwrite=FALSE", {
    skip_if_not_installed("data.table")
    df <- data.frame(x = 1:3)
    tmpfile <- tempfile(fileext = ".csv")
    iaw$write.csv(df, tmpfile, verbose = FALSE)
    expect_output(iaw$write.csv(df, tmpfile, allow.overwrite = FALSE, verbose = FALSE), "not overwriting")
    unlink(tmpfile)
})

test_that("iaw$write.csv abort.on.overwrite stops", {
    skip_if_not_installed("data.table")
    df <- data.frame(x = 1:3)
    tmpfile <- tempfile(fileext = ".csv")
    iaw$write.csv(df, tmpfile, verbose = FALSE)
    expect_error(iaw$write.csv(df, tmpfile, abort.on.overwrite = TRUE))
    unlink(tmpfile)
})

test_that("iaw$write.csv alias fwrite works", {
    skip_if_not_installed("data.table")
    df <- data.frame(x = 1:3)
    tmpfile <- tempfile(fileext = ".csv")
    iaw$fwrite(df, tmpfile, verbose = FALSE)
    expect_true(file.exists(tmpfile))
    unlink(tmpfile)
})

test_that("iaw$write.csv alias write.csv.gz works", {
    skip_if_not_installed("data.table")
    df <- data.frame(x = 1:3)
    tmpfile <- tempfile(fileext = ".csv.gz")
    iaw$write.csv.gz(df, tmpfile, verbose = FALSE)
    expect_true(file.exists(tmpfile))
    unlink(tmpfile)
})

# Failing tests
test_that("iaw$write.csv rejects non-data.frame", {
    skip_if_not_installed("data.table")
    expect_error(iaw$write.csv(1:10, tempfile(fileext = ".csv")))
})

test_that("iaw$write.csv rejects wrong extension", {
    skip_if_not_installed("data.table")
    df <- data.frame(x = 1:3)
    expect_error(iaw$write.csv(df, tempfile(fileext = ".txt")))
})

test_that("iaw$write.csv rejects non-string filename", {
    skip_if_not_installed("data.table")
    df <- data.frame(x = 1:3)
    expect_error(iaw$write.csv(df, 123))
})

# read.csv tests
test_that("iaw$read.csv reads csv", {
    skip_if_not_installed("data.table")
    df <- data.frame(x = 1:3, y = 4:6)
    tmpfile <- tempfile(fileext = ".csv")
    write.csv(df, tmpfile, row.names = FALSE)
    result <- iaw$read.csv(tmpfile, verbose = FALSE)
    expect_s3_class(result, "data.frame")
    unlink(tmpfile)
})

test_that("iaw$read.csv returns data frame", {
    skip_if_not_installed("data.table")
    df <- data.frame(x = 1:3)
    tmpfile <- tempfile(fileext = ".csv")
    write.csv(df, tmpfile, row.names = FALSE)
    result <- iaw$read.csv(tmpfile, verbose = FALSE)
    expect_s3_class(result, "data.frame")
    unlink(tmpfile)
})

test_that("iaw$read.csv preserves data", {
    skip_if_not_installed("data.table")
    df <- data.frame(x = 1:3, y = c("a", "b", "c"))
    tmpfile <- tempfile(fileext = ".csv")
    write.csv(df, tmpfile, row.names = FALSE)
    result <- iaw$read.csv(tmpfile, verbose = FALSE)
    expect_equal(result$x, df$x)
    unlink(tmpfile)
})

test_that("iaw$read.csv alias fread works", {
    skip_if_not_installed("data.table")
    df <- data.frame(x = 1:3)
    tmpfile <- tempfile(fileext = ".csv")
    write.csv(df, tmpfile, row.names = FALSE)
    result <- iaw$fread(tmpfile, verbose = FALSE)
    expect_s3_class(result, "data.frame")
    unlink(tmpfile)
})

test_that("iaw$read.csv handles tsv", {
    skip_if_not_installed("data.table")
    df <- data.frame(x = 1:3)
    tmpfile <- tempfile(fileext = ".tsv")
    write.table(df, tmpfile, row.names = FALSE, sep = "\t")
    result <- iaw$read.csv(tmpfile, verbose = FALSE)
    expect_s3_class(result, "data.frame")
    unlink(tmpfile)
})

test_that("iaw$read.csv adds search path", {
    skip_if_not_installed("data.table")
    df <- data.frame(x = 1)
    tmpfile <- tempfile(fileext = ".csv")
    write.csv(df, tmpfile, row.names = FALSE)
    result <- iaw$read.csv(tmpfile, search = dirname(tmpfile), verbose = FALSE)
    expect_s3_class(result, "data.frame")
    unlink(tmpfile)
})

test_that("iaw$read.csv correct dimensions", {
    skip_if_not_installed("data.table")
    df <- data.frame(x = 1:5, y = 6:10)
    tmpfile <- tempfile(fileext = ".csv")
    write.csv(df, tmpfile, row.names = FALSE)
    result <- iaw$read.csv(tmpfile, verbose = FALSE)
    expect_equal(dim(result), c(5, 2))
    unlink(tmpfile)
})

# Failing tests
test_that("iaw$read.csv rejects non-existent file", {
    skip_if_not_installed("data.table")
    expect_error(iaw$read.csv("nonexistent.csv", verbose = FALSE))
})

test_that("iaw$read.csv rejects wrong extension", {
    skip_if_not_installed("data.table")
    tmpfile <- tempfile(fileext = ".txt")
    writeLines("hello", tmpfile)
    expect_error(iaw$read.csv(tmpfile, verbose = FALSE))
    unlink(tmpfile)
})

test_that("iaw$read.csv rejects non-string filename", {
    skip_if_not_installed("data.table")
    expect_error(iaw$read.csv(123, verbose = FALSE))
})

# locate tests
test_that("iaw$locate finds file in current dir", {
    tmpfile <- tempfile(fileext = ".txt")
    writeLines("test", tmpfile)
    result <- iaw$locate(basename(tmpfile), dirname(tmpfile))
    expect_true(!is.null(result))
    unlink(tmpfile)
})

test_that("iaw$locate returns NULL for missing file", {
    result <- iaw$locate("nonexistent_file_12345.txt")
    expect_null(result)
})

test_that("iaw$locate returns full path", {
    tmpfile <- tempfile(fileext = ".txt")
    writeLines("test", tmpfile)
    result <- iaw$locate(basename(tmpfile), dirname(tmpfile))
    expect_true(file.exists(result))
    unlink(tmpfile)
})

test_that("iaw$locate searches multiple paths", {
    tmpdir <- tempdir()
    tmpfile <- file.path(tmpdir, "testfile.txt")
    writeLines("test", tmpfile)
    result <- iaw$locate("testfile.txt", c(".", tmpdir))
    expect_true(!is.null(result))
    unlink(tmpfile)
})

test_that("iaw$locate handles relative paths", {
    result <- iaw$locate("nonexistent.txt", ".")
    expect_null(result)
})

test_that("iaw$locate single path", {
    result <- iaw$locate("nonexistent.txt", tempdir())
    expect_null(result)
})

test_that("iaw$locate returns character or NULL", {
    result <- iaw$locate("nonexistent.txt")
    expect_true(is.null(result) || is.character(result))
})

# Failing tests
test_that("iaw$locate rejects non-character filename", {
    expect_error(iaw$locate(123))
})

test_that("iaw$locate rejects vector filename", {
    expect_error(iaw$locate(c("a.txt", "b.txt")))
})

test_that("iaw$locate rejects NULL filename", {
    expect_error(iaw$locate(NULL))
})

# touch tests
test_that("iaw$touch creates file", {
    tmpfile <- tempfile()
    iaw$touch(tmpfile)
    expect_true(file.exists(tmpfile))
    unlink(tmpfile)
})

test_that("iaw$touch returns invisible TRUE", {
    tmpfile <- tempfile()
    expect_invisible(iaw$touch(tmpfile))
    unlink(tmpfile)
})

test_that("iaw$touch updates timestamp", {
    tmpfile <- tempfile()
    iaw$touch(tmpfile)
    time1 <- file.mtime(tmpfile)
    Sys.sleep(0.1)
    iaw$touch(tmpfile)
    time2 <- file.mtime(tmpfile)
    expect_true(time2 >= time1)
    unlink(tmpfile)
})

test_that("iaw$touch creates empty file", {
    tmpfile <- tempfile()
    iaw$touch(tmpfile)
    expect_equal(file.info(tmpfile)$size, 0)
    unlink(tmpfile)
})

test_that("iaw$touch preserves content", {
    tmpfile <- tempfile()
    writeLines("content", tmpfile)
    iaw$touch(tmpfile)
    expect_equal(readLines(tmpfile), "content")
    unlink(tmpfile)
})

test_that("iaw$touch handles path with spaces", {
    tmpdir <- tempdir()
    tmpfile <- file.path(tmpdir, "file with spaces.txt")
    iaw$touch(tmpfile)
    expect_true(file.exists(tmpfile))
    unlink(tmpfile)
})

test_that("iaw$touch returns TRUE", {
    tmpfile <- tempfile()
    result <- iaw$touch(tmpfile)
    expect_true(result)
    unlink(tmpfile)
})

# Failing tests
test_that("iaw$touch rejects non-character filename", {
    expect_error(iaw$touch(123))
})

test_that("iaw$touch rejects vector filename", {
    expect_error(iaw$touch(c("a.txt", "b.txt")))
})

test_that("iaw$touch rejects NULL", {
    expect_error(iaw$touch(NULL))
})

# lock tests
test_that("iaw$lock creates lock file", {
    lockfile <- tempfile()
    result <- iaw$lock(lockfile, timeout = 1)
    expect_true(result)
    expect_true(file.exists(lockfile))
    iaw$unlock(lockfile)
})

test_that("iaw$lock returns TRUE on success", {
    lockfile <- tempfile()
    result <- iaw$lock(lockfile, timeout = 1)
    expect_true(result)
    iaw$unlock(lockfile)
})

test_that("iaw$unlock removes lock file", {
    lockfile <- tempfile()
    iaw$lock(lockfile, timeout = 1)
    iaw$unlock(lockfile)
    expect_false(file.exists(lockfile))
})

test_that("iaw$lock waits for existing lock", {
    lockfile <- tempfile()
    writeLines("locked", lockfile)  # Create existing lock
    result <- iaw$lock(lockfile, timeout = 0.5)
    expect_false(result)
    unlink(lockfile)
})

test_that("iaw$lock contains PID", {
    lockfile <- tempfile()
    iaw$lock(lockfile, timeout = 1)
    content <- readLines(lockfile)
    expect_match(content, "\\d+")
    iaw$unlock(lockfile)
})

test_that("iaw$unlock handles missing file", {
    expect_silent(iaw$unlock(tempfile()))
})

test_that("iaw$lock returns FALSE on timeout", {
    lockfile <- tempfile()
    writeLines("locked", lockfile)
    result <- iaw$lock(lockfile, timeout = 0.1)
    expect_false(result)
    unlink(lockfile)
})

# Failing tests
test_that("iaw$lock rejects non-character lockfile", {
    expect_error(iaw$lock(123))
})

test_that("iaw$lock rejects non-numeric timeout", {
    expect_error(iaw$lock(tempfile(), timeout = "long"))
})

test_that("iaw$lock rejects vector lockfile", {
    expect_error(iaw$lock(c("a", "b")))
})
