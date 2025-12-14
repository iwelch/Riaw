# Tests for recode, rename.columns, check.names, wide2long, long2wide, multimerge, etc.

# recode tests
test_that("iaw$recode replaces values", {
    x <- c(1, 2, 3, 1, 2)
    result <- iaw$recode(x, c(1, 2), c(10, 20))
    expect_equal(result, c(10, 20, 3, 10, 20))
})

test_that("iaw$recode preserves unmatched", {
    x <- c(1, 2, 3)
    result <- iaw$recode(x, c(1), c(10))
    expect_equal(result, c(10, 2, 3))
})

test_that("iaw$recode non.from.becomes.na", {
    x <- c(1, 2, 3)
    result <- iaw$recode(x, c(1, 2), c(10, 20), non.from.becomes.na = TRUE)
    expect_equal(result, c(10, 20, NA))
})

test_that("iaw$recode handles character", {
    x <- c("a", "b", "c")
    result <- iaw$recode(x, c("a", "b"), c("x", "y"))
    expect_equal(result, c("x", "y", "c"))
})

test_that("iaw$recode handles NA in input", {
    x <- c(1, NA, 3)
    result <- iaw$recode(x, c(1), c(10))
    expect_true(is.na(result[2]))
})

test_that("iaw$recode preserves type", {
    x <- c(1.0, 2.0, 3.0)
    result <- iaw$recode(x, c(1), c(10))
    expect_type(result, "double")
})

test_that("iaw$recode handles factor-like recoding", {
    x <- c("low", "med", "high", "low")
    result <- iaw$recode(x, c("low", "med", "high"), c(1, 2, 3))
    expect_equal(result, c(1, 2, 3, 1))
})

# Failing tests
test_that("iaw$recode rejects mismatched from/to length", {
    expect_error(iaw$recode(1:3, c(1, 2), c(10)))
})

test_that("iaw$recode rejects mismatched lengths", {
    expect_error(iaw$recode(1:3, c(1, 2, 3), c(10, 20)))
})

test_that("iaw$recode rejects empty from/to", {
    expect_error(iaw$recode(1:3, c(), c()))
})

# rename.columns tests
test_that("iaw$rename.columns renames columns", {
    df <- data.frame(a = 1, b = 2)
    result <- iaw$rename.columns(df, c("a", "b"), c("x", "y"))
    expect_equal(names(result), c("x", "y"))
})

test_that("iaw$rename.columns named vector syntax", {
    df <- data.frame(a = 1, b = 2)
    result <- iaw$rename.columns(df, c(a = "x", b = "y"))
    expect_equal(names(result), c("x", "y"))
})

test_that("iaw$rename.columns partial rename", {
    df <- data.frame(a = 1, b = 2, c = 3)
    result <- iaw$rename.columns(df, c(a = "x"))
    expect_equal(names(result), c("x", "b", "c"))
})

test_that("iaw$rename.columns works on character vector", {
    nms <- c("a", "b", "c")
    result <- iaw$rename.columns(nms, c(a = "x"))
    expect_equal(result, c("x", "b", "c"))
})

test_that("iaw$rename.columns preserves data", {
    df <- data.frame(a = 1:3, b = 4:6)
    result <- iaw$rename.columns(df, c(a = "x"))
    expect_equal(result$x, 1:3)
})

test_that("iaw$rename.columns alias works", {
    df <- data.frame(a = 1)
    result <- iaw$rename.column(df, c(a = "x"))
    expect_equal(names(result), "x")
})

test_that("iaw$rename.columns returns data frame for df input", {
    df <- data.frame(a = 1)
    result <- iaw$rename.columns(df, c(a = "x"))
    expect_s3_class(result, "data.frame")
})

# Failing tests
test_that("iaw$rename.columns rejects mismatched lengths", {
    df <- data.frame(a = 1, b = 2)
    expect_error(iaw$rename.columns(df, c("a", "b"), c("x")))
})

test_that("iaw$rename.columns handles missing names gracefully", {
    df <- data.frame(a = 1, b = 2)
    result <- iaw$rename.columns(df, c(z = "x"))  # z doesn't exist
    expect_equal(names(result), c("a", "b"))  # unchanged
})

test_that("iaw$rename.columns rejects NULL input", {
    expect_error(iaw$rename.columns(NULL, c(a = "x")))
})

# check.names tests
test_that("iaw$check.names passes for existing names", {
    df <- data.frame(a = 1, b = 2, c = 3)
    expect_silent(iaw$check.names(c("a", "b"), df))
})

test_that("iaw$check.names returns invisible TRUE", {
    df <- data.frame(a = 1)
    expect_invisible(iaw$check.names("a", df))
})

test_that("iaw$check.names fails for missing names", {
    df <- data.frame(a = 1, b = 2)
    expect_error(iaw$check.names(c("a", "x"), df))
})

test_that("iaw$check.names handles single name", {
    df <- data.frame(a = 1)
    expect_silent(iaw$check.names("a", df))
})

test_that("iaw$check.names handles all names", {
    df <- data.frame(a = 1, b = 2, c = 3)
    expect_silent(iaw$check.names(names(df), df))
})

test_that("iaw$check.names error message lists missing", {
    df <- data.frame(a = 1)
    expect_error(iaw$check.names(c("x", "y"), df), "x")
})

test_that("iaw$check.names handles empty wanted", {
    df <- data.frame(a = 1)
    expect_silent(iaw$check.names(character(0), df))
})

# Failing tests
test_that("iaw$check.names rejects non-data.frame", {
    expect_error(iaw$check.names("a", c(a = 1)))
})

test_that("iaw$check.names rejects non-character wanted", {
    df <- data.frame(a = 1)
    expect_error(iaw$check.names(1, df))
})

test_that("iaw$check.names rejects NULL df", {
    expect_error(iaw$check.names("a", NULL))
})

# wide2long tests
test_that("iaw$wide2long reshapes correctly", {
    mat <- matrix(1:6, nrow = 2)
    rownames(mat) <- c("r1", "r2")
    colnames(mat) <- c("c1", "c2", "c3")
    result <- iaw$wide2long(mat)
    expect_s3_class(result, "data.frame")
})

test_that("iaw$wide2long has correct dimensions", {
    mat <- matrix(1:6, nrow = 2)
    rownames(mat) <- c("r1", "r2")
    colnames(mat) <- c("c1", "c2", "c3")
    result <- iaw$wide2long(mat)
    expect_equal(nrow(result), 6)
})

test_that("iaw$wide2long custom column names", {
    mat <- matrix(1:4, nrow = 2)
    rownames(mat) <- c("r1", "r2")
    colnames(mat) <- c("c1", "c2")
    result <- iaw$wide2long(mat, valname.is = "value", row.is = "row", col.is = "col")
    expect_true("value" %in% names(result))
})

test_that("iaw$wide2long handles data frame input", {
    df <- data.frame(c1 = 1:2, c2 = 3:4, row.names = c("r1", "r2"))
    result <- iaw$wide2long(df)
    expect_s3_class(result, "data.frame")
})

test_that("iaw$wide2long preserves values", {
    mat <- matrix(1:4, nrow = 2)
    rownames(mat) <- c("r1", "r2")
    colnames(mat) <- c("c1", "c2")
    result <- iaw$wide2long(mat)
    expect_true(all(1:4 %in% result$val))
})

test_that("iaw$wide2long default column names", {
    mat <- matrix(1:4, nrow = 2)
    rownames(mat) <- c("r1", "r2")
    colnames(mat) <- c("c1", "c2")
    result <- iaw$wide2long(mat)
    expect_true("val" %in% names(result))
})

test_that("iaw$wide2long has 3 columns", {
    mat <- matrix(1:4, nrow = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
    result <- iaw$wide2long(mat)
    expect_equal(ncol(result), 3)
})

# Failing tests
test_that("iaw$wide2long rejects vector", {
    expect_error(iaw$wide2long(1:10))
})

test_that("iaw$wide2long rejects non-character valname", {
    mat <- matrix(1:4, nrow = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
    expect_error(iaw$wide2long(mat, valname.is = 123))
})

test_that("iaw$wide2long rejects NULL", {
    expect_error(iaw$wide2long(NULL))
})

# long2wide tests
test_that("iaw$long2wide reshapes correctly", {
    df <- data.frame(firm = c("A","A","B","B"), year = c(1,2,1,2), val = 1:4)
    result <- iaw$long2wide(df, "year", "firm", "val")
    expect_s3_class(result, "data.frame")
})

test_that("iaw$long2wide correct dimensions", {
    df <- data.frame(firm = c("A","A","B","B"), year = c(1,2,1,2), val = 1:4)
    result <- iaw$long2wide(df, "year", "firm", "val")
    expect_equal(nrow(result), 2)
})

test_that("iaw$long2wide preserves values", {
    df <- data.frame(firm = c("A","A"), year = c(1,2), val = c(10, 20))
    result <- iaw$long2wide(df, "year", "firm", "val")
    expect_true(10 %in% unlist(result))
})

test_that("iaw$long2wide uses firm as rownames", {
    df <- data.frame(firm = c("A","A","B","B"), year = c(1,2,1,2), val = 1:4)
    result <- iaw$long2wide(df, "year", "firm", "val")
    expect_true("A" %in% rownames(result))
})

test_that("iaw$long2wide creates column per time", {
    df <- data.frame(firm = c("A","A","A"), year = c(1,2,3), val = 1:3)
    result <- iaw$long2wide(df, "year", "firm", "val")
    expect_equal(ncol(result), 3)
})

test_that("iaw$long2wide handles numeric ids", {
    df <- data.frame(firm = c(1,1,2,2), year = c(1,2,1,2), val = 1:4)
    result <- iaw$long2wide(df, "year", "firm", "val")
    expect_equal(nrow(result), 2)
})

test_that("iaw$long2wide returns data frame", {
    df <- data.frame(firm = c("A","A"), year = c(1,2), val = 1:2)
    expect_s3_class(iaw$long2wide(df, "year", "firm", "val"), "data.frame")
})

# Failing tests
test_that("iaw$long2wide rejects missing column", {
    df <- data.frame(firm = 1:2, year = 1:2, val = 1:2)
    expect_error(iaw$long2wide(df, "missing", "firm", "val"))
})

test_that("iaw$long2wide rejects non-data.frame", {
    expect_error(iaw$long2wide(1:10, "a", "b", "c"))
})

test_that("iaw$long2wide rejects non-character column names", {
    df <- data.frame(firm = 1:2, year = 1:2, val = 1:2)
    expect_error(iaw$long2wide(df, 1, "firm", "val"))
})

# multimerge tests
test_that("iaw$multimerge merges two data frames", {
    df1 <- data.frame(id = 1:3, x = 1:3)
    df2 <- data.frame(id = 1:3, y = 4:6)
    result <- iaw$multimerge(list(df1, df2), by = "id")
    expect_s3_class(result, "data.frame")
})

test_that("iaw$multimerge merges three data frames", {
    df1 <- data.frame(id = 1:2, x = 1:2)
    df2 <- data.frame(id = 1:2, y = 3:4)
    df3 <- data.frame(id = 1:2, z = 5:6)
    result <- iaw$multimerge(list(df1, df2, df3), by = "id")
    expect_equal(ncol(result), 4)
})

test_that("iaw$multimerge preserves all columns", {
    df1 <- data.frame(id = 1:2, x = 1:2)
    df2 <- data.frame(id = 1:2, y = 3:4)
    result <- iaw$multimerge(list(df1, df2), by = "id")
    expect_true(all(c("id", "x", "y") %in% names(result)))
})

test_that("iaw$multimerge handles single data frame", {
    df1 <- data.frame(id = 1:3, x = 1:3)
    result <- iaw$multimerge(list(df1), by = "id")
    expect_equal(result, df1)
})

test_that("iaw$multimerge handles inner join", {
    df1 <- data.frame(id = 1:3, x = 1:3)
    df2 <- data.frame(id = 2:4, y = 1:3)
    result <- iaw$multimerge(list(df1, df2), by = "id")
    expect_equal(nrow(result), 2)
})

test_that("iaw$multimerge returns data frame", {
    df1 <- data.frame(id = 1:2, x = 1:2)
    df2 <- data.frame(id = 1:2, y = 3:4)
    expect_s3_class(iaw$multimerge(list(df1, df2), by = "id"), "data.frame")
})

test_that("iaw$multimerge handles character keys", {
    df1 <- data.frame(id = c("a", "b"), x = 1:2)
    df2 <- data.frame(id = c("a", "b"), y = 3:4)
    result <- iaw$multimerge(list(df1, df2), by = "id")
    expect_equal(nrow(result), 2)
})

# Failing tests
test_that("iaw$multimerge rejects non-list", {
    expect_error(iaw$multimerge(data.frame(a = 1), by = "a"))
})

test_that("iaw$multimerge rejects list of non-data.frames", {
    expect_error(iaw$multimerge(list(1:10, 1:10), by = "a"))
})

test_that("iaw$multimerge rejects empty list", {
    expect_error(iaw$multimerge(list(), by = "a"))
})
