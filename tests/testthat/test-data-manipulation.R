# Tests for data manipulation functions: doublesort, recode, rename.columns,
# kill.useless.cols, long2wide, wide2long, merge4seq, multimerge,
# lagdataframe, colN, colSds, rowSds, diffid.for.fixed.effects

## ---------------------------------------------------------------------------
## doublesort
## ---------------------------------------------------------------------------

test_that("doublesort returns correct ranks from inline test case", {
    crit1 <- c(4, 9, 13, 11, 5, 7, 10, 1, 12, 8, 3, 6, 2)
    crit2 <- c(-1, 10, 104, 103, 22, -99, 102, 1, 105, 11, 2, 33, -2)
    result <- iaw$doublesort(crit1, crit2, NUMPERGROUP = 4)
    expect_equal(result, c(2, 1, 3, 2, 3, NA, 1, 3, 4, 2, 4, 4, 1))
})

test_that("doublesort .mkusefullist handles exact divisibility", {
    expect_equal(iaw$.mkusefullist(9, 3), 1:9)
})

test_that("doublesort .mkusefullist inserts NAs for remainder", {
    expect_equal(iaw$.mkusefullist(10, 3), c(1:4, NA, 5:9))
    expect_equal(iaw$.mkusefullist(11, 3), c(1:4, NA, NA, 5:9))
})

test_that("doublesort rejects mismatched lengths", {
    expect_error(iaw$doublesort(1:10, 1:9, NUMPERGROUP = 3))
})

test_that("doublesort rejects NUMPERGROUP <= 1", {
    expect_error(iaw$doublesort(1:10, 1:10, NUMPERGROUP = 1))
})

## ---------------------------------------------------------------------------
## recode
## ---------------------------------------------------------------------------

test_that("recode replaces matched values and preserves others", {
    x <- c(1, 2, 3, 1, 2)
    expect_equal(iaw$recode(x, c(1, 2), c(10, 20)), c(10, 20, 3, 10, 20))
})

test_that("recode with non.from.becomes.na sets unmatched to NA", {
    x <- c(1, 2, 3)
    result <- iaw$recode(x, c(1, 2), c(10, 20), non.from.becomes.na = TRUE)
    expect_equal(result, c(10, 20, NA))
})

test_that("recode works with character vectors", {
    x <- c("a", "b", "c")
    expect_equal(iaw$recode(x, c("a", "c"), c("X", "Z")), c("X", "b", "Z"))
})

test_that("recode rejects mismatched from/to lengths", {
    expect_error(iaw$recode(1:3, c(1, 2), c(10)))
})

## ---------------------------------------------------------------------------
## rename.columns
## ---------------------------------------------------------------------------

test_that("rename.columns renames with from/to vectors", {
    df <- data.frame(a = 1:3, b = 4:6)
    result <- iaw$rename.columns(df, c("a", "b"), c("x", "y"))
    expect_equal(names(result), c("x", "y"))
    expect_equal(result$x, 1:3)
})

test_that("rename.columns renames with named vector syntax", {
    df <- data.frame(a = 1, b = 2, c = 3)
    result <- iaw$rename.columns(df, c(a = "alpha", c = "gamma"))
    expect_equal(names(result), c("alpha", "b", "gamma"))
})

test_that("rename.column alias works identically", {
    df <- data.frame(x = 10)
    expect_equal(names(iaw$rename.column(df, c(x = "y"))), "y")
})

test_that("rename.columns rejects NULL input", {
    expect_error(iaw$rename.columns(NULL, c(a = "x")))
})

## ---------------------------------------------------------------------------
## kill.useless.cols
## ---------------------------------------------------------------------------

test_that("kill.useless.cols removes constant columns", {
    df <- data.frame(a = 1:3, b = rep(5, 3), c = c(1, 2, 1))
    result <- iaw$kill.useless.cols(df)
    expect_equal(names(result), c("a", "c"))
})

test_that("kill.useless.cols keeps all columns when none constant", {
    df <- data.frame(x = 1:3, y = c(10, 20, 30))
    result <- iaw$kill.useless.cols(df)
    expect_equal(ncol(result), 2)
})

test_that("kill.useless.cols returns zero-column df when all constant", {
    df <- data.frame(a = rep(1, 3), b = rep(2, 3))
    result <- iaw$kill.useless.cols(df)
    expect_equal(ncol(result), 0)
    expect_equal(nrow(result), 3)
})

test_that("kill.useless.cols rejects non-data.frame", {
    expect_error(iaw$kill.useless.cols(matrix(1:4, 2, 2)))
})

## ---------------------------------------------------------------------------
## long2wide
## ---------------------------------------------------------------------------

test_that("long2wide reshapes long to wide", {
    df <- data.frame(
        firm = c("A", "A", "B", "B"),
        year = c(1, 2, 1, 2),
        val  = c(10, 20, 30, 40)
    )
    result <- iaw$long2wide(df, "year", "firm", "val")
    expect_equal(nrow(result), 2)
    expect_equal(ncol(result), 2)
    expect_true("A" %in% rownames(result))
    expect_true("B" %in% rownames(result))
})

test_that("long2wide rejects missing column name", {
    df <- data.frame(firm = 1:2, year = 1:2, val = 1:2)
    expect_error(iaw$long2wide(df, "nosuch", "firm", "val"))
})

## ---------------------------------------------------------------------------
## wide2long
## ---------------------------------------------------------------------------

test_that("wide2long reshapes matrix to long data frame", {
    mat <- matrix(1:6, nrow = 2,
                  dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
    result <- iaw$wide2long(mat)
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 6)
    expect_equal(ncol(result), 3)
    expect_true(all(c("time", "unit", "val") %in% names(result)))
    expect_true(all(1:6 %in% result$unit))
})

test_that("wide2long custom column names", {
    mat <- matrix(1:4, nrow = 2,
                  dimnames = list(c("r1", "r2"), c("c1", "c2")))
    result <- iaw$wide2long(mat, valname.is = "value", row.is = "row", col.is = "col")
    expect_equal(names(result), c("row", "col", "value"))
})

test_that("wide2long rejects plain vector", {
    expect_error(iaw$wide2long(1:5))
})

## ---------------------------------------------------------------------------
## merge4seq
## ---------------------------------------------------------------------------

test_that("merge4seq preserves row order of first data frame", {
    x <- data.frame(id = c(3, 1, 2), val_x = c(30, 10, 20))
    y <- data.frame(id = c(1, 2, 3), val_y = c(100, 200, 300))
    result <- iaw$merge4seq(x, y, by = "id")
    expect_equal(result$id, c(3, 1, 2))
    expect_equal(result$val_x, c(30, 10, 20))
    expect_equal(result$val_y, c(300, 100, 200))
})

test_that("merge4seq handles partial overlap (inner join by default)", {
    x <- data.frame(id = c(1, 2, 3), v = c(10, 20, 30))
    y <- data.frame(id = c(2, 3, 4), w = c(200, 300, 400))
    result <- iaw$merge4seq(x, y, by = "id")
    expect_equal(nrow(result), 2)
    # order of x preserved for matching rows
    expect_equal(result$id, c(2, 3))
})

test_that("merge4seq rejects non-data.frame inputs", {
    expect_error(iaw$merge4seq(1:3, data.frame(id = 1:3), by = "id"))
})

## ---------------------------------------------------------------------------
## multimerge
## ---------------------------------------------------------------------------

test_that("multimerge merges three data frames", {
    df1 <- data.frame(id = 1:3, a = c(10, 20, 30))
    df2 <- data.frame(id = 1:3, b = c(40, 50, 60))
    df3 <- data.frame(id = 1:3, c = c(70, 80, 90))
    result <- iaw$multimerge(list(df1, df2, df3), by = "id")
    expect_equal(ncol(result), 4)
    expect_true(all(c("id", "a", "b", "c") %in% names(result)))
})

test_that("multimerge requires at least two data frames", {
    expect_error(iaw$multimerge(list(data.frame(id = 1)), by = "id"))
})

test_that("multimerge rejects list of non-data.frames", {
    expect_error(iaw$multimerge(list(1:5, 1:5), by = "x"))
})

## ---------------------------------------------------------------------------
## lagdataframe
## ---------------------------------------------------------------------------

test_that("lagdataframe adds lagged columns with correct names", {
    df <- data.frame(t = 1:5, x = c(10, 20, 30, 40, 50), y = c(5, 4, 3, 2, 1))
    result <- iaw$lagdataframe(df, vars = c("x", "y"), nlags = 1)
    expect_true("x.L1" %in% names(result))
    expect_true("y.L1" %in% names(result))
    expect_equal(result$x.L1, c(NA, 10, 20, 30, 40))
    expect_equal(result$y.L1, c(NA, 5, 4, 3, 2))
})

test_that("lagdataframe with nlags=2 names columns accordingly", {
    df <- data.frame(v = c(1, 2, 3, 4, 5))
    result <- iaw$lagdataframe(df, vars = "v", nlags = 2)
    expect_true("v.L2" %in% names(result))
    expect_equal(result$v.L2, c(NA, NA, 1, 2, 3))
})

test_that("lagdataframe rejects missing variable name", {
    df <- data.frame(a = 1:5)
    expect_error(iaw$lagdataframe(df, vars = "nonexistent"))
})

## ---------------------------------------------------------------------------
## colN
## ---------------------------------------------------------------------------

test_that("colN counts non-NA values per column", {
    df <- data.frame(a = c(1, NA, 3), b = c(1, 2, 3), c = c(NA, NA, NA))
    result <- iaw$colN(df)
    expect_equal(as.integer(result), c(2L, 3L, 0L))
    expect_equal(names(result), c("a", "b", "c"))
})

test_that("colN on all-complete data returns full counts", {
    m <- matrix(1:6, nrow = 3, ncol = 2, dimnames = list(NULL, c("x", "y")))
    result <- iaw$colN(m)
    expect_equal(as.integer(result), c(3L, 3L))
})

## ---------------------------------------------------------------------------
## colSds
## ---------------------------------------------------------------------------

test_that("colSds computes column standard deviations", {
    m <- matrix(c(1, 2, 3, 10, 20, 30), nrow = 3, ncol = 2)
    result <- iaw$colSds(m)
    expect_equal(result[1], sd(c(1, 2, 3)), tolerance = 1e-10)
    expect_equal(result[2], sd(c(10, 20, 30)), tolerance = 1e-10)
})

test_that("colSds handles NA with na.rm=TRUE", {
    m <- matrix(c(1, NA, 3, 4, 5, 6), nrow = 3, ncol = 2)
    result <- iaw$colSds(m, na.rm = TRUE)
    expect_equal(result[1], sd(c(1, 3), na.rm = TRUE), tolerance = 1e-10)
})

## ---------------------------------------------------------------------------
## rowSds
## ---------------------------------------------------------------------------

test_that("rowSds computes row standard deviations", {
    m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
    # row 1: c(1,3,5), row 2: c(2,4,6)
    result <- iaw$rowSds(m)
    expect_equal(result[1], sd(c(1, 3, 5)), tolerance = 1e-10)
    expect_equal(result[2], sd(c(2, 4, 6)), tolerance = 1e-10)
})

test_that("rowSds preserves rownames", {
    m <- matrix(1:6, nrow = 2, ncol = 3, dimnames = list(c("A", "B"), NULL))
    result <- iaw$rowSds(m)
    expect_equal(names(result), c("A", "B"))
})

## ---------------------------------------------------------------------------
## diffid.for.fixed.effects
## ---------------------------------------------------------------------------

test_that("diffid.for.fixed.effects demeans vector by group", {
    h  <- c(10, 20, 30, 100, 200, 300)
    id <- c("a", "a", "a", "b", "b", "b")
    result <- iaw$diffid.for.fixed.effects(h, id)
    # group a mean = 20, group b mean = 200
    expect_equal(as.numeric(result), c(-10, 0, 10, -100, 0, 100))
})

test_that("diffid.for.fixed.effects demeans matrix columns independently", {
    h <- matrix(c(1, 3, 5, 10, 20, 30), nrow = 3, ncol = 2)
    id <- c("x", "x", "y")
    result <- iaw$diffid.for.fixed.effects(h, id)
    # col1 group x: mean(1,3)=2, group y: mean(5)=5
    # col2 group x: mean(10,20)=15, group y: mean(30)=30
    expect_equal(result[1, 1], 1 - 2)
    expect_equal(result[2, 1], 3 - 2)
    expect_equal(result[3, 1], 5 - 5)
    expect_equal(result[1, 2], 10 - 15)
    expect_equal(result[3, 2], 30 - 30)
})

test_that("diffid.for.fixed.effects single group yields zero-mean", {
    h  <- c(5, 10, 15)
    id <- c("g", "g", "g")
    result <- as.numeric(iaw$diffid.for.fixed.effects(h, id))
    expect_equal(mean(result), 0)
    expect_equal(result, c(-5, 0, 5))
})
