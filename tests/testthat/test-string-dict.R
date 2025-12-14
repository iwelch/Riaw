# Tests for string and dictionary utilities: dict.lookup, mkRdictionary, grepcolname, require.variables

# dict.lookup tests
test_that("iaw$dict.lookup finds values", {
    d <- c(a = 1, b = 2, c = 3)
    result <- iaw$dict.lookup(c("a", "c"), d)
    expect_equal(as.numeric(result), c(1, 3))
})

test_that("iaw$dict.lookup returns NA for missing", {
    d <- c(a = 1, b = 2)
    result <- iaw$dict.lookup("x", d)
    expect_true(is.na(result))
})

test_that("iaw$dict.lookup custom default", {
    d <- c(a = 1)
    result <- iaw$dict.lookup("x", d, default = 0)
    expect_equal(as.numeric(result), 0)
})

test_that("iaw$dict.lookup handles all found", {
    d <- c(a = 1, b = 2, c = 3)
    result <- iaw$dict.lookup(c("a", "b", "c"), d)
    expect_equal(as.numeric(result), c(1, 2, 3))
})

test_that("iaw$dict.lookup handles all missing", {
    d <- c(a = 1)
    result <- iaw$dict.lookup(c("x", "y"), d)
    expect_true(all(is.na(result)))
})

test_that("iaw$dict.lookup single key", {
    d <- c(a = 100)
    result <- iaw$dict.lookup("a", d)
    expect_equal(as.numeric(result), 100)
})

test_that("iaw$dict.lookup preserves names", {
    d <- c(a = 1, b = 2)
    result <- iaw$dict.lookup(c("a", "b"), d)
    expect_true(!is.null(names(result)))
})

test_that("iaw$dict.lookup handles numeric keys", {
    d <- c("1" = 10, "2" = 20)
    result <- iaw$dict.lookup(c(1, 2), d)
    expect_equal(as.numeric(result), c(10, 20))
})

test_that("iaw$dict.lookup returns vector", {
    d <- c(a = 1, b = 2)
    result <- iaw$dict.lookup(c("a", "b"), d)
    expect_true(is.vector(result))
})

test_that("iaw$dict.lookup handles mixed found/missing", {
    d <- c(a = 1, b = 2)
    result <- iaw$dict.lookup(c("a", "x", "b"), d)
    expect_true(is.na(result[2]))
})

# mkRdictionary tests
test_that("iaw$mkRdictionary creates named vector", {
    result <- iaw$mkRdictionary(c("a", "b"), c(1, 2))
    expect_true(!is.null(names(result)))
})

test_that("iaw$mkRdictionary correct names", {
    result <- iaw$mkRdictionary(c("x", "y"), c(10, 20))
    expect_equal(names(result), c("x", "y"))
})

test_that("iaw$mkRdictionary correct values", {
    result <- iaw$mkRdictionary(c("a", "b"), c(1, 2))
    expect_equal(as.numeric(result), c(1, 2))
})

test_that("iaw$mkRdictionary single pair", {
    result <- iaw$mkRdictionary("key", "value")
    expect_equal(result["key"], c(key = "value"))
})

test_that("iaw$mkRdictionary works with lookup", {
    d <- iaw$mkRdictionary(c("a", "b"), c(1, 2))
    expect_equal(as.numeric(d["a"]), 1)
})

test_that("iaw$mkRdictionary preserves value types", {
    result <- iaw$mkRdictionary(c("a", "b"), c(1.5, 2.5))
    expect_type(result, "double")
})

test_that("iaw$mkRdictionary character values", {
    result <- iaw$mkRdictionary(c("a", "b"), c("x", "y"))
    expect_type(result, "character")
})

# Failing tests
test_that("iaw$mkRdictionary rejects mismatched lengths", {
    expect_error(iaw$mkRdictionary(c("a", "b"), c(1)))
})

test_that("iaw$mkRdictionary rejects NULL keys", {
    expect_error(iaw$mkRdictionary(NULL, c(1, 2)))
})

test_that("iaw$mkRdictionary rejects empty", {
    result <- iaw$mkRdictionary(c(), c())
    expect_length(result, 0)
})

# grepcolname tests
test_that("iaw$grepcolname finds matching columns", {
    df <- data.frame(var_a = 1, var_b = 2, other = 3)
    result <- iaw$grepcolname("var", df)
    expect_equal(result, c("var_a", "var_b"))
})

test_that("iaw$grepcolname returns character vector", {
    df <- data.frame(x1 = 1, x2 = 2)
    result <- iaw$grepcolname("x", df)
    expect_type(result, "character")
})

test_that("iaw$grepcolname no matches returns empty", {
    df <- data.frame(a = 1, b = 2)
    result <- iaw$grepcolname("xyz", df)
    expect_length(result, 0)
})

test_that("iaw$grepcolname regex patterns", {
    df <- data.frame(var1 = 1, var2 = 2, other = 3)
    result <- iaw$grepcolname("^var", df)
    expect_equal(result, c("var1", "var2"))
})

test_that("iaw$grepcolname case sensitive", {
    df <- data.frame(Var = 1, var = 2)
    result <- iaw$grepcolname("^var$", df)
    expect_equal(result, "var")
})

test_that("iaw$grepcolname single match", {
    df <- data.frame(unique = 1, other = 2)
    result <- iaw$grepcolname("unique", df)
    expect_equal(result, "unique")
})

test_that("iaw$grepcolname end anchor", {
    df <- data.frame(x_ret = 1, y_ret = 2, x_vol = 3)
    result <- iaw$grepcolname("ret$", df)
    expect_equal(result, c("x_ret", "y_ret"))
})

test_that("iaw$grepcolname handles many columns", {
    df <- data.frame(matrix(1:100, nrow = 1))
    result <- iaw$grepcolname("X[0-9]", df)
    expect_true(length(result) > 0)
})

test_that("iaw$grepcolname handles special chars", {
    df <- data.frame(a.b = 1, a_c = 2)
    result <- iaw$grepcolname("\\.", df)
    expect_equal(result, "a.b")
})

test_that("iaw$grepcolname all match", {
    df <- data.frame(var1 = 1, var2 = 2, var3 = 3)
    result <- iaw$grepcolname("var", df)
    expect_length(result, 3)
})

# require.variables tests
test_that("iaw$require.variables passes for existing", {
    df <- data.frame(a = 1, b = 2, c = 3)
    expect_silent(iaw$require.variables(c("a", "b"), df))
})

test_that("iaw$require.variables returns invisible TRUE", {
    df <- data.frame(a = 1)
    expect_invisible(iaw$require.variables("a", df))
})

test_that("iaw$require.variables fails for missing", {
    df <- data.frame(a = 1)
    expect_error(iaw$require.variables(c("a", "x"), df), "x")
})

test_that("iaw$require.variables empty vars ok", {
    df <- data.frame(a = 1)
    expect_silent(iaw$require.variables(character(0), df))
})

test_that("iaw$require.variables single var", {
    df <- data.frame(test = 1)
    expect_silent(iaw$require.variables("test", df))
})

test_that("iaw$require.variables error lists missing", {
    df <- data.frame(a = 1)
    expect_error(iaw$require.variables(c("x", "y"), df), "x")
})

test_that("iaw$require.variables all required present", {
    df <- data.frame(x = 1, y = 2, z = 3)
    expect_silent(iaw$require.variables(c("x", "y", "z"), df))
})

# Failing tests
test_that("iaw$require.variables rejects non-character vars", {
    df <- data.frame(a = 1)
    expect_error(iaw$require.variables(1, df))
})

test_that("iaw$require.variables rejects non-data.frame", {
    expect_error(iaw$require.variables("a", list(a = 1)))
})

test_that("iaw$require.variables rejects NULL df", {
    expect_error(iaw$require.variables("a", NULL))
})
