# Tests for %and%, %or%, %inrange% operators

test_that("%and% executes on TRUE condition", {
    x <- 5
    result <- NULL
    (x > 3) %and% { result <- "executed" }
    expect_equal(result, "executed")
})

test_that("%and% does not execute on FALSE condition", {
    x <- 1
    result <- "not changed"
    (x > 3) %and% { result <- "changed" }
    expect_equal(result, "not changed")
})

test_that("%and% returns invisible NULL", {
    expect_invisible((TRUE) %and% message("test"))
})

test_that("%and% works with expression blocks", {
    x <- 0
    (TRUE) %and% { x <- x + 1; x <- x + 1 }
    expect_equal(x, 2)
})

# CORRECT - use fixed matching
test_that("%and% handles NA correctly", {
    expect_error((NA) %and% stop("test"))  # Just check it errors, don't match message
})


# Failing tests - parameter validation
test_that("%and% rejects non-logical first argument", {
    expect_error(("not logical") %and% stop("test"))
})

test_that("%and% rejects vector logical", {
    expect_error((c(TRUE, FALSE)) %and% stop("test"))
})

test_that("%and% rejects NULL", {
    expect_error((NULL) %and% stop("test"))
})

# %or% tests
test_that("%or% executes on FALSE condition", {
    x <- 1
    result <- NULL
    (x > 3) %or% { result <- "executed" }
    expect_equal(result, "executed")
})

test_that("%or% does not execute on TRUE condition", {
    x <- 5
    result <- "not changed"
    (x > 3) %or% { result <- "changed" }
    expect_equal(result, "not changed")
})

test_that("%or% returns invisible NULL", {
    expect_invisible((FALSE) %or% message("test"))
})

test_that("%or% aborts with character on FALSE", {
    expect_error((FALSE) %or% stop("error message"))
})

test_that("%or% handles TRUE correctly", {
    expect_silent((TRUE) %or% stop("no error"))
})

test_that("%or% works for assertions", {
    x <- 5
    expect_silent((is.numeric(x)) %or% stop("x must be numeric"))
})

test_that("%or% catches type errors", {
    x <- "text"
    expect_error((is.numeric(x)) %or% stop("x must be numeric"))
})

# Failing tests
test_that("%or% rejects non-logical", {
    expect_error((5) %or% stop("test"))
})

test_that("%or% rejects vector", {
    expect_error((c(TRUE, TRUE)) %or% stop("test"))
})

test_that("%or% rejects NA", {
    expect_error((NA) %or% stop("test"))
})

# %inrange% tests
test_that("%inrange% returns TRUE for value in range", {
    expect_true(5 %inrange% c(1, 10))
})

test_that("%inrange% returns FALSE for value outside range", {
    expect_false(15 %inrange% c(1, 10))
})

test_that("%inrange% is inclusive on boundaries", {
    expect_true(1 %inrange% c(1, 10))
    expect_true(10 %inrange% c(1, 10))
})

test_that("%inrange% works with vectors", {
    result <- c(-5, 0, 5, 10, 15) %inrange% c(0, 10)
    expect_equal(result, c(FALSE, TRUE, TRUE, TRUE, FALSE))
})

test_that("%inrange% handles negative ranges", {
    expect_true(-5 %inrange% c(-10, 0))
})

test_that("%inrange% handles decimal values", {
    expect_true(5.5 %inrange% c(5.0, 6.0))
})

test_that("%inrange% returns logical vector", {
    expect_type(1:5 %inrange% c(2, 4), "logical")
})

# Failing tests
test_that("%inrange% rejects non-numeric x", {
    expect_error("a" %inrange% c(1, 10))
})

test_that("%inrange% rejects wrong length range", {
    expect_error(5 %inrange% c(1, 5, 10))
})

test_that("%inrange% rejects non-numeric range", {
    expect_error(5 %inrange% c("a", "b"))
})
