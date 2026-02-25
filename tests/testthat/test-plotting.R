# Tests for plotting functions: hline, vline, colorblind, plot.pdf.start/end, etc.

# hline tests
test_that("iaw$hline returns invisible NULL", {
    pdf(NULL)  # Open null device
    plot(1:10)
    expect_invisible(iaw$hline(5))
    dev.off()
})

test_that("iaw$hline handles multiple lines", {
    pdf(NULL)
    plot(1:10)
    expect_silent(iaw$hline(c(3, 5, 7)))
    dev.off()
})

test_that("iaw$hline handles color argument", {
    pdf(NULL)
    plot(1:10)
    expect_silent(iaw$hline(5, col = "red"))
    dev.off()
})

test_that("iaw$hline handles linetype", {
    pdf(NULL)
    plot(1:10)
    expect_silent(iaw$hline(5, lty = 2))
    dev.off()
})

test_that("iaw$hline handles xrange", {
    pdf(NULL)
    plot(1:10)
    expect_silent(iaw$hline(5, xrange = c(2, 8)))
    dev.off()
})

test_that("iaw$hline handles negative y", {
    pdf(NULL)
    plot(-10:10, -10:10)
    expect_silent(iaw$hline(-5))
    dev.off()
})

test_that("iaw$hline handles decimal y", {
    pdf(NULL)
    plot(1:10)
    expect_silent(iaw$hline(5.5))
    dev.off()
})

# Failing tests
test_that("iaw$hline rejects non-numeric y", {
    pdf(NULL)
    plot(1:10)
    expect_error(iaw$hline("five"))
    dev.off()
})

test_that("iaw$hline rejects NULL y", {
    pdf(NULL)
    plot(1:10)
    expect_error(iaw$hline(NULL))
    dev.off()
})

test_that("iaw$hline rejects character vector", {
    pdf(NULL)
    plot(1:10)
    expect_error(iaw$hline(c("a", "b")))
    dev.off()
})

# vline tests
test_that("iaw$vline returns invisible NULL", {
    pdf(NULL)
    plot(1:10)
    expect_invisible(iaw$vline(5))
    dev.off()
})

test_that("iaw$vline handles multiple lines", {
    pdf(NULL)
    plot(1:10)
    expect_silent(iaw$vline(c(3, 5, 7)))
    dev.off()
})

test_that("iaw$vline handles color argument", {
    pdf(NULL)
    plot(1:10)
    expect_silent(iaw$vline(5, col = "blue"))
    dev.off()
})

test_that("iaw$vline handles linetype", {
    pdf(NULL)
    plot(1:10)
    expect_silent(iaw$vline(5, lty = 3))
    dev.off()
})

test_that("iaw$vline handles yrange", {
    pdf(NULL)
    plot(1:10)
    expect_silent(iaw$vline(5, yrange = c(2, 8)))
    dev.off()
})

test_that("iaw$vline handles negative x", {
    pdf(NULL)
    plot(-10:10, -10:10)
    expect_silent(iaw$vline(-5))
    dev.off()
})

test_that("iaw$vline handles decimal x", {
    pdf(NULL)
    plot(1:10)
    expect_silent(iaw$vline(5.5))
    dev.off()
})

# Failing tests
test_that("iaw$vline rejects non-numeric x", {
    pdf(NULL)
    plot(1:10)
    expect_error(iaw$vline("five"))
    dev.off()
})

test_that("iaw$vline rejects NULL x", {
    pdf(NULL)
    plot(1:10)
    expect_error(iaw$vline(NULL))
    dev.off()
})

test_that("iaw$vline rejects character vector", {
    pdf(NULL)
    plot(1:10)
    expect_error(iaw$vline(c("a", "b")))
    dev.off()
})

# colorblind tests
test_that("iaw$colorblind returns character vector", {
    result <- iaw$colorblind(5)
    expect_type(result, "character")
})

test_that("iaw$colorblind returns requested number", {
    result <- iaw$colorblind(5)
    expect_length(result, 5)
})

test_that("iaw$colorblind default is 8", {
    result <- iaw$colorblind()
    expect_length(result, 8)
})

test_that("iaw$colorblind includes black", {
    result <- iaw$colorblind(8)
    expect_true("black" %in% result)
})

test_that("iaw$colorblind handles n > 8", {
    result <- iaw$colorblind(10)
    expect_length(result, 10)
})

test_that("iaw$colorblind handles n = 1", {
    result <- iaw$colorblind(1)
    expect_length(result, 1)
})

test_that("iaw$colorblind colors are valid", {
    result <- iaw$colorblind(5)
    # All should be valid color names
    expect_true(all(result %in% colors() | grepl("^#", result) | result %in% c("black", "orange", "skyblue", "green", "yellow", "blue", "red", "gray")))
})

# Failing tests
test_that("iaw$colorblind rejects n <= 0", {
    expect_error(iaw$colorblind(0))
})

test_that("iaw$colorblind rejects non-numeric n", {
    expect_error(iaw$colorblind("five"))
})

test_that("iaw$colorblind rejects negative n", {
    expect_error(iaw$colorblind(-5))
})

# pdf.start/pdf.end tests (iaw$pdf.start uses wd/ht not width/height)
test_that("iaw$pdf.start creates file", {
    tmpfile <- tempfile(fileext = ".pdf")
    on.exit({ try(dev.off(), silent = TRUE); unlink(tmpfile) })
    iaw$pdf.start(tmpfile, verbose = FALSE)
    plot(1:10)
    iaw$pdf.end(verbose = FALSE)
    expect_true(file.exists(tmpfile))
})

test_that("iaw$pdf.end closes device", {
    tmpfile <- tempfile(fileext = ".pdf")
    on.exit({ try(dev.off(), silent = TRUE); unlink(tmpfile) })
    iaw$pdf.start(tmpfile, verbose = FALSE)
    expect_no_error(iaw$pdf.end(verbose = FALSE))
})

test_that("iaw$pdf.start respects wd/ht", {
    tmpfile <- tempfile(fileext = ".pdf")
    on.exit({ try(dev.off(), silent = TRUE); unlink(tmpfile) })
    iaw$pdf.start(tmpfile, wd = 10, ht = 5, verbose = FALSE)
    plot(1:10)
    iaw$pdf.end(verbose = FALSE)
    expect_true(file.exists(tmpfile))
})

# Failing tests
test_that("iaw$pdf.start rejects non-character filename", {
    expect_error(iaw$pdf.start(123))
})

test_that("iaw$pdf.start rejects vector filename", {
    expect_error(iaw$pdf.start(c("a.pdf", "b.pdf")))
})

# MixColor tests
test_that("iaw$mixcolor returns color", {
    result <- iaw$mixcolor("red", "blue")
    expect_type(result, "character")
})

test_that("iaw$mixcolor mixes colors", {
    result <- iaw$mixcolor("white", "black")
    expect_type(result, "character")
})

test_that("iaw$mixcolor handles named colors", {
    result <- iaw$mixcolor("red", "green")
    expect_type(result, "character")
})

test_that("iaw$mixcolor single color output", {
    result <- iaw$mixcolor("blue", "yellow")
    expect_length(result, 1)
})

test_that("iaw$mixcolor with ratio", {
    result <- iaw$mixcolor("red", "blue", ratio = 0.5)
    expect_type(result, "character")
})

test_that("iaw$mixcolor same colors", {
    result <- iaw$mixcolor("red", "red")
    expect_type(result, "character")
})

test_that("iaw$mixcolor returns valid color", {
    result <- iaw$mixcolor("red", "blue")
    # Should be a hex color or named color
    expect_true(nchar(result) > 0)
})

# plot.native.slope tests
test_that("iaw$native.slope returns numeric", {
    pdf(NULL)
    plot(1:10, 1:10)
    result <- iaw$native.slope(1)
    dev.off()
    expect_type(result, "double")
})

test_that("iaw$native.slope handles zero slope", {
    pdf(NULL)
    plot(1:10)
    result <- iaw$native.slope(0)
    dev.off()
    expect_equal(result, 0)
})

test_that("iaw$native.slope handles negative slope", {
    pdf(NULL)
    plot(1:10)
    result <- iaw$native.slope(-1)
    dev.off()
    expect_true(result < 0)
})

test_that("iaw$native.slope handles steep slope", {
    pdf(NULL)
    plot(1:10)
    result <- iaw$native.slope(10)
    dev.off()
    expect_true(abs(result) > 45)
})

test_that("iaw$native.slope returns angle in degrees", {
    pdf(NULL)
    plot(1:10, 1:10, asp = 1)
    result <- iaw$native.slope(1)
    dev.off()
    expect_true(result > 0 && result < 90)
})

test_that("iaw$native.slope single value", {
    pdf(NULL)
    plot(1:10)
    result <- iaw$native.slope(0.5)
    dev.off()
    expect_length(result, 1)
})

test_that("iaw$native.slope returns finite", {
    pdf(NULL)
    plot(1:10)
    result <- iaw$native.slope(2)
    dev.off()
    expect_true(is.finite(result))
})

# Failing tests
test_that("iaw$native.slope rejects non-numeric", {
    pdf(NULL)
    plot(1:10)
    expect_error(iaw$native.slope("steep"))
    dev.off()
})

test_that("iaw$native.slope rejects NULL", {
    pdf(NULL)
    plot(1:10)
    expect_error(iaw$native.slope(NULL))
    dev.off()
})

test_that("iaw$native.slope rejects vector", {
    pdf(NULL)
    plot(1:10)
    expect_error(iaw$native.slope(c(1, 2)))
    dev.off()
})
