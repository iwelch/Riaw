# Tests for datetime functions: epoch2yyyymmdd, epoch2hhmmss, epoch2POSIXct,
# epoch2nyc, yyyymmdd.toggle, yyyymmdd, diffdays, tmdiffsec,
# latlon.distance, mklatlonid, invlatlon, now

# --- epoch2yyyymmdd ---

test_that("epoch2yyyymmdd converts known epoch to correct date", {
    # 2021-01-01 00:00:00 UTC = 1609459200
    expect_equal(iaw$epoch2yyyymmdd(1609459200), 20210101L)
    # epoch 0 = 1970-01-01
    expect_equal(iaw$epoch2yyyymmdd(0), 19700101L)
})

test_that("epoch2yyyymmdd rejects non-numeric input", {
    expect_error(iaw$epoch2yyyymmdd("2021-01-01"))
})

# --- epoch2hhmmss ---

test_that("epoch2hhmmss converts known epoch to correct time", {
    # 2021-01-01 13:30:45 UTC = 1609459200 + 13*3600 + 30*60 + 45 = 1609507845
    expect_equal(iaw$epoch2hhmmss(1609507845), 133045L)
    # Midnight UTC
    expect_equal(iaw$epoch2hhmmss(1609459200), 0L)
})

test_that("epoch2hhmmss rejects non-numeric input", {
    expect_error(iaw$epoch2hhmmss("noon"))
})

# --- epoch2POSIXct ---

test_that("epoch2POSIXct returns POSIXct with correct timezone", {
    result <- iaw$epoch2POSIXct(1609459200, tz = "UTC")
    expect_s3_class(result, "POSIXct")
    expect_equal(attr(result, "tzone"), "UTC")
    expect_match(format(result, "%Y-%m-%d"), "2021-01-01")
})

test_that("epoch2POSIXct handles vector input", {
    result <- iaw$epoch2POSIXct(c(0, 86400))
    expect_length(result, 2)
})

# --- epoch2nyc ---

test_that("epoch2nyc returns POSIXct in America/New_York", {
    result <- iaw$epoch2nyc(1609459200)
    expect_s3_class(result, "POSIXct")
    expect_equal(attr(result, "tzone"), "America/New_York")
})

test_that("epoch2nyc differs from UTC by offset", {
    utc <- iaw$epoch2POSIXct(1609459200, tz = "UTC")
    nyc <- iaw$epoch2nyc(1609459200)
    # Formatted hour or date should differ (UTC midnight = NYC previous evening)
    expect_true(format(utc, "%H") != format(nyc, "%H") ||
                format(utc, "%d") != format(nyc, "%d"))
})

# --- yyyymmdd.toggle ---

test_that("yyyymmdd.toggle round-trips YYYYMMDD -> gregorian -> YYYYMMDD", {
    original <- 20210115
    greg <- iaw$yyyymmdd.toggle(original)
    expect_true(is.numeric(greg))
    back <- iaw$yyyymmdd.toggle(greg)
    expect_equal(back, original)
})

test_that("yyyymmdd.toggle converts known date correctly", {
    # 2021-01-15 is day 18642 since 1970-01-01
    expect_equal(iaw$yyyymmdd.toggle(20210115), as.numeric(as.Date("2021-01-15")))
})

test_that("yyyymmdd.toggle passes through all-NA", {
    result <- iaw$yyyymmdd.toggle(NA_real_)
    expect_true(is.na(result))
})

# --- yyyymmdd ---

test_that("yyyymmdd converts YYYYMMDD to weekday", {
    # 2024-01-15 is a Monday
    result <- iaw$yyyymmdd(20240115, output = "weekday")
    expect_equal(result, "Mon")
})

test_that("yyyymmdd converts YYYYMMDD to gregorian number", {
    greg <- iaw$yyyymmdd(20210115, output = "gregorian")
    expect_true(is.numeric(greg))
    # Gregorian day number should match yyyymmdd.toggle
    expect_equal(greg, iaw$yyyymmdd.toggle(20210115))
})

test_that("yyyymmdd errors if no output format given with multiple defaults", {
    # When output is not specified as a single value, it errors
    expect_error(iaw$yyyymmdd(20210115))
})

# --- diffdays ---

test_that("diffdays computes correct day difference", {
    d1 <- as.Date("2021-01-01")
    d2 <- as.Date("2021-01-15")
    expect_equal(iaw$diffdays(d1, d2), 14)
})

test_that("diffdays returns negative for reversed dates", {
    d1 <- as.Date("2021-01-15")
    d2 <- as.Date("2021-01-01")
    expect_equal(iaw$diffdays(d1, d2), -14)
})

test_that("diffdays same date is zero", {
    d <- as.Date("2021-06-15")
    expect_equal(iaw$diffdays(d, d), 0)
})

# --- tmdiffsec ---

test_that("tmdiffsec computes correct seconds", {
    t1 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
    t2 <- as.POSIXct("2021-01-01 01:00:00", tz = "UTC")
    expect_equal(iaw$tmdiffsec(t1, t2), 3600)
})

test_that("tmdiffsec handles negative difference", {
    t1 <- as.POSIXct("2021-01-01 01:00:00", tz = "UTC")
    t2 <- as.POSIXct("2021-01-01 00:00:00", tz = "UTC")
    expect_equal(iaw$tmdiffsec(t1, t2), -3600)
})

# --- latlon.distance ---

test_that("latlon.distance NYC to LA is approximately 3944 km", {
    # NYC: 40.7128, -74.0060; LA: 34.0522, -118.2437
    d <- iaw$latlon.distance(40.7128, -74.0060, 34.0522, -118.2437)
    expect_true(is.numeric(d))
    expect_equal(d, 3944, tolerance = 50)
})

test_that("latlon.distance same point is zero", {
    d <- iaw$latlon.distance(0, 0, 0, 0)
    expect_equal(d, 0)
})

# --- mklatlonid / invlatlon round-trip ---

test_that("mklatlonid and invlatlon round-trip correctly", {
    lat <- 40.5
    lon <- -74.5
    id <- iaw$mklatlonid(lat, lon)
    expect_true(is.numeric(id))
    expect_true(id >= 1 && id <= 64800)
    back <- iaw$invlatlon(id)
    expect_equal(back[["lat"]], lat, tolerance = 1)
    expect_equal(back[["lon"]], lon, tolerance = 1)
})

test_that("mklatlonid rejects out-of-range coordinates", {
    expect_error(iaw$mklatlonid(100, 0))   # lat > 90
    expect_error(iaw$mklatlonid(0, 200))   # lon > 180
})

# --- now ---

test_that("now returns a character timestamp", {
    result <- iaw$now()
    expect_type(result, "character")
    expect_match(result, "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$")
})

test_that("now respects custom format", {
    result <- iaw$now(format = "%Y")
    expect_match(result, "^\\d{4}$")
})
