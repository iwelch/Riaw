#' Check if Values are Within a Range
#'
#' @name %inrange%
#'
#' Tests whether values fall within a specified inclusive range.
#'
#' @param x A numeric value or vector to test.
#' @param range_vector A numeric vector of length 2: c(min, max).
#'
#' @return A logical vector.
#'
#' @family operators
#' @export
#'
#' @examples
#' # Single value test
#' 5 %inrange% c(1, 10)
#'
#' # Vector: which elements fall in [0, 10]?
#' c(-5, 0, 5, 10, 15) %inrange% c(0, 10)
#'
#' # Filter rows of a data frame by a numeric range
#' df <- data.frame(year = 2018:2023, ret = c(0.2, -0.1, 0.15, 0.3, -0.2, 0.1))
#' df[df$year %inrange% c(2020, 2022), ]
#'
#' # Boundary values are inclusive
#' c(0, 5, 10) %inrange% c(0, 10)   # all TRUE
#'
#' # Screen stocks by P/E ratio within a reasonable range
#' pe_ratios <- c(5, 12, 18, 25, 42, 60)
#' pe_ratios[pe_ratios %inrange% c(10, 30)]   # 12, 18, 25
#'
#' # Flag trading days with extreme returns (outside +/- 5%)
#' daily_ret <- c(0.01, -0.06, 0.03, 0.08, -0.02)
#' normal <- daily_ret %inrange% c(-0.05, 0.05)  # TRUE FALSE TRUE FALSE TRUE
#' daily_ret[!normal]   # -0.06, 0.08 (the outliers)
#'
#' # Date range filtering (works with numeric year representation)
#' years <- 2015:2025
#' years[years %inrange% c(2019, 2022)]   # 2019 2020 2021 2022

`%inrange%` <- function(x, range_vector) {
    stopifnot(is.numeric(x))
    stopifnot(is.numeric(range_vector), length(range_vector) == 2L)
#    stopifnot( typeof(x) == typeof(range_vector) )
    (x >= range_vector[1]) & (x <= range_vector[2])
}
