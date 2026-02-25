#' Descriptive Statistics Summary
#'
#' @name summary
#'
#' Computes comprehensive descriptive statistics.
#'
#' @param df Data frame, matrix, or vector.
#' @param verbose Statistics profile: "p", "x", "X", "a", "sr", "sr252".
#' @param digits Decimal places. Default 4.
#'
#' @return Numeric matrix of statistics.
#'
#' @family statistics
#' @export
#'
#' @examples
#' # Basic profile "p": n, pct NA, mean, sd, t-stat
#' set.seed(1)
#' df <- data.frame(x = rnorm(100), y = rnorm(100))
#' iaw$summary(df, "p")
#'
#' # Extended profile "x" adds min/median/max
#' iaw$summary(df, "x")
#'
#' # Default profile "X" adds quantiles and autocorrelation
#' iaw$summary(df)
#'
#' # Works on a plain vector too
#' iaw$summary(rnorm(200, mean = 0.01, sd = 0.1), "p")
#'
#' # Return profile with NA values to see pctna column
#' df2 <- data.frame(a = c(1:50, rep(NA, 50)), b = rnorm(100))
#' iaw$summary(df2, "p")

iaw$summary <- function(df, verbose = "X", digits = 4) {
    if (is.vector(df) | is.matrix(df)) df <- data.frame(df)
    stopifnot(is.data.frame(df))
    stopifnot(nrow(df) >= 1)

    wantedstats <- list()
    wantedstats[["p"]] <- c("nok", "pctna", "mean", "sd", "tstat")
    wantedstats[["sr"]] <- c("nok", "pctna", "mean", "var", "sd", "tstat", "sharpe", "min", "max")
    wantedstats[["sr252"]] <- c("nok", "pctna", "mean252", "var252", "sd252", "tstat", "sharpe252")
    wantedstats[["x"]] <- c(wantedstats[["p"]], c("min", "median", "max"))
    wantedstats[["X"]] <- c(wantedstats[["p"]], c("pmost", "auto"))
    wantedstats[["a"]] <- c(wantedstats[["p"]], c("pall", "frcpos", "trimmn", "sd2", "auto"))

    stopifnot(verbose %in% names(wantedstats))
    w <- wantedstats[[verbose]]

    nok <- function(x, na.rm = FALSE) sum(!is.na(x))
    pctna <- function(x, na.rm = FALSE) {
        if (sum(is.na(x)) == 0) NA else as.integer(100 * sum(is.na(x)) / length(x))
    }
    pall <- function(x, na.rm = TRUE) {
        quantile(x, c(0, 0.01, 0.05, 0.25, 0.50, 0.75, 0.95, 0.99, 1), na.rm = na.rm)
    }
    pmost <- function(x, na.rm = TRUE) {
        quantile(x, c(0, 0.10, 0.50, 0.90, 1), na.rm = na.rm)
    }
    trimmn <- function(x, na.rm = FALSE) mean(x, trim = 0.05, na.rm = na.rm)
    frcpos <- function(x, na.rm = FALSE) round(mean(x > 0, na.rm = na.rm), 2)
    tstat <- function(x, na.rm = FALSE) {
        round(mean(x, na.rm = na.rm) / sd(x, na.rm = na.rm) * sqrt(nok(x)), 2)
    }
    sharpe <- function(x, na.rm = FALSE) {
        round(mean(x, na.rm = na.rm) / sd(x, na.rm = na.rm), 4)
    }
    mean252 <- function(x, na.rm = FALSE) round(252 * mean(x, na.rm = na.rm), 4)
    var252 <- function(x, na.rm = FALSE) round(252 * var(x, na.rm = na.rm), 4)
    sd252 <- function(x, na.rm = FALSE) round(sqrt(252) * sd(x, na.rm = na.rm), 4)
    sharpe252 <- function(x, na.rm = FALSE) {
        round(sqrt(252) * mean(x, na.rm = na.rm) / sd(x, na.rm = na.rm), 4)
    }
    sd2 <- function(x, na.rm = FALSE) {
        pm <- 0.1915
        quantile(x, 0.5 + pm, na.rm = na.rm) - quantile(x, 0.5 - pm, na.rm = na.rm)
    }
    auto <- function(x, na.rm = FALSE) {
        suppressWarnings(cor(x[2:length(x)], x[1:(length(x) - 1)], use = "pair"))
    }

    stat_env <- environment()

    df <- as.data.frame(lapply(df, function(x) if (is.logical(x)) as.integer(x) else x))
    nums <- unlist(lapply(df, is.numeric))
    if (all(!nums)) stop("No numeric columns found")
    names.of.numerics <- names(nums[nums])

    describeonevar <- function(x, statswanted) {
        if (is.logical(x)) x <- as.numeric(x)
        if (!is.numeric(x)) return(NULL)
        s <- c()
        for (sw1 in statswanted) {
            s <- c(s, get(sw1, envir = stat_env)(x, na.rm = TRUE))
            if (!sw1 %in% c("pall", "pmost")) names(s)[length(s)] <- sw1
        }
        s
    }

    o <- t(simplify2array(lapply(names.of.numerics, function(v) describeonevar(df[, v], w))))
    rownames(o) <- names.of.numerics
    round(o, digits)
}
