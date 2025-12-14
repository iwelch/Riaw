#' Descriptive Statistics Summary
#'
#' Computes a comprehensive summary of descriptive statistics for numeric
#' columns in a data frame. Offers multiple pre-defined summary profiles
#' suitable for different use cases (general statistics, financial returns, etc.).
#'
#' @param df A data frame, matrix, or vector. Non-numeric columns are ignored.
#' @param verbose A character string selecting the statistics profile:
#'   \itemize{
#'     \item \code{"p"}: Basic - N, \%NA, mean, sd, t-stat
#'     \item \code{"x"}: Extended - basic + min, median, max
#'     \item \code{"X"}: Default - basic + key percentiles, autocorrelation
#'     \item \code{"a"}: All - basic + all percentiles, fraction positive, trimmed mean, robust SD, autocorrelation
#'     \item \code{"sr"}: Sharpe ratio - for financial returns
#'     \item \code{"sr252"}: Annualized Sharpe - for daily returns (252 trading days)
#'   }
#' @param digits Integer; number of decimal places for rounding. Default is 4.
#'
#' @return A numeric matrix with variables as rows and statistics as columns.
#'
#' @details
#' The function automatically:
#' \itemize{
#'   \item Converts logical columns to 0/1
#'   \item Ignores non-numeric columns
#'   \item Handles NA values appropriately in each statistic
#' }
#'
#' Statistics computed (depending on profile):
#' \itemize{
#'   \item \code{nok}: Number of non-NA observations
#'   \item \code{pctna}: Percentage of NA values (-1 if none)
#'   \item \code{mean}, \code{sd}, \code{var}: Standard moments
#'   \item \code{tstat}: t-statistic for mean = 0
#'   \item \code{sharpe}: Sharpe ratio (mean/sd)
#'   \item \code{min}, \code{max}, \code{median}: Order statistics
#'   \item \code{pmost}: 0\%, 10\%, 50\%, 90\%, 100\% percentiles
#'   \item \code{pall}: 0\%, 1\%, 5\%, 25\%, 50\%, 75\%, 95\%, 99\%, 100\% percentiles
#'   \item \code{auto}: First-order autocorrelation
#'   \item \code{frcpos}: Fraction of positive values
#'   \item \code{trimmn}: 5\% trimmed mean
#' }
#'
#' @export
#'
#' @seealso \code{\link{summary}}, \code{\link{describe}} (psych package)
#'
#' @examples
#' # Create sample data
#' df <- data.frame(
#'     returns = rnorm(100, 0.001, 0.02),
#'     volume = rlnorm(100, 10, 1),
#'     flag = sample(c(TRUE, FALSE), 100, replace = TRUE)
#' )
#' df$returns[c(5, 10, 15)] <- NA  # Add some missing values
#'
#' # Basic summary
#' iaw$summary(df, "p")
#'
#' # Extended summary with percentiles
#' iaw$summary(df, "X")
#'
#' # Financial returns with Sharpe ratio
#' iaw$summary(df["returns"], "sr")
#'
#' # Annualized statistics for daily returns
#' iaw$summary(df["returns"], "sr252")
#'
#' # All statistics
#' iaw$summary(df, "a")

iaw$summary <- function(df, verbose = "X", digits = 4) {
    if (is.vector(df) | is.matrix(df)) df <- data.frame(df)
    (is.data.frame(df)) %or% "iaw$summary requires data frame, not {{class(df)}}"
    if (nrow(df) == 0) return("no observations\n")

    # Define statistic profiles
    wantedstats <- list()
    wantedstats[["p"]] <- c("nok", "pctna", "mean", "sd", "tstat")
    wantedstats[["sr"]] <- c("nok", "pctna", "mean", "var", "sd", "tstat", "sharpe", "min", "max")
    wantedstats[["sr252"]] <- c("nok", "pctna", "mean252", "var252", "sd252", "tstat", "sharpe252")
    wantedstats[["x"]] <- c(wantedstats[["p"]], c("min", "median", "max"))
    wantedstats[["X"]] <- c(wantedstats[["p"]], c("pmost", "auto"))
    wantedstats[["a"]] <- c(wantedstats[["p"]], c("pall", "frcpos", "trimmn", "sd2", "auto"))

    if (verbose %in% names(wantedstats)) {
        w <- wantedstats[[verbose]]
    } else {
        stop("Unknown verbose option: ", verbose,
             ". Choose from: ", paste(names(wantedstats), collapse = ", "))
    }

    # Define statistics functions
    nok <- function(x, na.rm = FALSE) sum(!is.na(x))
    pctna <- function(x, na.rm = FALSE) {
        if (sum(is.na(x)) == 0) -1 else as.integer(100 * sum(is.na(x)) / length(x))
    }
    pall <- function(x, na.rm = TRUE) {
        quantile(x, c(0.0, 0.01, 0.05, 0.25, 0.50, 0.75, 0.95, 0.99, 1.00), na.rm = na.rm)
    }
    pmost <- function(x, na.rm = TRUE) {
        quantile(x, c(0.0, 0.10, 0.50, 0.90, 1.00), na.rm = na.rm)
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

    # Convert logical to numeric
    df <- as.data.frame(lapply(df, function(x) if (is.logical(x)) as.integer(x) else x))

    # Find numeric columns
    nums <- unlist(lapply(df, is.numeric))
    if (all(!nums)) stop("No numeric columns found")
    names.of.numerics <- names(nums[nums])

    # Compute statistics for one variable
    describeonevar <- function(x, statswanted) {
        if (is.logical(x)) x <- as.numeric(x)
        if (!is.numeric(x)) return(NULL)
        s <- c()
        for (sw1 in statswanted) {
            s <- c(s, get(sw1)(x, na.rm = TRUE))
            if (sw1 != "pall") names(s)[length(s)] <- sw1
        }
        s
    }

    # Apply to all numeric columns
    o <- t(simplify2array(lapply(names.of.numerics, function(v) describeonevar(df[, v], w))))
    rownames(o) <- names.of.numerics

    round(o, digits)
}
