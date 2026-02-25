#' Dictionary Lookup
#'
#' @name dict.lookup
#'
#' NOTE: This was redesigned from a data-frame merge interface (see Rold/dict-lookup.R)
#' to a named-vector lookup. Verify this was intentional.
#'
#' Looks up values in a dictionary.
#'
#' @param keys Keys to look up.
#' @param dict Named vector (dictionary).
#' @param default Default for missing.
#'
#' @return Looked up values.
#'
#' @family utilities
#' @export
#'
#' @examples
#' d <- c(a = 1, b = 2, c = 3)
#' iaw$dict.lookup(c("a", "c", "x"), d, NA)
#'
#' # Map ticker symbols to full company names; missing get a default
#' tickers <- c(AAPL = "Apple", MSFT = "Microsoft", GOOG = "Alphabet")
#' iaw$dict.lookup(c("AAPL", "GOOG", "TSLA"), tickers, default = "Unknown")
#'
#' # Use 0 as default instead of NA for numeric dictionaries
#' sector_cap <- c(tech = 5e12, energy = 2e12)
#' iaw$dict.lookup(c("tech", "finance", "energy"), sector_cap, default = 0)
#'
#' # Map country codes to region labels for panel data
#' regions <- c(US = "Americas", CA = "Americas", GB = "EMEA", DE = "EMEA", JP = "APAC")
#' iaw$dict.lookup(c("US", "JP", "BR"), regions, default = "Other")
#'   # "Americas", "APAC", "Other"
#'
#' # All keys present: no defaults needed
#' d <- c(x = 10, y = 20)
#' iaw$dict.lookup(c("x", "y"), d, default = -1)  # 10, 20
#'
#' # Empty lookup returns zero-length result
#' iaw$dict.lookup(character(0), c(a = 1), default = NA)  # character(0)

iaw$dict.lookup <- function(keys, dict, default = NA) {
    stopifnot(is.character(keys))

    ifelse(keys %in% names(dict), dict[keys], default)

    # result <- dict[as.character(keys)]
    # result[is.na(result)] <- default
    # result
}
