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

iaw$dict.lookup <- function(keys, dict, default = NA) {
    stopifnot(is.character(keys))

    ifelse(keys %in% names(dict), dict[keys], default)

    # result <- dict[as.character(keys)]
    # result[is.na(result)] <- default
    # result
}
