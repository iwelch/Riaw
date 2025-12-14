#' Out-of-Sample Regression
#'
#' @name oosreg
#'
#' Performs out-of-sample regression.
#'
#' @param formula Regression formula.
#' @param data Data frame.
#' @param train_frac Fraction for training.
#'
#' @return List with in-sample and out-of-sample results.
#'
#' @family regression
#' @export

iaw$oosreg <- function(formula, data, train_frac = 0.7) {
    stopifnot(inherits(formula, "formula"))
    stopifnot(is.data.frame(data))
    stopifnot(is.numeric(train_frac), train_frac > 0, train_frac < 1)
    
    n <- nrow(data)
    train_idx <- seq_len(floor(n * train_frac))
    
    train_data <- data[train_idx, ]
    test_data <- data[-train_idx, ]
    
    model <- lm(formula, data = train_data)
    pred <- predict(model, newdata = test_data)
    
    y_var <- all.vars(formula)[1]
    actual <- test_data[[y_var]]
    
    list(
        model = model,
        oos_rmse = sqrt(mean((actual - pred)^2, na.rm = TRUE)),
        oos_rsq = cor(actual, pred, use = "complete.obs")^2
    )
}
