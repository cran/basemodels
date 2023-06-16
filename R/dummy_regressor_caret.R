#' dummy regressor for a numerical variable, used in the train function in caret.
#'
#' @param strategy a strategy from "constant", "mean", "median", or "quantile".
#' @param quantile used when using the quantile strategy. It is a value between 0 and 1.
#' @param constant used when using the constant strategy. It is a numeric value.
#'
#' @return a list containing information of the model.
dummy_regressor_caret <- function(strategy = "mean", quantile = NULL, constant = NULL) {
  if (!strategy %in% c("mean", "median", "quantile", "constant")) {
    stop("Invalid strategy. Choose from 'mean', 'median', 'quantile', or 'constant'.")
  }
  if (strategy == "quantile" & is.null(quantile)) {
    stop("For quantile strategy, you have to specify the desired quantile in the range [0, 1].")
  }
  if (strategy == "constant" & is.null(constant)) {
    stop("For constant strategy, you need to provide a constant value.")
  }

  function(X, y) {
    model <- list()
    model$strategy <- strategy
    model$quantile <- quantile
    model$constant <- constant
    model$y <- y
    return(model)
  }
}
