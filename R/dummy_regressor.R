#' dummy regressor for a numerical variable.
#'
#' @param y a numerical vector.
#' @param strategy a strategy from "constant", "mean", "median", or "quantile".
#' @param quantile used when using the quantile strategy. It is a value between 0 and 1.
#' @param constant used when using the constant strategy. It is a numeric value.
#'
#' @return a list containing information of the model.
#' @export
#'
#' @examples
#' # Split the data into training and testing sets
#' set.seed(2023)
#' index <- sample(1:nrow(iris), nrow(iris) * 0.8)
#' train_data <- iris[index,]
#' test_data <- iris[-index,]
#' reg_model <- dummy_regressor(train_data$Sepal.Length, strategy = "median")
#' reg_model
dummy_regressor <- function(y, strategy = "mean", quantile = NULL, constant = NULL) {
  if (!strategy %in% c("mean", "median", "quantile", "constant")) {
    stop("Invalid strategy. Choose from 'mean', 'median', 'quantile', or 'constant'.")
  }
  if (strategy == "quantile" & is.null(quantile)) {
    stop("For quantile strategy, you have to specify the desired quantile in the range [0, 1].")
  }
  if (strategy == "constant" & is.null(constant)) {
    stop("For constant strategy, you need to provide a constant value.")
  }

  model <- list()
  model$strategy <- strategy
  model$quantile <- quantile
  model$constant <- constant
  model$y <- y
  return(model)

}
