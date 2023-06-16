#' dummy regressor predictor
#'
#' @param object a list from the dummy_regressor function
#' @param X a data frame
#'
#' @return the predicted values
#' @export
#'
#' @examples
#' #' # Split the data into training and testing sets
#' set.seed(2023)
#' index <- sample(1:nrow(iris), nrow(iris) * 0.8)
#' train_data <- iris[index,]
#' test_data <- iris[-index,]
#'
#' # Make predictions using the trained dummy regressor
#' reg_model <- dummy_regressor(train_data$Sepal.Length, strategy = "median")
#' y_hat <- predict_dummy_regressor(reg_model, test_data)
#' # Find mean squared error
#' mean((test_data$Sepal.Length-y_hat)^2)
predict_dummy_regressor <- function(object, X) {

  n_samples <- nrow(X)
  strategy <- object$strategy
  percentile <- object$quantile
  constant <- object$constant
  y <- object$y
  if (!(is.numeric(y)|is.integer(y))) stop("The response variable is not numerical.")
  if (strategy == "mean") {
    y.hat <- rep(mean(y, na.rm=T), n_samples)
  } else if (strategy == c("median")) {
    y.hat <- rep(stats::median(y, na.rm=T), n_samples)
  } else if (strategy == "quantile") {
    if (percentile < 0 | percentile > 1){
      stop("quantile must be a value in the range [0, 1].")
    }
    y.hat <- rep(unname(stats::quantile(y, percentile, na.rm=T)), n_samples)
  } else if (strategy == "constant") {
    y.hat <- rep(constant, n_samples)
  } else {
    stop("Invalid strategy specified.")
  }

  return(y.hat)
}
