#' dummy classifier for a categorical variable.
#'
#' @param y a categorical vector, containing the outcomes of interest
#' @param strategy a strategy from "constant", "most_frequent", "proportional", "uniform", or "stratified".
#' @param constant a constant value for the constant strategy.
#' @param random_state a random seed.
#'
#' @return a list
#' @export
#'
#' @examples
#' # Split the data into training and testing sets
#' set.seed(2023)
#' index <- sample(1:nrow(iris), nrow(iris) * 0.8)
#' train_data <- iris[index,]
#' test_data <- iris[-index,]
#' dummy_model <- dummy_classifier(train_data$Species, strategy = "proportional", random_state = 2024)
#' dummy_model
dummy_classifier <- function(y, strategy = "proportional", constant = NULL, random_state = NULL) {
  if (!strategy %in% c("stratified", "most_frequent", "proportional", "uniform", "constant")) {
    stop("Invalid strategy. Choose from 'stratified', 'most_frequent', 'proportional', 'uniform', or 'constant'.")
  }

  if (strategy == "constant" & is.null(constant)) {
    stop("For constant strategy, you need to provide a constant value.")
  }

  model <- list()
  model$strategy <- strategy
  model$constant <- constant
  model$y <- as.factor(y)
  model$classes <- levels(y)
  model$n_classes <- length(model$classes)
  model$class_prior <- table(y) / length(y)
  model$random_state <- random_state

  if (strategy == "most_frequent") {
    model$most_frequent <- levels(y)[which.max(table(y))]
  }

  return(model)
}
