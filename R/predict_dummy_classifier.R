#' dummy classifier predictor
#'
#' @param object a list created using dummy classifier.
#' @param X a data frame.
#'
#' @return predicted values for the response variable.
#' @export
#'
#' @examples
#' # Split the data into training and testing sets
#' set.seed(2023)
#' index <- sample(1:nrow(iris), nrow(iris) * 0.8)
#' train_data <- iris[index,]
#' test_data <- iris[-index,]
#' dummy_model <- dummy_classifier(train_data$Species, strategy = "proportional", random_state = 2024)
#'
#' # Make predictions using the trained dummy classifier
#' pred_vec <- predict_dummy_classifier(dummy_model, test_data)
#'
#' # Evaluate the performance of the dummy classifier
#' conf_matrix <- caret::confusionMatrix(pred_vec, test_data$Species)
#' print(conf_matrix)
predict_dummy_classifier <- function(object, X) {

  n_samples <- nrow(X)
  set.seed(object$random_state)
  n_classes <- object$n_classes
  classes <- object$classes
  class_prior <- object$class_prior
  constant <- object$constant
  strategy <- object$strategy

  if (strategy == "proportional") {
    y <- sample(classes, n_samples, replace = TRUE, prob = class_prior)
  } else if (strategy == c("most_frequent")) {
    temp <- unname(sort(class_prior))
    if (temp[1] == temp[2]) warning(paste0("At least two classes had equal and highest frequency. The reported results use the first majority class, ", classes[which.max(class_prior)], "."))
    y <- rep(classes[which.max(class_prior)], each = n_samples)
  } else if (strategy == "stratified") {
    proba <- predict_proba(object, X)
    y <- classes[apply(proba, 1, which.max)]
  } else if (strategy == "constant") {
    y <- rep(constant, n_samples)
  } else if (strategy == "uniform") {
    y <- sample(classes, n_samples, replace = TRUE, prob = rep(1/n_classes, n_classes))
  } else {
    stop("Invalid strategy specified.")
  }
  y <- factor(y, levels = classes)
  return(y)
}
