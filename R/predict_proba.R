#' probabilities for predicting classes
#'
#' @param model a list from dummy classifier.
#' @param X a data frame.
#'
#' @return a probability matrix.
predict_proba <- function(model, X) {
  y <- model$y
  # Extract necessary parameters from the model object
  n_classes <- model$n_classes
  class_prior <- model$class_prior
  classes <- model$classes
  constant <- model$constant
  strategy <- model$strategy
  set.seed(model$random_state)

  # Compute the number of samples and create a random state
  n_samples <- nrow(X)

  # Initialize an empty list to store the probability estimates
  P <- matrix(NA, n_samples, n_classes)
  # Loop over each output and compute the probability estimates
  if (strategy == "most_frequent") {
    temp <- unname(sort(class_prior))
    if (temp[1] == temp[2]) warning(paste0("At least two classes had equal and highest frequency. The reported results use the first majority class, ", classes[which.max(class_prior)], "."))
    ind <- which.max(class_prior)
    out <- matrix(0, n_samples, n_classes)
    out[, ind] <- 1.0
  } else if (strategy == "proportional") {
    out <- matrix(class_prior, n_samples, n_classes, byrow=T)
  } else if (strategy == "stratified") {
    out <- matrix(0, n_samples, n_classes)
    for (i in seq_len(n_samples)) {
      out[i, ] <- stats::rmultinom(1, 1, class_prior)
    }
    out <- as.matrix(out, dtype = "numeric")
  } else if (strategy == "uniform") {
    out <- matrix(1/n_classes, n_samples, n_classes)
  } else if (strategy == "constant") {
    ind <- which(classes == constant)
    out <- matrix(0, n_samples, n_classes)
    out[, ind] <- 1.0
  }
  P <- out

  return(P)
}
