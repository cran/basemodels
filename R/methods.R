#' a method used for the train function in caret
#' @export
#' @examples
#' # Split the data into training and testing sets
#' set.seed(2023)
#' index <- sample(1:nrow(iris), nrow(iris) * 0.8)
#' train_data <- iris[index,]
#' test_data <- iris[-index,]
#'
#' ctrl1 <- caret::trainControl(method = "none")
#' # Train a dummy classifier with caret
#' dummy_model <- caret::train(Species ~ ., data = train_data,
#'                                method = dummyClassifier,
#'                                strategy = "stratified",
#'                                trControl = ctrl1)
#'
#' # Make predictions using the trained dummy classifier
#' pred_vec <- predict(dummy_model, test_data)
#'
#' # Evaluate the performance of the dummy classifier
#' conf_matrix <- caret::confusionMatrix(pred_vec, test_data$Species)
#' print(conf_matrix)

dummyClassifier <- list(
  label = "dummyClassifier",
  library = NULL,
  type = "Classification",
  parameters = data.frame(parameter = "parameter",
                          class = "character",
                          label = "parameter"),
  grid = function(x, y, len = NULL, search = "grid") {
    data.frame(parameter = "none")
  },
  loop = NULL,
  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    theDots <- list(...)
    if (!any(names(theDots) == "strategy")){
      theDots$strategy <- "proportional"
    }
    if (!any(names(theDots) == "constant")){
      theDots$constant <- NULL
    }
    if (!any(names(theDots) == "random_state")){
      theDots$random_state <- NULL
    }
    model <- dummy_classifier_caret(theDots$strategy, theDots$constant, theDots$random_state)(x, y)
    model$levels <- levels(model$y)
    model
  },
  predict = function(modelFit, newdata, submodels = NULL) {
    factor(predict_dummy_classifier(modelFit, newdata), levels = modelFit$levels)
  },
  prob = NULL,
  predictors = function(x, ...) {
    NULL
  },
  tags = c("Dummy Classifier"),
  levels = NULL,
  sort = function(x) x
)

#' a method used for the train function in caret
#' @export
#' @examples
#' # Split the data into training and testing sets
#' set.seed(2023)
#' index <- sample(1:nrow(iris), nrow(iris) * 0.8)
#' train_data <- iris[index,]
#' test_data <- iris[-index,]
#'
#' ctrl1 <- caret::trainControl(method = "none")
#' # Train a dummy regressor with caret
#' reg_model <- caret::train(Sepal.Length ~ ., data = train_data,
#'                                method = dummyRegressor,
#'                                strategy = "median",
#'                                trControl = ctrl1)
#' y_hat <- predict(reg_model, test_data)
#' # Find mean squared error
#' mean((test_data$Sepal.Length-y_hat)^2)
dummyRegressor <- list(
  label = "dummyRegressor",
  library = NULL,
  type = "Regression",
  parameters = data.frame(parameter = "parameter",
                          class = "character",
                          label = "Strategy"),
  grid = function(x, y, len = NULL, search = "grid") {
    data.frame(parameter = "none")
  },
  loop = NULL,
  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    theDots <- list(...)
    if (!any(names(theDots) == "strategy")){
      theDots$strategy <- "mean"
    }
    if (!any(names(theDots) == "quantile")){
      theDots$quantile <- NULL
    }
    if (!any(names(theDots) == "constant")){
      theDots$constant <- NULL
    }
    model <- dummy_regressor_caret(theDots$strategy, theDots$quantile, theDots$constant)(x, y)
    model$levels <- lev
    model
  },
  predict = function(modelFit, newdata, submodels = NULL) {
    predict_dummy_regressor(modelFit, newdata)
  },
  prob = NULL,
  predictors = function(x, ...) {
    NULL
  },
  tags = c("Dummy Regressor"),
  levels = NULL,
  sort = function(x) x
)
