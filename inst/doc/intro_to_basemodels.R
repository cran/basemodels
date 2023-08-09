## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(basemodels)

## -----------------------------------------------------------------------------
set.seed(2023)
index <- sample(1:nrow(iris), nrow(iris) * 0.8)
train_data <- iris[index,]
test_data <- iris[-index,]

## -----------------------------------------------------------------------------
ctrl1 <- caret::trainControl(method = "none")
# Train a dummy classifier with caret
dummy_model <- caret::train(Species ~ ., 
                            data = train_data,
                            method = dummyClassifier,
                            strategy = "stratified",
                               trControl = ctrl1)

# Make predictions using the trained dummy classifier
pred_vec <- predict(dummy_model, test_data)

# Evaluate the performance of the dummy classifier
conf_matrix <- caret::confusionMatrix(pred_vec, test_data$Species)
print(conf_matrix)

## -----------------------------------------------------------------------------
dummy_model <- dummy_classifier(train_data$Species, strategy = "proportional", random_state = 2024)

# Make predictions using the trained dummy classifier
pred_vec <- predict_dummy_classifier(dummy_model, test_data)

# Evaluate the performance of the dummy classifier
conf_matrix <- caret::confusionMatrix(pred_vec, test_data$Species)
print(conf_matrix)

## -----------------------------------------------------------------------------
# Make predictions using the trained dummy regressor
reg_model <- dummy_regressor(train_data$Sepal.Length, strategy = "median")
y_hat <- predict_dummy_regressor(reg_model, test_data)
# Find mean squared error
mean((test_data$Sepal.Length-y_hat)^2)

## -----------------------------------------------------------------------------
ctrl1 <- caret::trainControl(method = "none")
# Train a dummy regressor with caret
reg_model <- caret::train(Sepal.Length ~ ., data = train_data,
                               method = dummyRegressor,
                               strategy = "median",
                               trControl = ctrl1)
y_hat <- predict(reg_model, test_data)
# Find mean squared error
mean((test_data$Sepal.Length-y_hat)^2)

