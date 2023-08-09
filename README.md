# basemodels: Baseline Models for Classification and Regression
[![CRAN](https://www.r-pkg.org/badges/version/basemodels)](https://cran.r-project.org/package=basemodels)
[![CRAN](https://cranlogs.r-pkg.org/badges/grand-total/basemodels)](https://cran.r-project.org/package=basemodels)

This R package, `basemodels`, provides equivalent functions for the dummy classifier and regressor used in 'Python' 'scikit-learn' library with some modifications. Our goal is to allow R users to **easily identify baseline performance for their classification and regression problems**. Our baseline models use no predictors, and are useful in cases of class imbalance, multi-class classification, and when users want to quickly identify how much improvement their statistical and machine learning models are over several baseline models. We use a "better" default (proportional guessing) for the dummy classifier than the Python implementation ("prior", which is the most frequent class in the training set).

# Example

```
# Split the data into training and testing sets
set.seed(2023)
index <- sample(1:nrow(iris), nrow(iris) * 0.8)
train_data <- iris[index,]
test_data <- iris[-index,]
dummy_model <- dummy_classifier(train_data$Species, strategy = "proportional", random_state = 2024)

# Make predictions using the trained dummy classifier
pred_vec <- predict_dummy_classifier(dummy_model, test_data)

# Evaluate the performance of the dummy classifier
conf_matrix <- caret::confusionMatrix(pred_vec, test_data$Species)
print(conf_matrix)

# Make predictions using the trained dummy regressor
reg_model <- dummy_regressor(train_data$Sepal.Length, strategy = "median")
y_hat <- predict_dummy_regressor(reg_model, test_data)
# Find mean squared error
mean((test_data$Sepal.Length-y_hat)^2)
```

# Install
The package can be installed directly from CRAN:

```
install.packages("basemodels")
```

or directly from GitHub:

```
devtools::install_github("Ying-Ju/basemodels")
```
