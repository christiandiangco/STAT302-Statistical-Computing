#' Cross Validation
#'
#' Function that performs k-Nearest Neighbors Cross Validation.
#'
#' @param train Input data frame.
#' @param cl Vector of true class values of your training data.
#' @param k_nn Integer representing the number of neighbors.
#' @param k_cv Integer representing the number of folds.
#'
#' @return A vector of the predicted class for all observations.
#' @return A numeric with the cross-validation misclassification error.
#'
#' @examples
#' train <- iris[1:4]
#' cl <- iris$Species
#' my_knn_cv(train, cl, 5, 10)
#'
#' @import dplyr
#' @import class
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  n = nrow(train)
  # A vector for storing the misclassification rates for each fold
  error_rates <- c()

  # Randomly assigns observations to folds 1-10 and join assignments to dataframe
  folds <- sample(rep(1:10, length=n))
  train$fold <- folds

  # Iterate through each fold and predict the class for each
  for (i in 1:k_cv) {
    data_train <- train %>% filter(fold != i)
    data_test <- train %>% filter(fold == i)

    # Predict species
    knn_pred <- knn(train=data_train[-1], test=data_test[-1], cl=data_train[[1]], k=k_nn)

    # Proportion of observations classified incorrectly
    error_rate <- 1 - sum(knn_pred == data_test[[1]]) / dim(data_test)[1]
    error_rates <- c(error_rates, error_rate)
  }

  # Output of knn() with the full data as both the training and the test data
  class <- knn(train[-1], train[-1], cl, k=k_nn)

  # Average misclassification rate from cross validation
  cv_error <- mean(error_rates)

  return(list(class=class, cv_error=cv_error))
}
