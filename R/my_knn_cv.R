#' k-Nearest Neightbors Cross-Validation Function
#'
#' This function predicts an output class using given covariates.
#'
#' @param train Input data frame.
#' @param cl Class value of training data.
#' @param k_nn Integer representing the number of neighbors.
#' @param k_cv Integer representing the number of folds.
#' @keywords prediction
#'
#' @return List with objects,
#'  \code{class}: Vector of predicted class for all observations,
#'  \code{cv_err}: Numeric with the cross-validation misclassification error.
#'
#' @import stats class dplyr utils magrittr tidyr
#'
#' @examples
#' data(my_penguins)
#' penguins_df <- tidyr::drop_na(my_penguins)
#' my_knn_cv(penguins_df[ , 3:6], penguins_df$species, 5, 5)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  #creating return output
  my_list <- list("class" = NA,
                  "cvErr" = 0)
  #creating variable n, the number of rows in train data
  n <- nrow(train)

  #creating variable that randomly assigns observations
  fold <- sample(rep(1:k_cv, length = n))
  train$fold <- fold

  #creating vector to hold knn output
  pred_vector <- rep(NA, n)

  for (i in 1:k_cv) {
    #creating train input
    data_train <- train %>% dplyr::filter(fold != i)

    #creating cl input
    cl_train <- cl[fold != i]
    cl_test <- cl[fold == i]

    #creating test input
    data_test <- train %>% dplyr::filter(fold == i)

    #store knn output into class vector
    predictions <- knn(train = data_train[ , -ncol(data_train)],
                       test = data_test[ , -ncol(data_test)],
                       cl = cl_train, k = k_nn)
    pred_vector[train$fold == i] <- as.character(predictions)

    #compute error value
    error_value <- 1 - sum(predictions == cl_test) / length(predictions)
    my_list$cvErr <- as.numeric(my_list$cvErr) + as.numeric(error_value)
  }
  #input average error value into my_list
  my_list$cvErr <- as.numeric(my_list$cvErr) / k_cv

  #input predictions vector into my_list
  my_list$class <- pred_vector

  return(my_list)
}
