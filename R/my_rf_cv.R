#' Random Forest Cross-Validation Function
#'
#' This Function performs Random Forest Cross-Validation.
#'
#' @param k Numeric with number of folds.
#' @keywords prediction
#'
#' @return Numeric with cross-validation error
#'
#' @import randomForest stats tidyr dplyr magrittr
#'
#' @examples
#' my_rf_cv(k = 5)
#'
#' @export
my_rf_cv <- function(k) {
#using penguins dataset
my_penguins <- my_penguins
penguins_df <- tidyr::drop_na(my_penguins)

#creating variable that randomly assigns observations
fold <- sample(rep(1:k, length = nrow(penguins_df)))
penguins_df$fold <- fold
value = 0

for (i in 1:k) {
  #creating train input
  data_train <- penguins_df %>% dplyr::filter(fold != i)

  #creating test input
  data_test <- penguins_df %>% dplyr::filter(fold == i)

  #creating model data
  model_data <- randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm, data = data_train, ntree = 100)

   #creating predictions
  predictions <- predict(model_data, data_test[, -1])

   #computing MSE for the iteration, storing in 'value'
  value = value + (mean(predictions) - mean(penguins_df$body_mass_g))^2
 }
return(value / k)
}
