#' Linear Model Function
#'
#' This function fits a linear model.
#'
#' @param formula Formula class object.
#' @param data input data frame.
#' @keywords inference
#'
#' @return Data frame with rows for each coefficient and columns,
#'  \code{Estimate}: a column with the solved linear regression coefficients,
#'  \code{Standard_Error}: a column with the solved standard errors,
#'  \code{t_value}: a column with the solved t-values,
#'  \code{twoSided_test}: a column demonstrating a two-sided t-test for the covariates.
#'
#' @importFrom stats model.frame model.matrix model.response terms
#' @examples
#' data("mtcars")
#' my_lm(formula = mpg ~ hp + wt, data = mtcars)
#'
#' @export
my_lm <- function(formula, data) {

  #extracting model matrix x
  x <- model.matrix(formula, data)

  #extracting model frame object
  my_lm_frame <- model.frame(formula, data)

  #extracting model response y
  y <- model.response(my_lm_frame)

  #solving for coefficients
  beta <- solve(t(x) %*% x) %*% (t(x) %*% y)

  #solving for the standard errors
  df <- nrow(data) - ncol(x)
  sigma_squared <- sum(((y - (x %*% beta))^2) / df)
  se <- diag(sqrt(sigma_squared * solve(t(x) %*% x)))

  #solving for the t values a
  t_value <- beta / se

  #constructing the two sided t test values
  twoSided_t_test <- 2*pt(abs(t_value), df, lower.tail = FALSE)

  #creating table to present values
  my_data <- cbind(beta, se, t_value, twoSided_t_test)
  colnames(my_data) <- c("Estimate", "Std. Error", "t-value", "two-sided t-test")
  my_data <- as.table(my_data)
  return(my_data)
}
