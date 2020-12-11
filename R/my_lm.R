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

  #creating data frame to hold values in
  my_data <- data.frame("Estimate" = 1:ncol(x),
                        "Standard_Error" = 1:ncol(x),
                        "t_value" = 1:ncol(x),
                        "twoSided_test" = 1:ncol(x))
  #creating rownames
  rownames(my_data) <- c("Intercept", labels(terms(formula)))

  #solving for coefficients and adding them into the dataframe
  beta <- solve(t(x) %*% x) %*% (t(x) %*% y)
  my_data$Estimate <- beta

  #solving for the standard errors and adding them into the dataframe
  df <- nrow(data) - ncol(x)
  sigma_squared <- sum(((y - (x %*% beta))^2) / df)
  se <- diag(sqrt(sigma_squared * solve(t(x) %*% x)))
  my_data$Standard_Error <- se

  #solving for the t values and adding them into the dataframe
  t_value <- beta / se
  my_data$t_value <- t_value

  #contucting the two sided t test values and adding them into the dataframe
  twoSided_t_test <- 2*pt(abs(t_value), df, lower.tail = FALSE)
  my_data$twoSided_test <- twoSided_t_test

  return(my_data)
}
