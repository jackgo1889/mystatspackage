#' T Test Function
#'
#' This function performs a one sample t-test.
#'
#' @param x Numeric vector of data.
#' @param alternative Character string specifying alternative hypothesis. Only accepts \code{"two.sided"}, \code{"less"}, or \code{"greater"}.
#' @param mu Numeric indicating null hypothesis value of the mean.
#' @keywords inference
#'
#' @return List with elements,
#' \code{test_stat}: the numeric test statistic,
#' \code{df}: the degrees of freedom, \code{alternative}: the value of the parameter,
#' \code{alternative},
#' \code{p_val}: the numeric p-value.
#'
#' @importFrom stats pt sd
#'
#' @examples
#' test_vector = c(1:25)
#' my_t_test(test_vector, alternative = "less", mu = 14)
#'
#' @export
my_t_test <- function(x, alternative, mu) {
  #defining degrees of freedom
  df <- length(x) - 1
  #defining test statistic
  test_stat <- (mean(x) - mu) / (sd(x) / sqrt(length(x)))
  #defining p value
  p_val = 0
  #creating a list from defined elements
  my_list <- list("test statistic" = test_stat,
                  "degrees of freedom" = df,
                  "alternative hypothesis" = alternative,
                  "p_value" = p_val)
  #if doing a two sided test, this runs
  if (alternative == "two.sided") {
    my_list$p_value <- 2*pt(abs(test_stat), df, lower.tail = FALSE)
    return(my_list)
    #if alternative test is less, this runs
  } else if (alternative == "less") {
    my_list$p_value <- pt(test_stat, df, lower.tail = TRUE)
    return(my_list)
    #if alternative test is greater, this runs
  } else if (alternative == "greater") {
    my_list$p_value <- pt(test_stat, df, lower.tail = FALSE)
    return(my_list)
    #if alternative value is none of the above specified strings, this error message will pop up
  } else {
    stop("Alternative value must be 'two.sided', 'less', or 'greater'.")
  }
}
