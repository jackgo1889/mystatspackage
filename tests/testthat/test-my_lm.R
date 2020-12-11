test_that("my_lm works mathematically", {
  data("mtcars")
  results <- my_lm(formula = mpg ~ hp + wt, data = mtcars)
  expect_equal(round(results[2, 2], digits = 8), 0.00902971)
})
