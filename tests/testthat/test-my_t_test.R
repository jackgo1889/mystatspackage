test_that("my_t_test works mathematically", {
  test_vector <- c(1:25)
  results <- my_t_test(test_vector, alternative = "less", mu = 14)
  expect_equal(results$test_stat, -0.679366)
  expect_equal(results$df, 24)
  expect_match(results$alternative, "less")
  expect_equal(results$p_val, 0.25170)
})
test_that("Unspecified string throws error", {
  test_vector <- c(1:25)
  expect_error(my_t_test(test_vector, alternative = "some string", mu = 0))
})
