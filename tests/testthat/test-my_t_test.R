test_that("my_t_test works mathematically", {
  test_vector <- c(1:25)
  results <- my_t_test(test_vector, alternative = "less", mu = 14)
  test_1 <- results$`test statistic`
  test_1 <- round(test_1, digits = 6)
  test_2 <- results$`degrees of freedom`
  test_3 <- results$`alternative hypothesis`
  test_4 <- results$p_value
  test_4 <- round(test_4, digits = 6)
  expect_equal(test_1, -0.679366)
  expect_equal(test_2, 24)
  expect_match(test_3, "less")
  expect_equal(test_4, 0.251703)
})
test_that("Unspecified string throws error", {
  test_vector <- c(1:25)
  expect_error(my_t_test(test_vector, alternative = "some string", mu = 0))
})
