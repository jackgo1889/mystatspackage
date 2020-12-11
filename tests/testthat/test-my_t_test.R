test_that("my_t_test works mathematically", {
  test_vector <- c(1:25)
  results_1 <- my_t_test(test_vector, alternative = "less", mu = 14)
  results_2 <- my_t_test(test_vector, alternative = "greater", mu = 14)
  results_3 <- my_t_test(test_vector, alternative = "two.sided", mu = 14)
  test_1 <- results_1$`test statistic`
  test_2 <- results_1$`degrees of freedom`
  test_3 <- results_1$`alternative hypothesis`
  test_4 <- results_2$`alternative hypothesis`
  test_5 <- results_3$`alternative hypothesis`
  test_6 <- results_1$p_value
  expect_equal(round(test_1, digits = 6), -0.679366)
  expect_equal(test_2, 24)
  expect_match(test_3, "less")
  expect_match(test_4, "greater")
  expect_match(test_5, "two.sided")
  expect_equal(round(test_6, digits = 6), 0.251703)
})
test_that("Unspecified string throws error", {
  test_vector <- c(1:25)
  expect_error(my_t_test(test_vector, alternative = "some string", mu = 0))
})
