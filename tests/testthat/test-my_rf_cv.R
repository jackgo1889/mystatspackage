test_that("my_rf_cv works mathematically", {
  results <- my_rf_cv(5)
  expect_is(results, "numeric")
})
