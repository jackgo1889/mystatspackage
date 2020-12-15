test_that("my_rf_cv outputs a numeric", {
  results <- my_rf_cv(5)
  expect_is(results, "numeric")
})
