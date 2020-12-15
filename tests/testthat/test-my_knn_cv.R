test_that("my_knn_cv outputs correct classes", {
  data(my_penguins)
  penguins_df <- tidyr::drop_na(my_penguins)
  results <- my_knn_cv(penguins_df[ , 3:6], penguins_df$species, 5, 5)
  expect_is(results, "list")
  expect_is(results$class, "character")
  expect_is(results$cvErr, "numeric")
})
