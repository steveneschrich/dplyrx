test_that("list_join with NULL", {
  expect_equal(list_join(list(NULL, iris), by="Species"), iris)
})
