test_that("Equality works", {
  expect_equal(c(1,2,3) %==% c(1,NA,3), c(TRUE,TRUE,TRUE))
  expect_equal(equals(c(1,2,3),c(1,NA,3)), c(TRUE,TRUE,TRUE))

})
