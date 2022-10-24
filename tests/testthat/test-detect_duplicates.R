test_that("duplicates works", {
  expect_equal(detect_duplicate_colnames(iris), setNames(rep(FALSE,5), colnames(iris)))
  expect_equal(
    detect_duplicate_colnames(cbind(iris,iris)),
    setNames(rep(TRUE,10), c(colnames(iris),colnames(iris)))
  )
  expect_equal(
    detect_duplicate_colnames(cbind(iris[,1:3],iris[2:5])),
    setNames(c(FALSE,TRUE,TRUE,TRUE,TRUE, FALSE,FALSE), c(colnames(iris)[1:3],colnames(iris)[2:5]))
  )

})
