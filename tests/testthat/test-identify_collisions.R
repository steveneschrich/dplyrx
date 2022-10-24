test_that("identify_collisions degenerate cases", {
  x <- data.frame(A.x=c(1,2), A.y=c(1,2))
  expect_error(identify_collisions())
  expect_equal(identify_collisions(data.frame()), NULL)
  expect_error(identify_collisions(c()))
  expect_equal(identify_collisions(iris), NULL)
})


test_that("identify_collisions works", {
  x <- data.frame(A.x=c(1,2), A.y=c(1,2))
  expect_equal(identify_collisions(x), list("A"=c("A.x","A.y")))

  expect_equal(
    identify_collisions(dplyr::left_join(iris,iris,by="Species")),
    setNames(purrr::map(colnames(iris)[1:4], ~paste0(.x, c(".x",".y"))),colnames(iris)[1:4])
  )

  expect_equal(
    identify_collisions(dplyr::left_join(iris[,c(1:3,5)], iris[,3:5], by="Species")),
    setNames(list(paste0(colnames(iris)[3], c(".x",".y"))), colnames(iris)[3])
  )
})
