tbls <- list(
  i = iris,
  ir = iris[,1:3],
  m = mtcars,
  m = mtcars[,1]
)

test_that("Degenerate case works", {
  expect_equal(find_common_colnames(), NULL)
  expect_equal(common_colnames(), NULL)
  expect_equal(find_common_colnames(list()), list())
  expect_equal(common_colnames(list()), NULL)
  expect_equal(find_common_colnames(iris), list())
  expect_equal(common_colnames(iris), NULL)
  expect_equal(find_common_colnames(list(iris)), list())
  expect_equal(common_colnames(list(iris)), NULL)
  expect_equal(find_common_colnames(data.frame()), list())
  expect_equal(common_colnames(data.frame()), NULL)
  expect_equal(find_common_colnames(list(data.frame())), list())
  expect_equal(common_colnames(list(data.frame())), NULL)
})


test_that("Non-overlap works", {
  expect_equal(find_common_colnames(list(iris, mtcars)), list())
  expect_equal(find_common_colnames(list(i=iris, m=mtcars)), list())
  expect_equal(common_colnames(list(iris,mtcars)), NULL)
  expect_equal(common_colnames(list(i=iris,m=mtcars)), NULL)
  expect_equal(find_common_colnames(list(iris, mtcars, cars)), list())
  expect_equal(common_colnames(list(iris,mtcars,cars)), NULL)
})

test_that("Full overlap works", {
  expect_equal(
    find_common_colnames(list(iris, iris)),
    purrr::map(setNames(1:5,colnames(iris)), ~c(1,2))
  )
  expect_equal(
    common_colnames(list(iris,iris)),
    colnames(iris)
  )

  expect_equal(
    find_common_colnames(list(iris, iris, mtcars)),
    purrr::map(setNames(1:5,colnames(iris)), ~c(1,2))
  )

  expect_equal(
    common_colnames(list(iris,iris,mtcars)),
    colnames(iris)
  )
})

test_that("Duplicates from multiple tables works", {

  expect_equal(
    common_colnames(list(iris,iris,mtcars,mtcars)),
    c(colnames(iris), colnames(mtcars))
  )
})

test_that("Non-table inclusion fails", {
  expect_error(find_common_colnames(list(iris, iris[,1])))
})
test_that("Partial overlap works", {
  expect_equal(
    find_common_colnames(list(iris, iris[,2:3])),
    list(Sepal.Width=c(1,2), Petal.Length=c(1,2))
  )
  expect_equal(
    find_common_colnames(list(iris, iris[,1,drop=FALSE])),
    list(Sepal.Length=c(1,2))
  )
})

test_that("Complex overlaps works", {
  expect_equal(
    find_common_colnames(list(iris, iris[,3:4], mtcars,mtcars[,2,drop=FALSE],cars)),
    list(Petal.Length=c(1,2), Petal.Width=c(1,2), cyl=c(3,4))
  )
  expect_equal(
    common_colnames(list(iris,iris[,3:4], mtcars,mtcars[,2,drop=FALSE], cars)),
    c("Petal.Length","Petal.Width","cyl")
  )
})
