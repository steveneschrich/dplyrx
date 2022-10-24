test_that("error conditions", {
  expect_error(rename_common_columns(iris))
  expect_error(rename_common_columns(iris, suffix=c()))

})

test_that("Degenerate condition", {
  expect_equal(rename_common_columns(list(iris), suffix=c("a")), list(iris))
  expect_equal(rename_common_columns(list(iris, NULL), suffix=c("a","b")), list(iris, NULL))
  expect_equal(rename_common_columns(list(iris, mtcars), suffix=c(".iris",".mtcars")),
               list(iris,mtcars))

})


iris_rename <- list(
  setNames(iris, paste0(colnames(iris),".x")),
  setNames(iris, paste0(colnames(iris),".y"))
)
test_that("Rename works", {
  expect_equal(rename_common_columns(list(iris,iris), suffix=c(".x",".y")),
               list(
                 setNames(iris, paste0(colnames(iris),".x")),
                 setNames(iris, paste0(colnames(iris),".y"))
               )
  )
  expect_equal(rename_common_columns(list(iris[,1:3], iris[,3:5]), suffix=c(".x",".y")),
    list(
      setNames(iris[,1:3],c(colnames(iris)[1:2],paste0(colnames(iris[3]),".x"))),
      setNames(iris[,3:5],c(paste0(colnames(iris[3]), ".y"), colnames(iris)[4:5]))
    )
  )
})

test_that("rename with no suffix works", {
  expect_equal(rename_common_columns(list(iris, iris)),
               list(
                 setNames(iris, paste0(colnames(iris), ".t1")),
                 setNames(iris, paste0(colnames(iris), ".t2"))
               )
  )
})

test_that("rename with ignore works", {

  expect_equal(rename_common_columns(list(iris,iris), ignore=colnames(iris)),
               list(iris,iris)
  )

  expect_equal(rename_common_columns(list(iris[,1:3], iris[,3:5]), ignore=colnames(iris)[3]),
               list(iris[,1:3],iris[,3:5])
  )

})
