test_that("Output is not a dataframe!", {
  bbands <- get_bbands("MSFT")
  expect_equal(is.data.frame(bbands), TRUE)
})

test_that("We cannot have an empty dataframe!", {
  bbands <- get_bbands("MSFT")
  expect_equal(nrow(bbands)>0,TRUE)
})

test_that("The number of columns of dataframe must be 2!", {
  bbands <- get_bbands("MSFT")
  expect_equal(ncol(bbands), 5)
})
