test_that("Output is not a dataframe!", {
  ma_df <- moving_average("MSFT", 200)
  expect_equal(is.data.frame(ma_df), TRUE)
})

test_that("We cannot have an empty dataframe!", {
  ma_df <- moving_average("MSFT", 200)
  expect_equal(nrow(ma_df)>0,TRUE)
})

test_that("The number of columns of dataframe must be 2!", {
  ma_df <- moving_average("MSFT", 200)
  expect_equal(ncol(ma_df), 2)
})


