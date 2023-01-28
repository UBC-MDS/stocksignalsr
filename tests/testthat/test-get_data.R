test_that("data file exists", {
  get_data("MSFT", "1986-03-13")
  datafile <- readr::read_csv("../data/MSFT.csv")
  expect_equal(nrow(datafile)>0,TRUE)
})

test_that("data folder exists", {
  get_data("MSFT", "1986-03-13")
  datafile <- readr::read_csv("../data/MSFT.csv")
  expect_equal(datafile$symbol[1],"MSFT")
})

test_that("data folder exists", {
  get_data("MSFT", "1986-03-13")
  datafile <- readr::read_csv("../data/MSFT.csv")
  expect_equal(typeof(datafile$adjusted[1]),"double")
})
