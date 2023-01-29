test_that("Plot should use geom_line", {
  bbands_plot <- plot_bbands("MSFT")
  expect_true("GeomLine" %in% c(class(bbands_plot$layers[[1]]$geom)))
})

test_that("Plot should map x to date", {
  bbands_plot <- plot_bbands("MSFT")
  expect_true("date"  == rlang::get_expr(bbands_plot$mapping$x))
})

test_that("Plot should map y to up value of bbands", {
  bbands_plot <- plot_bbands("MSFT")
  expect_true("up"  == rlang::get_expr(bbands_plot$layers[[1]]$mapping$y))
})
