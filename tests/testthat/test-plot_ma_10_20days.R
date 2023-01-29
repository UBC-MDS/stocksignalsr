test_that("Plot should use geom_line", {
  ma_plot <- plot_ma_10_20days("MSFT")
  expect_true("GeomLine" %in% c(class(ma_plot$layers[[1]]$geom)))
})

test_that("Plot should map x to date", {
  ma_plot <- plot_ma_10_20days("MSFT")
  expect_true("date"  == rlang::get_expr(ma_plot$mapping$x))
})

test_that("Plot should map y to value", {
  ma_plot <- plot_ma_10_20days("MSFT")
  expect_true("value"  == rlang::get_expr(ma_plot$mapping$y))
})
