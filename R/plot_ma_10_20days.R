#' Plot stock price and corresponding 10 and 20 day moving average.
#'
#' @param stock_symbol Ticker symbol of the stock for which the plot is created
#'
#' @return A line chart showing price data and corresponding 10 and 20 day moving average line for a stock.
#' @export
#'
#' @examples plot_ma_10_20days("MSFT")
#'
plot_ma_10_20days <- function(stock_symbol) {
  adjusted <- value <- variable <- NULL
  pathfile <- paste0("../data/", stock_symbol,".csv")
  data <- readr::read_csv(pathfile)  # read stock data

  period_plot = 200 # set how many days for plotting, default=200

  ma_df_10 <- stocksignalsr::moving_average(stock_symbol, 10)  # obtain dataframe for MA10
  ma_df_10 <- ma_df_10 |>
    dplyr::rename(moving_average_10 = adjusted) |>
    dplyr::slice_tail(n = period_plot)

  ma_df_20 <- stocksignalsr::moving_average(stock_symbol, 20)  # obtain dataframe for MA20
  ma_df_20 <- ma_df_20 |>
    dplyr::rename(moving_average_20 = adjusted) |>
    dplyr::slice_tail(n = period_plot)

  data <- data |> # obtain dataframe for stock price
    dplyr::select(date,adjusted) |>
    dplyr::rename(stock_price = adjusted) |>
    dplyr::slice_tail(n = period_plot)

  # join three dataframe with MA10, MA20, & stork price
  ma_df <- dplyr::left_join(ma_df_10, ma_df_20)
  ma_df <- dplyr::left_join(ma_df, data)

  # melt dataframe for plotting purposes
  ma_df <- reshape2::melt(ma_df,
                          na.rm = TRUE,
                          id = "date")
  ma_df$value <- as.numeric(ma_df$value) # set proper datatype for plotting

  ma_plot <-
    ggplot2::ggplot(ma_df) +
    ggplot2::aes(x = date,
                 y = value,
                 color = variable) +
    ggplot2::geom_line() +
    ggplot2::ggtitle("10, 20-day moving average with closing price") +
    ggplot2::labs(x = "Date",
                  y = "Stock Price, US$",
                  color = "Category")

  return(ma_plot)
}
