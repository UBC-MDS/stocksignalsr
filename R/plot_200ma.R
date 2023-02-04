#' Plot stock price and corresponding 200 day moving average.
#'
#' @param stock_ticker string
#' name of the stock for which the plot is created
#'
#' @return A line chart showing price data and corresponding 200 day
#' moving average line for a stock.
#'
#' @export
#'
#' @examples
#' plot_200ma("MSFT")
plot_200ma <- function(stock_ticker){
  adjusted <- tail <- NULL
  pathfile <- paste0("../data/", stock_ticker,".csv")
  if (file.exists(pathfile)){
    data <- readr::read_csv(pathfile)
  } else {
    stocksignalsr::get_data(stock_ticker, "1986-03-13")
    data <- readr::read_csv(pathfile)  # read stock data
  }
  price <- data|>
    dplyr::select(adjusted)
  ma <- TTR::SMA(price,n=200)

  data$ma<- ma
  data <- tail(data,252)

  ma_plot <-
    ggplot2::ggplot(data, ggplot2::aes(x=date)) +
    ggplot2::geom_line(ggplot2::aes(y = ma), color = "red") +
    ggplot2::geom_line(ggplot2::aes(y = adjusted), color="blue", linetype="twodash") +
    ggplot2::labs(y = "Stock price (USD)", x = "Date", fill = "") +
    ggplot2::ggtitle("200-day Moving average\n vs Adjusted closing price") +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 15, face = "bold"),
                   axis.text.x = ggplot2::element_text(size = 12, angle = 90),
                   axis.text.y = ggplot2::element_text(size = 12, angle = 0),
                   axis.title = ggplot2::element_text(size = 12),
                   legend.text = ggplot2::element_text(size = 12),
                   legend.title = ggplot2::element_text(size = 12, face = "bold"))

  return(ma_plot)
}
