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
  pathfile <- paste0("../data/", stock_ticker,".csv")
  data <- readr::read_csv(pathfile)  # read stock data
  
  source("moving_average.R")  # inherit the function for plotting
  period = 200 
  ma_df <- moving_average(stock_ticker, period)  # obtain dataframe for plotting
  ma_df$og_data <- paste(data[period:nrow(data), ]$adjusted)  # add the original datapoints to same df.
  
  ma_plot <- 
    ggplot2::ggplot(tail(ma_df, 200)) +
    ggplot2::aes(x = date, y = og_data) + 
    ggplot2::geom_point() +
    ggplot2::aes(x = date, y = adjusted, color = "red") + 
    ggplot2::geom_point() +
    ggplot2::scale_y_discrete("Close Price", breaks = 25)
  
  return(ma_plot)
}
