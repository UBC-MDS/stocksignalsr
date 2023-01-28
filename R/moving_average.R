#' Calculate the moving average of a stock price for a given range of days
#'
#' @param stock_ticker A string corresponding to the name of the stock.
#' @param period Number of days for which the moving average is computed
#'
#' @return moving_avg A tibble representing the moving average of a stock
#' price over the specified period.
#'
#' @export
#'
#' @examples
#' moving_average("MSFT", 20)
moving_average <- function(stock_ticker, period){
  pathfile <- paste0("../data/", stock_ticker,".csv")
  data <- readr::read_csv(pathfile)  # read stock data
  ma <- zoo::rollmean(data |> dplyr::select(adjusted), period)  # calculate moving average
  
  # create df for plotting 
  
  ma_df <- data.frame(date = data[period:nrow(data), ]$date, 
                      mov_avg = ma)
  return(ma_df)
}
