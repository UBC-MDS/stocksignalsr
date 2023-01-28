#' Get Bollinger bands
#' Calculates upper and lower Bollinger bands based on standard deviation
#' of stock's close prirce over the past 20 days
#'
#' @param stock_ticker String. Stock ticker of company to be analysed
#'
#' @return tibble containing upper and lower Bollinger bands
#' adjusted closing price, moving average, date
#' @export
#'
#' @examples
#' get_bbands("MSFT")
get_bbands <- function(stock_ticker){
  adjusted <- mavg <- dn <- up <- NULL #setting null values for column variables
  pathfile <- paste0("../data/", stock_ticker,".csv")
  if (file.exists(pathfile)){
    data <- readr::read_csv(pathfile)
  } else {
    stocksignalsr::get_data(stock_ticker, "1986-03-13")
    data <- readr::read_csv(pathfile)  # read stock data
  }
  price <- data|>
    dplyr::select(adjusted) #get adjusted price values
  bbands <- TTR::BBands(price)
  bbands <- cbind(data, bbands) |>
    dplyr::select(date,adjusted, mavg,dn, up)
  return(bbands)
}
