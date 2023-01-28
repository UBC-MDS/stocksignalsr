#' Downloads all available historical daily data
#' for stock_ticker from Yahoo finance and stores it as a
#' csv file in ata folder. If data folder does not exist
#' it is created.
#'
#' @param stock_ticker string
#' Ticker of the stock such as 'MSFT
#' @param start_date
#' starting date from which data needs to be downloaded
#'
#'
#' @export
#'
#' @examples
#' get_data("MSFT", "1986-03-13")
get_data <- function(stock_ticker, start_date){
  stock_data  <- tidyquant::tq_get(stock_ticker,
          get = "stock.prices", from = start_date)
  save_to <-  "data"
  pathfile <-  paste0(save_to,"/",stock_ticker,".csv")
  if (file.exists(save_to)){
    readr::write_csv(x = stock_data, pathfile)
  } else {
    dir.create(save_to)
    readr::write_csv(x = stock_data, pathfile)

  }
}
