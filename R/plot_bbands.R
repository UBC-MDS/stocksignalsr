#' Plot stock price and corresponding upper and lower Bollinger bands
#' for the last 200 trading days.
#'
#' @param stock_ticker string Ticker symbol of the
#' stock for which the plot is created such as "MSFT"
#'
#' @return A line chart showing price data and corresponding upper
#' and lower Bollinger bands.
#'
#' @export
#'
#' @examples
#' plot_bbands("MSFT")
plot_bbands <- function(stock_ticker){
  tail <- up <- dn <- adjusted <- NULL
  bbands_df <- get_bbands(stock_ticker)
  bbands_df <- tail(bbands_df,252)
  options(repr.plot.height = 7, repr.plot.width = 10)
  bbands_plot <-
    ggplot2::ggplot(bbands_df, ggplot2::aes(x=date)) +
    ggplot2::geom_line(ggplot2::aes(y = up), color = "red") +
    ggplot2::geom_line(ggplot2::aes(y = dn), color = "green") +
    ggplot2::geom_line(ggplot2::aes(y = adjusted), color="blue", linetype="twodash") +
    ggplot2::labs(y = "Adjusted close price", x = "Date", fill = "") +
    ggplot2::ggtitle("Bollinger bands last 252 trading days") +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 15, face = "bold"),
                   axis.text.x = ggplot2::element_text(size = 12, angle = 90),
                   axis.text.y = ggplot2::element_text(size = 12, angle = 0),
                   axis.title = ggplot2::element_text(size = 12),
                   legend.text = ggplot2::element_text(size = 12),
                   legend.title = ggplot2::element_text(size = 12, face = "bold"))
  return(bbands_plot)

}
