#' Yield Channel
#' 
#' Shows the dividend channels of a dividend stock. 
#' 
#' If a dividend stock's price is below the channel it might indicate that it is 
#' undervalued based on the historical dividend yields. If it is above the 
#' channel it might indicate that it is overvalued. The dividend bands show bands 
#' of slightly overvalued to highly overvalued and slightly undervalued and highly
#' undervalued. 
#' 
#' The calculation for the dividend channels are based on the 5\% and the 95\% value 
#' of the dividend percentages and smoothed out via a loess regression. The stock 
#' is undervalued if its current dividend yield is higher than the average dividend 
#' yield of the past based on the chosen look back period. The shorter the look 
#' back period, the faster a stock might appear under- or overvalued. 
#' 
#' The dividend bands show ranges between 5\%-25\%, 25\%-50\%, 50\%-75\% and 75\%-95\%
#' percentage. These bands are not smoothed out and will show the steps in 
#' dividend increases.
#' 
#' More information can be found in \href{https://seekingalpha.com/article/4393616-revisiting-yield-channel-charts}{this Seeking Alpha} article.
#'
#' @param ticker A stock symbol 
#' @param n number of years to look back. Ideally one or two business cycles. 
#' Default look back period is 12 years.
#' @param dividend_channel Show the dividend channels. Default is TRUE.
#' @param dividend_bands Show the dividend bands. Default is TRUE.
#'
#' @return Returns a chart showing the price, the dividend channels and dividend 
#' bands.
#' @export
#'
#' @examples
#' \dontrun{
#' # shows the yield channel chart for Merck.
#' yield_channel("MRK")
#' }
yield_channel <- function(ticker, n = 12, dividend_channel = TRUE, dividend_bands = TRUE){
  
  # input tests
  if(length(ticker) > 1) {
    stop("ticker should only have have one value", 
         call. = FALSE)
  }
  
  if(n <= 0) {
    stop("n should have a positive value", 
         call. = FALSE)
  }
  
  if(!is.logical(dividend_channel)) {
    stop("dividend_channel has to be logical value (TRUE or FALSE).", 
         call. = FALSE)
  }
  
  if(!is.logical(dividend_bands)) {
    stop("dividend_bands has to be logical value (TRUE or FALSE).", 
         call. = FALSE)
  }
  
  # current year minus 12 is +/- 2 business cycles. (or at least 1)
  from = paste0(as.numeric(format.Date(Sys.Date(), "%Y")) - n, "-01-01")
  
  # get the data
  stock_data <- quantmod::getSymbols(ticker, from = from, auto.assign = FALSE)
  div_data <- quantmod::getDividends(ticker, from = from, split.adjust = FALSE)
  
  clean_column_names <- function(ticker, xts_data){
    # remove ticker symbol and "." from column names
    tolower(gsub(paste0(ticker, "."), "", colnames(xts_data)))
  }
  
  # clean column names
  colnames(stock_data) <- clean_column_names(ticker, stock_data)
  colnames(div_data) <- clean_column_names(ticker, div_data)
  
  # turn into data.frames
  stock_data <- zoo::fortify.zoo(stock_data, names = "date")
  div_data <- zoo::fortify.zoo(div_data, names = "date")
  
  
  divs_total <- div_data %>% 
    dplyr::group_by(year = lubridate::year(date)) %>%
    dplyr::add_tally() %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(n = max(n),
                  annual_div = .data$div * n) %>% 
    dplyr::select(-.data$year)
  
  total <- stock_data %>% 
    dplyr::select(date, close) %>% 
    dplyr::left_join(divs_total) %>% 
    tidyr::fill(c(.data$div, .data$n, .data$annual_div), .direction = "down") %>% 
    dplyr::filter(!is.na(.data$div)) %>% 
    dplyr::mutate(div_perc = .data$annual_div / .data$close,
                  perc_95 = stats::quantile(.data$div_perc, 0.95),
                  perc_05 = stats::quantile(.data$div_perc, 0.05),
                  perc_75 = stats::quantile(.data$div_perc, 0.75),
                  perc_25 = stats::quantile(.data$div_perc, 0.25),
                  perc_50 = stats::quantile(.data$div_perc, 0.50),
                  lower = .data$annual_div / .data$perc_95,
                  upper = .data$annual_div / .data$perc_05,
                  lower_25 = .data$annual_div / .data$perc_75,
                  upper_75 = .data$annual_div / .data$perc_25,
                  middle = .data$annual_div / .data$perc_50
    )
  
  p <- ggplot2::ggplot(total, ggplot2::aes(x = date)) 
  
  # add dividend bands
  if(dividend_bands == TRUE){
    p <- p +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$upper_75, ymax = .data$upper), fill = "dark red") + 
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$middle, ymax = .data$upper_75), fill = "red") + 
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower_25, ymax = .data$middle), fill = "green") + 
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower, ymax = .data$lower_25), fill = "dark green") 
  }
  
  # add loess lines
  if(dividend_channel == TRUE) {
    p <- p + 
      ggplot2::geom_smooth(ggplot2::aes(y = .data$upper), formula = y ~ x, method = "loess") + 
      ggplot2::geom_smooth(ggplot2::aes(y = .data$lower), formula = y ~ x, method = "loess")
  } 
  
  # print plot
  p + ggplot2::geom_line(ggplot2::aes(y = .data$close)) +
    ggplot2::labs(title = paste0("Dividend channel for ", ticker),
                  y = "close",
                  x = "date") + 
    ggplot2::theme_classic() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  
}