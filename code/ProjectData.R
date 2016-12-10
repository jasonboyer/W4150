# ProjectData.R - functions to manipulate data in support of the stock market project

#  ReadStock(symbol)
#   arguments:
#     symbol - string representing the stock symbol
#     ascending - boolean indicating whether to sort earliest to lates (default)
#     monthly - boolean indicating whether to remove day from date values
#   return value:
#     data frame of the historical stock closing price
ReadStock <- function(symbol, ascending=TRUE, monthly=FALSE) {
  filename <- paste("data/",symbol,"/",symbol,".csv", sep="")
  prices <- read.csv(file=filename, head=TRUE, sep=",")
  df <- dplyr::select(prices, Date, Open, Close)
  if (monthly) {
    # Yahoo monthly
    dateFormat = "%Y-%m"
    ret<-data.frame(Open = df$Open,
                    Close = df$Close, 
                    Date = as.yearmon(df$Date))
    ret<-data.frame(Open = ret$Open, 
                    Close = ret$Close, 
                    Date = ret$Date,
                    Month = format(ret$Date, "%m"), 
                    Year = format(ret$Date, "%Y"))
  } else {
    # Google daily
    dateFormat = "%d-%B-%y"
    ret <- data.frame(Open=df$Open,
                      Close=df$Close, 
                      Date=timeDate(stringr::str_c(df$Date), 
                                    format = dateFormat))
    # for some reason, the date column name is not set correctly,
    # so set it here
    colnames(ret) <- c("Open", "Close", "Date")
  }
  if (ascending) {
    ret <- ret[order(ret[["Date"]]),]    
  } else {
    ret <- ret[order(rev(ret[["Date"]])),]
  }
  
  # Add a column so that we can handle this as a regular regression
  # instead of a time series regression, which is problematic (for me, at least)
  ret$Val <- seq.int(nrow(ret))
  ret
}

#LogReturn(prices)
#   arguments:
#     prices - vector containing the historical stock prices
#   return value:
#     vector containing log returns per interval 
LogReturn <- function(price) {
  diff(log(price))
}

#LogReturnInterval(openPrices, closingPrices)
#   arguments:
#     openPrices - vector containing the historical opening stock prices
#     closingPrices - vector containing the historical closing stock prices
#   return value:
#     vector containing log returns per interval 
LogReturnInterval<-function(openPrices, closingPrices) {
  log(closingPrices)-log(openPrices)
}
