#Given one stock symbol, your code needs to be able to: 
#(1) Display histograms for your data by stock symbol. 
#(2) Display a normal probability plot to see if the data is approximately normal. 
#(3) Create (approximate) confidence intervals for the means and variances given a confidence level. 
#(4) Perform a regression of the log-return on time.

#Given two stock symbols, your code needs to be able to: 
#(1) Test the equality of the two population means. 
#(2) Perform a regression of one log-return on the other.

#All regression output needs to include intercept and slope estimates, 
#a diagram of the data with the least-squares line, 
#a graphical depiction of residuals, 
#and R^2.

# functions:
#  PlotPrice()
#  PlotHistogram()
#  PlotVsNormal()
#  PlotMeanConfidenceInterval(percent)
#  CalculateVarianceConfidenceInterval(percent)
#  PlotLogReturnRegression()
#
#PlotTwoStocks(symbol1, symbol2)
#  MeanEqualityTest()
#  PlotTwoStockRegression()
#
#Regression utilities:
#  RegressionCalcSlopeAndIntercept()
#  RegressionPlotResiduals()
#
#Data utilities:
#  ReadStock(symbol)
#  LogReturn(prices)

# libraries
#   dplyr - data manipulation
#   forecast - time series plotting and regression
#   ggplot - plotting
#   lattice - plotting
#   tseries - time series analysis and computational finance

#  ReadStock(symbol)
#   arguments:
#     symbol - string representing the stock symbol
#   return value:
#     data frame of the historical stock closing price
ReadStock<-function(symbol, ascending=TRUE, monthly=FALSE) {
  filename <- paste("data/",symbol,"/",symbol,".csv", sep="")
  prices<-read.csv(file=filename, head=TRUE, sep=",")
  df<-dplyr::select(prices, Date, Open, Close)
  if (monthly) {
    # Yahoo monthly
    dateFormat="%Y-%m"
    ret<-data.frame(Open=df$Open,
                    Close=df$Close, 
                    Date=as.yearmon(df$Date))
    ret<-data.frame(Open=ret$Open, 
                   Close=ret$Close, 
                   Date=ret$Date,
                   Month=format(ret$Date, "%m"), 
                   Year=format(ret$Date, "%Y"))
    } else {
    # Google daily
    dateFormat="%d-%B-%y"
    ret<-data.frame(Open=df$Open,
                    Close=df$Close, 
                    Date=timeDate(stringr::str_c(df$Date), 
                                  format=dateFormat))
    # for some reason, the date column name is not set correctly,
    # so set it here
    colnames(ret)<-c("Open", "Close", "Date")
  }
  if (ascending) {
    #df<-df[rev(order(as.Date(df$Date, format="%d-%m-%Y"))),]    
    ret<-ret[order(ret[["Date"]]),]    
  } else {
    ret<-ret[order(rev(ret[["Date"]])),]
  }
    
  # Add a column so that we can handle this as a regular regression
  # instead of a time series regression, which is problematic (for me, at least)
  ret$Val<-seq.int(nrow(ret))
  ret
}

#LogReturn(prices)
#   arguments:
#     prices - vector containing the historical stock prices
#   return value:
#     vector containing log returns per interval 
LogReturn<-function(price) {
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

################################################################################
#
# Plotting functions
#
################################################################################

# PlotPrice(dfStock)
#    arguments:
#      dfStock - data frame containing stock closing price and date
#      strSymbol - stock ticker symbol
#    return value:
#      none
PlotPrice<-function(dfStock, strSymbol, graphTitle=NULL) {
  plot(dfStock$Date, log(dfStock$Close), xlab="Date", 
       ylab=paste(strSymbol, "closing price"), type="l", 
       col="maroon" )
  abline(lm(log(dfStock$Close) ~ dfStock$Date), col="black")
  if (is.null(graphTitle)) {
    title(paste("Closing prices for ", strSymbol))
  } else {
    title(graphTitle)
  }
}
# PlotHistogram(dfLogReturn, breaks)
#    arguments:
#      dfLogReturn - vector containing log returns per interval
#      breaks - number of vertical bars to use in the plot
#    return value:
#      none
PlotHistogram<-function(dfLogReturn, breaks) {
  hist(dfLogReturn, breaks=30)  
}

#  PlotVsNormal()
#    arguments:
#      dfLogReturn - vector containing log returns per interval
#    return value:
#      none
PlotVsNormal<-function(dfLogReturn) {
  curve(dnorm(x, mean(dfLogReturn), sd(dfLogReturn)), add=TRUE, 
        col="maroon", lwd=2)
}

#  PlotMeanConfidenceInterval()
#    arguments:
#      dfLogReturn - vector containing log returns per interval
#      percent - desired confidence interval, expressed in percent
#    return value:
#      list containing the lower bound of the interval in the first element,
#      and the upper bound in the second element
PlotMeanConfidenceInterval<-function(dfLogReturn, percent) {
  width <- qnorm(1-(100-percent)/200)*sd(dfLogReturn)/sqrt(length(dfLogReturn))
  lowerBound <- mean(dfLogReturn) - width
  upperBound <- mean(dfLogReturn) + width
  orderedLog<-dfLogReturn[order(dfLogReturn)]
  i <- orderedLog >= lowerBound & orderedLog <= upperBound
  normalLine = dnorm(orderedLog, mean(orderedLog), sd(orderedLog))
  lines(orderedLog, normalLine)
  polygon(c(lowerBound,orderedLog[i],upperBound), c(0,normalLine[i],0), col="blue") 
  list(lowerBound, upperBound)
}

#  CalculateVarianceConfidenceInterval(percent)
#    arguments:
#      dfLogReturn - vector containing log returns per interval
#      percent - desired confidence interval, expressed in percent
#    return value:
#      list containing the lower bound of the interval in the first element,
#      and the upper bound in the second element
CalculateVarianceConfidenceInterval<-function(dfLogReturn, percent) {
  # (n-1)s^2/sigma^2 has a chi-squared(n-1) distribution
  sampleVariance=var(dfLogReturn)
  n = length(dfLogReturn)
  lowerBound = (n-1)*sampleVariance/qchisq((100+percent)/200, n-1)
  upperBound = (n-1)*sampleVariance/qchisq(1-(100+percent)/200, n-1)
  list(lowerBound, upperBound)
}
  
#  PlotLogReturnRegression()
#    arguments:
#      dep - vector containing dependent variable
#      indep - vector containing independent variable
#    return value:
#      the linear regression model
PlotRegression<-function(dep, indep, graphType) {
  plot(dep ~ indep, type=graphType)
  regress = lm(dep ~ indep)
  abline(regress)
  regress
}

# PlotTwoStockRegression(strSymbolA, strSymbolB)
#    arguments:
#      strSymbolA - first stock symbol
#      strSymbolB - second stock symbol
#    return value:
#      the regression model of logreturn of B vs. log return of A
#
# This function calculates the log returns of each of the stock
# symbols, based on their daily closing prices over the past year.
# It then graphs the prices with stock A on the X-axis, and stock B
# on the Y-axis. Finally, it calculates and graphs the linear 
# regression line of stock B vs. stock A.
PlotTwoStockRegression<-function(strSymbolA, strSymbolB, graphingType) {
  stockA <- ReadStock(strSymbolA)
  stockB <- ReadStock(strSymbolB)
  logStockA <- LogReturn(stockA$Close)
  logStockB <- LogReturn(stockB$Close)
  PlotRegression(logStockA, logStockB, graphType=graphingType)
}

PlotTwoLogReturnRegression<-function(logStockA, logStockB) {
  plot(logStockB ~ logStockA)
  regress <- lm(logStockB ~ logStockA)
  abline(regress)
  regress
}

PlotResiduals<-function(lMod, indep) {
  plot(indep, resid(lMod))
  abline(0,0)
}

MeanEqualityTest<-function(strSymbolA, strSymbolB) {
  stockA <- ReadStock(strSymbolA)
  stockB <- ReadStock(strSymbolB)
  logStockA <- LogReturn(stockA$Close)
  logStockB <- LogReturn(stockB$Close)
  t.test(logStockA, logStockB)
}

######################################################################
#
# Beginning of main code execution
#
######################################################################

library(dplyr)
library(lattice)
library(forecast)
library(ggplot2)
library(lattice)
library(tseries)
library(zoo)

# Demonsrate plotting and calculation functions

adbe<-ReadStock("adbe")
adbeLog<-LogReturn(adbe$Close)
PlotPrice(adbe, "ADBE")
PlotHistogram(adbeLog)
PlotVsNormal(adbeLog)
meanCI<-PlotMeanConfidenceInterval(adbeLog, 90)
meanCI<-PlotMeanConfidenceInterval(adbeLog, 95)
varCI<-CalculateVarianceConfidenceInterval(adbeLog, 90)
varCI<-CalculateVarianceConfidenceInterval(adbeLog, 95)
adbeModel = PlotRegression(adbeLog, adbe$Val[2:length(adbe$Val)], graphType="l")
PlotResiduals(adbeModel, adbe$Val[2:length(adbe$Val)])
summary(adbeModel)

msft<-ReadStock("msft")
msftLog<-LogReturn(msft$Close)
PlotPrice(msft, "MSFT", "Daily closing price for Microsoft")
PlotHistogram(msftLog)
PlotVsNormal(msftLog)
meanCI<-PlotMeanConfidenceInterval(msftLog, 95)
varCI<-CalculateVarianceConfidenceInterval(msftLog, 95)
msftModel<-PlotRegression(msftLog, msft$Val[2:length(msft$Val)], graphType="l")
PlotResiduals(msftModel, msft$Val[2:length(msft$Val)])
summary(msftModel)

twoStockModel=PlotTwoStockRegression("adbe", "msft", graphingType="p")

# Plotting and calculations for the January predictor wives' tale
gspc<-ReadStock("gspc", monthly=TRUE)
monthNumStrings=c("01","02","03","04","05","06",
                  "07","08","09","10","11","12")

# Build a list of dataframes that contain the historical monthly data
gspcList<-list(length(monthNumStrings))
for (i in 1:length(monthNumStrings)) {
  gspcList[[i]] <- data.frame(filter(gspc, Month==monthNumStrings[i]))
}

# Calculate the historical return for each month
gspcLogs<-list(length(monthNumStrings))
for (j in 1:length(monthNumStrings)) {
  gspcLogs[[j]] <- LogReturnInterval(gspcList[[j]]$Open, gspcList[[j]]$Close)
}

# Calculate the annual log returns
gspcYearLog=LogReturnInterval(gspcList[[1]]$Open, gspcList[[12]]$Close)

# Compare the monthly returns against the annual returns, using the 
# monthly returns as the independent variable
r.squared.list<-list(length(monthNumStrings))
for (k in 1:length(monthNumStrings)) {
  plot(gspcLogs[[k]], gspcYearLog, type="p")
  gspcModel<-PlotRegression(gspcYearLog, gspcLogs[[k]], graphType="p")
  PlotResiduals(gspcModel, gspcLogs[[k]])
  r.squared.list[[k]]<-summary(gspcModel)$r.squared
}

# Find the largest r squared
largest.r.squared.month<-month.abb[which.max(r.squared.list)]
largest.r.squared<-r.squared.list[[which.max(r.squared.list)]]

# Calculate January vs. the rest of the year
gspcRestOfYearLog=LogReturnInterval(gspcList[[2]]$Open, gspcList[[12]]$Close)
plot(gspcLogs[[1]], gspcRestOfYearLog, type="p")
gspcModel<-PlotRegression(gspcRestOfYearLog, gspcLogs[[1]], graphType="p")
PlotResiduals(gspcModel, gspcLogs[[1]])

summary(gspcModel)
