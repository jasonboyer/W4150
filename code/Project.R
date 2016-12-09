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
#   base - for adjusting axis scales
#   dplyr - data manipulation
#   forecast - time series plotting and regression
#   ggplot - plotting
#   lattice - plotting
#   plotly - plotting and uploading plots to the web
#   scales - adjusting axis scales
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
#      ggplot object
PlotPrice<-function(dfStock, strSymbol, graphTitle=NULL) {
  if (is.null(graphTitle)) {
    gTitle<-paste("Log of Closing Prices for ", strSymbol)
  } else {
    gTitle<-graphTitle
  }
  lMod<-lm(dfStock$Close ~ dfStock$Val)
  intercept<-coefficients(lMod)[1]
  slope<-coefficients(lMod)[2]
  info=paste("Regression: ",
            "Intercept:  ", intercept,
            " Slope: ", slope)  
  ret<-ggplot(dfStock, 
              aes(x=Date, y=Close)) +
    geom_line() +
    geom_smooth(method="lm") +
    labs(title = gTitle, x="Date", 
       y=paste(strSymbol, "Closing Price")) 
  print(ret)
  list(ret)
}
# PlotHistogram(dfLogReturn, breaks)
#    arguments:
#      dfLogReturn - vector containing log returns per interval
#      breaks - number of vertical bars to use in the plot
#    return value:
#      none
PlotHistogram<-function(dfLogReturn, strSymbol, numBins=30, graphTitle=NULL) {
  if (is.null(graphTitle)) {
    gTitle<-paste("Log returns of closing prices for ", strSymbol)
  } else {
    gTitle<-graphTitle
  }
  ret<-ggplot(data.frame(LogReturn=dfLogReturn), aes(LogReturn)) +
    labs(title = gTitle, x="Log Return", 
         y="Frequency") +
    geom_histogram(bins=numBins)
  list(ret)
}

#  PlotVsNormal()
#    arguments:
#      dfLogReturn - vector containing log returns per interval
#    return value:
#      none
PlotVsNormal<-function(dfLogReturn, existingPlot=NULL) {
  if (is.null(existingPlot)) {
    existingPlot = ggplot()
  }
  ret<-existingPlot +
    stat_function(fun=dnorm, args=list(mean=mean(dfLogReturn),
                                       sd=sd(dfLogReturn)),
                  color="maroon")
    
#  curve(dnorm(x, mean(dfLogReturn), sd(dfLogReturn)), add=TRUE, 
#        col="maroon", lwd=2)
  list(ret)
}

#  PlotMeanConfidenceInterval()
#    arguments:
#      dfLogReturn - vector containing log returns per interval
#      percent - desired confidence interval, expressed in percent
#    return value:
#      list containing the lower bound of the interval in the first element,
#      and the upper bound in the second element
PlotMeanConfidenceInterval<-function(dfLogReturn, percent, 
                                     intervalColor = "blue",
                                     intervalAlpha = 0.5,
                                     existingPlot = NULL,
                                     graphTitle = NULL) {
  width <- qt(1-(100-percent)/200, length(dfLogReturn)-1)*sd(dfLogReturn)#/sqrt(length(dfLogReturn))
  lowerBound <- mean(dfLogReturn) - width
  upperBound <- mean(dfLogReturn) + width
  orderedLog<-dfLogReturn[order(dfLogReturn)]
  normalLine = dnorm(orderedLog, mean(orderedLog), sd(orderedLog))
  gg<-subset(data.frame(x=orderedLog, y=normalLine), 
             x >= lowerBound & x <= upperBound)
  if (is.null(existingPlot)) {
    existingPlot <- ggplot(data=gg)
  }
  ret<-existingPlot + geom_line(data=gg, aes(x = x, y = y)) +
    geom_ribbon(data=gg, aes(x = x, ymax = y), ymin = 0, 
                fill = intervalColor, alpha = intervalAlpha) +
    scale_x_continuous(limits = c(min(orderedLog), max(orderedLog)))
 list(ret)
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
#      the plot object
PlotRegression<-function(dep, indep, linearModel = NULL,
                         graphType = "p", xtitle = NULL, ytitle = NULL,
                         graphTitle = NULL) {
  if (is.null(xtitle)) {
    xtitle <- "indep"
  }
  if (is.null(ytitle)) {
    ytitle <- "dep"
  }
  if (is.null(graphTitle)) {
    graphTitle = paste("Linear Regression of ", xtitle, " and ", ytitle)
  }
  
  ret<-qplot(indep, dep, xlab = xtitle, ylab = ytitle,
             main = graphTitle) 

    if (graphType == "l") {
    ret <- ret + geom_line()
  } else {
    ret<- ret + geom_point()
  }
  ret <- ret + geom_smooth(method="lm")
  if (is.null(linearModel)) {
    linearModel<-lm(dep ~ indep)
  }
  print(summary(linearModel))
  list(ret)
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
PlotTwoStockRegression<-function(strSymbolA, strSymbolB, graphingType,
                                 xtitle = NULL, ytitle = NULL,
                                 graphTitle = NULL) {
  stockA <- ReadStock(strSymbolA)
  stockB <- ReadStock(strSymbolB)
  logStockA <- LogReturn(stockA$Close)
  logStockB <- LogReturn(stockB$Close)
  PlotRegression(logStockB, logStockA, graphType=graphingType, 
                 xtitle = xtitle, ytitle = ytitle, graphTitle = graphTitle)
}

PlotTwoLogReturnRegression<-function(logStockA, logStockB) {
  ret<-qplot(logStockA, logStockB) + geom_point() +
    geom_smooth(method="lm")
  regress <- lm(logStockB ~ logStockA)
  print(summary(regress))
}

# PlotResiduals
# 
# arguments
#   lMod - linear regression model
#   indep - independent variable 
#   xtitle - label for x-axis
#   ytitle - label for y-axis
#   graphTitle - main label for plot
# return value
#   ggplot object
PlotResiduals<-function(lMod, indep, xtitle = NULL, ytitle = NULL,
                        graphTitle = NULL) {
  if (is.null(xtitle)) {
    xtitle <- "indep"
  }
  if (is.null(ytitle)) {
    ytitle <- "Residuals"
  }
  if (is.null(graphTitle)) {
    graphTitle = "Residuals of Linear Regression"
  }
  
  ret <- qplot(indep, resid(lMod),
               xlab = xtitle, ylab = ytitle,
               main = graphTitle) + geom_point() + 
    geom_hline(yintercept=0, color = I("blue"))
  list(ret)
}

MeanEqualityTest<-function(strSymbolA, strSymbolB) {
  stockA <- ReadStock(strSymbolA)
  stockB <- ReadStock(strSymbolB)
  logStockA <- LogReturn(stockA$Close)
  logStockB <- LogReturn(stockB$Close)
  print(t.test(logStockA, logStockB))
}

######################################################################
#
# Beginning of main code execution
#
######################################################################

# Load some libraries
library(base)
library(dplyr)
library(lattice)
library(forecast)
library(ggplot2)
library(lattice)
library(plotly)
library(scales)
library(tseries)
library(zoo)

# Demonstrate plotting and calculation functions

plotCount<-0
plots<-list(50)

# Calculations for symbol ADBE
adbe<-ReadStock("adbe")
adbeLog<-LogReturn(adbe$Close)

# Plot linear regression of returns
plots[plotCount<-plotCount+1]<-PlotPrice(adbe, "ADBE")
print(ggplotly(plots[[plotCount]]))

# Plot residuals of linear regression
adbeMod <- lm(adbe$Close ~ adbe$Val)
plots[plotCount<-plotCount+1] <- PlotResiduals(adbeMod, adbe$Val)
print(ggplotly(plots[[plotCount]]))

plots[plotCount<-plotCount+1]<-PlotHistogram(adbeLog, "ADBE", graphTitle = "ADBE Nov 2015- Nov 2016")
print(ggplotly(plots[[plotCount]]))

plots[plotCount<-plotCount+1]<-PlotVsNormal(adbeLog, plots[[plotCount]])
print(ggplotly(plots[[plotCount]]))

plots[plotCount<-plotCount+1]<-PlotMeanConfidenceInterval(adbeLog, 90, 
                                                          intervalColor = "lightblue",
                                                          existingPlot = plots[[plotCount]])
print(ggplotly(plots[[plotCount]]))
# Skip publishing step
plotCount <- plotCount - 1

plots[plotCount<-plotCount+1] <- PlotMeanConfidenceInterval(adbeLog, 95,
                                                            intervalColor="pink",
                                                            existingPlot = plots[[plotCount]])
print(ggplotly(plots[[plotCount]]))
# Skip publishing step
plotCount <- plotCount - 1

# Calculate and print confidence intervals for the variance
varCI<-CalculateVarianceConfidenceInterval(adbeLog, 90)
message(sprintf("90%% confidence interval for variance in ADBE log returns: %s", 
                toString(varCI)))
varCI<-CalculateVarianceConfidenceInterval(adbeLog, 95)
message(sprintf("95%% confidence interval for variance in ADBE log returns: %s",
                toString(varCI)))

# Calculate, plot, and summarize the linear regression model
adbeModel <- lm(adbeLog ~ adbe$Val[2:length(adbe$Val)])
plots[plotCount<-plotCount+1] <- PlotRegression(adbeLog, adbe$Val[2:length(adbe$Val)], linearModel=adbeModel,
                                                graphType="l", ytitle = "ADBE Log Return", xtitle = "Time",
                                                graphTitle = "ADBE Log Return Over Time")
print(ggplotly(plots[[plotCount]]))


# Plot the residuals of the linear regression model
plots[plotCount<-plotCount+1] <- PlotResiduals(adbeModel, adbe$Val[2:length(adbe$Val)],
                                               xtitle = "Time",
                                               graphTitle = "Residuals of Linear Regression of ADBE Log Returns Over Time")
print(ggplotly(plots[[plotCount]]))

print(summary(adbeModel))

# Calulations for symbol MSFT
msft<-ReadStock("msft")
msftLog<-LogReturn(msft$Close)

plots[plotCount<-plotCount+1] <- PlotPrice(msft, "MSFT", "Daily closing price for Microsoft")
print(ggplotly(plots[[plotCount]]))

plots[plotCount<-plotCount+1] <- PlotHistogram(msftLog, "MSFT", graphTitle = "MSFT Nov 2015- Nov 2016")
print(ggplotly(plots[[plotCount]]))

plots[plotCount<-plotCount+1] <- PlotVsNormal(msftLog, plots[[plotCount<-plotCount]])
print(ggplotly(plots[[plotCount]]))

plots[plotCount<-plotCount+1] <- PlotMeanConfidenceInterval(msftLog, 95, 
                                                            intervalColor="lightgreen",
                                                            intervalAlpha="0.7",
                                                            existingPlot = plots[[plotCount]])
print(ggplotly(plots[[plotCount]]))
# Skip publishing step. The confidence interval plots don't work on plotly
plotCount <- plotCount - 1

varCI<-CalculateVarianceConfidenceInterval(msftLog, 95)
message(sprintf("95%% confidence interval for variance in MSFT log returns: %s",
                toString(varCI)))

msftModel <- lm(msftLog ~ msft$Val[2:length(msft$Val)])
plots[plotCount<-plotCount+1] <- PlotRegression(msftLog, msft$Val[2:length(msft$Val)],
                                                graphType="l",
                                                xtitle = "Time",
                                                ytitle = "MSFT Log Return",
                                                graphTitle = "NSFT Log Return Over Time")
print(ggplotly(plots[[plotCount]]))

plots[plotCount<-plotCount+1] <- PlotResiduals(msftModel, msft$Val[2:length(msft$Val)],
                                               xtitle = "Time",
                                               graphTitle = "Residuals of Linear Regression of ADBE Log Returns Over Time")
print(ggplotly(plots[[plotCount]]))
summary(msftModel)

# Plot regression of ADBE vs. MSFT
plots[plotCount<-plotCount+1] <- PlotTwoStockRegression("adbe", "msft",
                                                        graphingType="p",
                                                        xtitle = "ADBE Log Return",
                                                        ytitle = "MSFT Log Return",
                                                        graphTitle = "Linear Regression of MSFT Log Return vs. ADBE Log Returns for Nov 2015-Nov 2016")
print(ggplotly(plots[[plotCount]]))

#################################################################
#
# Plotting and calculations for the January predictor wives' tale
#
#################################################################
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
  gspcModel<-lm(gspcYearLog ~ gspcLogs[[k]])
  #plot(gspcLogs[[k]], gspcYearLog, type="p")
  plots[plotCount<-plotCount+1] <- PlotRegression(gspcYearLog,
                                                  gspcLogs[[k]], graphType = "p",
                                                  xtitle = paste("Month ", k, " Log Returns"),
                                                  ytitle = "Annual Log Returns",
                                                  graphTitle = paste("Linear Regression of Annual Returns of S&P 500 Index vs. Month ", k, " Returns"))
  print(ggplotly(plots[[plotCount]]))
  plots[plotCount<-plotCount+1] <- PlotResiduals(gspcModel, gspcLogs[[k]],
                                                 xtitle = paste("Month ", k, " Log Returns"),
                                                 ytitle = "Residuals of Linear Regression of Annual Returns",
                                                 graphTitle = "S&P 500 Index")
  print(ggplotly(plots[[plotCount]]))
  r.squared.list[[k]]<-summary(gspcModel)$r.squared
}

# Find the largest r squared
largest.r.squared.month<-month.abb[which.max(r.squared.list)]
largest.r.squared<-r.squared.list[[which.max(r.squared.list)]]

# Calculate January vs. the rest of the year
gspcRestOfYearLog=LogReturnInterval(gspcList[[2]]$Open, gspcList[[12]]$Close)
gspcModel <- lm(gspcRestOfYearLog ~ gspcLogs[[1]])
#plot(gspcLogs[[1]], gspcRestOfYearLog, type="p")
plots[plotCount<-plotCount+1] <- PlotRegression(gspcRestOfYearLog, gspcLogs[[1]],
                                                graphType="p",
                                                xtitle = "January Log Returns",
                                                ytitle = "Feb-Dec Log Returns",
                                                graphTitle = "Linear Regression of Feb-Dec Log Returns of S&P 500 Index vs. Jan Log Returns")
print(ggplotly(plots[[plotCount]]))
plots[plotCount<-plotCount+1] <- PlotResiduals(gspcModel, gspcLogs[[1]],
                                               xtitle = "January Log Returns",
                                               ytitle = "Residuals of Linear Regression of Rest-of-year Log Returns",
                                               graphTitle = "S&P 500 Index")
print(ggplotly(plots[[plotCount]]))

print(gspcModel)

# Publish plots to plotly
# You need to set your username and API key in your .Rprofile
# See code/PostToPlotly.R for details.
# See my plots at https://plot.ly/~jb4022
#source('code/PostToPlotly.R')
#PostToPlotly(plots)
