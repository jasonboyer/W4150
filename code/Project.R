# SEIO W4150 Term Project
# Jason Boyer (jb4022)
# 2016-12-09
#
# See the accompanying PDF report for details on the project.

# Plot functions in ProjectPlot.R:
#  PlotPrice()
#  PlotResiduals()
#  PlotHistogram()
#  PlotVsNormal()
#  PlotRegression()
#  PlotMeanConfidenceInterval(percent)
#  PlotLogReturnRegression()
#  PlotTwoStocks(symbol1, symbol2)
#  PlotTwoStockRegression()
#  PlotGGAndPlotly()
#  PostToPlotly()
source('code/ProjectPlot.R')

# Regression utilities:
#  CalculateVarianceConfidenceInterval(percent)
#  MeanEqualityTest()
#
# Data utilities:
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
PlotGGAndPlotly(plots[[plotCount]])

# Plot residuals of linear regression
adbeMod <- lm(adbe$Close ~ adbe$Val)
plots[plotCount<-plotCount+1] <- PlotResiduals(adbeMod, adbe$Val)
PlotGGAndPlotly(plots[[plotCount]])

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
PlotGGAndPlotly(plots[[plotCount]])


# Plot the residuals of the linear regression model
plots[plotCount<-plotCount+1] <- PlotResiduals(adbeModel, adbe$Val[2:length(adbe$Val)],
                                               xtitle = "Time",
                                               graphTitle = "Residuals of Linear Regression of ADBE Log Returns Over Time")

PlotGGAndPlotly(plots[[plotCount]])

print(summary(adbeModel))

# Calulations for symbol MSFT
msft<-ReadStock("msft")
msftLog<-LogReturn(msft$Close)

plots[plotCount<-plotCount+1] <- PlotPrice(msft, "MSFT", "Daily closing price for Microsoft")
PlotGGAndPlotly(plots[[plotCount]])

# Plot residuals of linear regression
msftMod <- lm(msft$Close ~ msft$Val)
plots[plotCount<-plotCount+1] <- PlotResiduals(msftMod, msft$Val)
PlotGGAndPlotly(plots[[plotCount]])

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
PlotGGAndPlotly(plots[[plotCount]])

plots[plotCount<-plotCount+1] <- PlotResiduals(msftModel, msft$Val[2:length(msft$Val)],
                                               xtitle = "Time",
                                               graphTitle = "Residuals of Linear Regression of ADBE Log Returns Over Time")
PlotGGAndPlotly(plots[[plotCount]])

print(summary(msftModel))

# Plot regression of ADBE vs. MSFT
plots[plotCount<-plotCount+1] <- PlotTwoStockRegression("adbe", "msft",
                                                        graphingType="p",
                                                        xtitle = "ADBE Log Return",
                                                        ytitle = "MSFT Log Return",
                                                        graphTitle = "Linear Regression of MSFT Log Return vs. ADBE Log Returns for Nov 2015-Nov 2016")
PlotGGAndPlotly(plots[[plotCount]])

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
  PlotGGAndPlotly(plots[[plotCount]])
  
  plots[plotCount<-plotCount+1] <- PlotResiduals(gspcModel, gspcLogs[[k]],
                                                 xtitle = paste("Month ", k, " Log Returns"),
                                                 ytitle = "Residuals of Linear Regression of Annual Returns",
                                                 graphTitle = "S&P 500 Index")
  PlotGGAndPlotly(plots[[plotCount]])
  
  r.squared.list[[k]]<-summary(gspcModel)$r.squared
}

# Find the largest r squared
largest.r.squared.month<-month.abb[which.max(r.squared.list)]
largest.r.squared<-r.squared.list[[which.max(r.squared.list)]]

# Calculate January vs. the rest of the year
gspcRestOfYearLog=LogReturnInterval(gspcList[[2]]$Open, gspcList[[12]]$Close)
gspcModel <- lm(gspcRestOfYearLog ~ gspcLogs[[1]])

plots[plotCount<-plotCount+1] <- PlotRegression(gspcRestOfYearLog, gspcLogs[[1]],
                                                graphType="p",
                                                xtitle = "January Log Returns",
                                                ytitle = "Feb-Dec Log Returns",
                                                graphTitle = "Linear Regression of Feb-Dec Log Returns of S&P 500 Index vs. Jan Log Returns")
PlotGGAndPlotly(plots[[plotCount]])

plots[plotCount<-plotCount+1] <- PlotResiduals(gspcModel, gspcLogs[[1]],
                                               xtitle = "January Log Returns",
                                               ytitle = "Residuals of Linear Regression of Rest-of-year Log Returns",
                                               graphTitle = "S&P 500 Index")
PlotGGAndPlotly(plots[[plotCount]])

print(gspcModel)

# Publish plots to plotly
# You need to set your username and API key in your .Rprofile
# See code/PostToPlotly.R for details.
# See my plots at https://plot.ly/~jb4022
# Uncomment below to publish to your account
# source('code/PostToPlotly.R')
# PostToPlotly(plots)
