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

# Regression utilities in ProjectRegression.R:
#  CalculateVarianceConfidenceInterval(percent)
#  MeanEqualityTest()
source('code/ProjectRegression.R')

# Data utilities in ProjectData.R:
#  ReadStock(symbol)
#  LogReturn(prices)
source('code/ProjectData.R')

# Demonstration of functionality in ProjectDemo.R:
# DoStock(symbol)
source('code/ProjectDemo.R')

# libraries
#   base - for adjusting axis scales
#   dplyr - data manipulation
#   forecast - time series plotting and regression
#   ggplot - plotting
#   lattice - plotting
#   plotly - plotting and uploading plots to the web
#   scales - adjusting axis scales
#   tseries - time series analysis and computational finance

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
DoStock("adbe")
DoStock("msft")

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
