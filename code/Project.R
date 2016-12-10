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

# Calculate and plot the data for the January Effect wives' tale
source('code/Tale.R')

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
p <- PlotTwoStockRegression("adbe", "msft",
                            graphingType="p",
                            xtitle = "ADBE Log Return",
                            ytitle = "MSFT Log Return",
                            graphTitle = "Linear Regression of MSFT Log Return vs. ADBE Log Returns for Nov 2015-Nov 2016")
PlotGGAndPlotly(p[[1]])

# Calculate and plot January Effect wives' tale data
DoTale()