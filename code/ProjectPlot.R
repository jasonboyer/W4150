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
    gTitle <- paste("Closing Prices for ", strSymbol)
  } else {
    gTitle <- graphTitle
  }
  lMod <- lm(dfStock$Close ~ dfStock$Val)
  intercept <- coefficients(lMod)[1]
  slope <- coefficients(lMod)[2]
  info <- paste("Regression:",
                "\nIntercept:  ", intercept,
                "\nSlope: ", slope)  
  ret<-ggplot(dfStock, 
              aes(x = Date, y = Close)) +
    geom_line() +
    geom_smooth(method = "lm") +
    labs(title = gTitle, x = "Date", 
         y = paste(strSymbol, "Closing Price")) +
    annotate("label", x = max(dfStock$Date) - (max(dfStock$Date) - min(dfStock$Date)) * .2,
             y = max(dfStock$Close) - (max(dfStock$Close) - min(dfStock$Close)) * .85, 
             label = info)
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
PlotResiduals <- function(lMod, indep, xtitle = NULL, ytitle = NULL,
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
  res <- resid(lMod)
  info <- paste("R-squared: ", summary(lMod)$r.squared)
  ret <- qplot(indep, res,
               xlab = xtitle, ylab = ytitle,
               main = graphTitle) + geom_point() + 
    geom_hline(yintercept=0, color = I("blue")) +
    annotate("label", x = max(indep) - (max(indep) - min(indep)) * .2,
             y = max(res) - (max(res) - min(res)) * .85, 
             label = info)
  list(ret)
}

# PlotGGAndPlotly - displays the ggplot, waits for input, then displays the plotly version
#
# arguments
#   p - ggplot object
# return value
#   none
PlotGGAndPlotly <- function(p) {
  # Plotly does not support ggplot geom_label, so display the ggplot
  print(p)
  print("Press <ENTER> to continue ...")
  print(ggplotly(p))
}
