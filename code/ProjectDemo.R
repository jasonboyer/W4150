# ProjectDemo.R - demonstrate functionality of project for one stock symbol

# DoStock - demonstrate the functionality of the project for one stock symbol
DoStock <- function(strSymbol) {
  plotCount<-0
  plots<-list(50)
  
  # Calculations for the symbol
  adbe<-ReadStock(strSymbol)
  adbeLog<-LogReturn(adbe$Close)
  
  # Plot linear regression of returns
  plots[plotCount<-plotCount+1]<-PlotPrice(adbe, strSymbol)
  PlotGGAndPlotly(plots[[plotCount]])
  
  # Plot residuals of linear regression
  adbeMod <- lm(adbe$Close ~ adbe$Val)
  plots[plotCount<-plotCount+1] <- PlotResiduals(adbeMod, adbe$Val)
  PlotGGAndPlotly(plots[[plotCount]])
  
  plots[plotCount<-plotCount+1]<-PlotHistogram(adbeLog, strSymbol, graphTitle = paste(strSymbol, " Nov 2015- Nov 2016"))
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
  message(sprintf("90%% confidence interval for variance in %s log returns: %s", 
                  strSymbol, toString(varCI)))
  varCI<-CalculateVarianceConfidenceInterval(adbeLog, 95)
  message(sprintf("95%% confidence interval for variance in %s log returns: %s",
                  strSymbol, toString(varCI)))
  
  # Calculate, plot, and summarize the linear regression model
  adbeModel <- lm(adbeLog ~ adbe$Val[2:length(adbe$Val)])
  plots[plotCount<-plotCount+1] <- PlotRegression(adbeLog, adbe$Val[2:length(adbe$Val)], linearModel=adbeModel,
                                                  graphType="l", ytitle = paste(strSymbol, " Log Return"), xtitle = "Time",
                                                  graphTitle = paste(strSymbol, " Log Return Over Time"))
  PlotGGAndPlotly(plots[[plotCount]])
  
  
  # Plot the residuals of the linear regression model
  plots[plotCount<-plotCount+1] <- PlotResiduals(adbeModel, adbe$Val[2:length(adbe$Val)],
                                                 xtitle = "Time",
                                                 graphTitle = paste("Residuals of Linear Regression of ", 
                                                                    strSymbol, " Log Returns Over Time"))
  
  PlotGGAndPlotly(plots[[plotCount]])
  
  print(summary(adbeModel))

  # Publish plots to plotly
  # You need to set your username and API key in your .Rprofile
  # See code/PostToPlotly.R for details.
  # See my plots at https://plot.ly/~jb4022
  # Uncomment below to publish to your account
  # source('code/PostToPlotly.R')
  # PostToPlotly(plots)
}
