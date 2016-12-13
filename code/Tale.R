#################################################################
#
# Tale.R - Plotting and calculations for the January predictor wives' tale
#
#################################################################
DoTale <- function() {
  plotCount<-0
  plots<-list(50)

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
}