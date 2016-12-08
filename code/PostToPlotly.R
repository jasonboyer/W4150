# PostToPlotly.R - Code to post plots to plot.ly for 
# public web viewing and interaction
#
# Add the following lines in .Rprofile to enable uploading:
#
# Sys.setenv("plotly_username"="your_plotly_username")
# Sys.setenv("plotly_api_key"="your_api_key")
#
# See https://plot.ly/ggplot2/getting-started/
#

library(plotly)

# PostToPlotly()
#
# arguments
#   plots - list of plots to upload
#   plotNames - corresponding list or titles for the plots 
PostToPlotly <- function(plots, plotNames=NULL) {
  for (i in 1:length(plots)) {
    if (is.null(plotNames)) {
      name <- paste("Figure ", i)
    } else {
      name = plotNames[i]
    }
    plotly_POST(plots[[i]], name)
  }
}