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

# MeanEqualityTest - prints t test for two stocks to the console
#   arguments:
#     strSymbolA - first stock symbol
#     strSymbolB - second stock symbol
MeanEqualityTest<-function(strSymbolA, strSymbolB) {
  stockA <- ReadStock(strSymbolA)
  stockB <- ReadStock(strSymbolB)
  logStockA <- LogReturn(stockA$Close)
  logStockB <- LogReturn(stockB$Close)
  print(t.test(logStockA, logStockB))
}
