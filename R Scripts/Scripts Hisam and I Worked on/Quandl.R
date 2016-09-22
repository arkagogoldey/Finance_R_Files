install.packages("Quandl")
Quandl.auth("1zm1xSnnoqFeAGksg3S1")
library(ggplot2)
library(Quandl)

eur<-Quandl("CURRFX/EURUSD", start_date="2010-01-01")

stock_AAPL <-Quandl("GOOG/NASDAQ_AAPL", start_date="2010-01-01")

plot(oil, type="l")
lines(stock_AAPL, type="l", col=4)
