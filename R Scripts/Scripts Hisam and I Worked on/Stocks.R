# Take each of these Vectors and create a Data Frame From Them

require("quantmod")
sy<-c("AAPL","YHOO","IBM")
s=sy
s
getSymbols(Symbols = "IBM", 
           env = parent.frame(),
           reload.Symbols = FALSE,
           verbose = FALSE,
           warnings = TRUE,
           src = "yahoo",
           symbol.lookup = TRUE,
           auto.assign = getOption('getSymbols.auto.assign',TRUE))
####AAPL
mean(AAPL$AAPL.Close)
sd(AAPL$AAPL.Close)
var(AAPL$AAPL.Close)
chartSeries(AAPL)
DeltaA = dailyReturn(AAPL)
mean(Delta)
chartSeries(DeltaA)
####IBM
mean(IBM$IBM.Close)
sd(IBM$IBM.Close)
var(IBM$IBM.Close)
chartSeries(IBM)
DeltaI = dailyReturn(IBM)
mean(DeltaI)
chartSeries(DeltaI)
sd(DeltaI)
z=mean(DeltaI)+sd(DeltaI)
y=mean(DeltaI)-sd(DeltaI)
col
#####SPY
getSymbols(Symbols = "SPY", 
           env = parent.frame(),
           reload.Symbols = FALSE,
           verbose = FALSE,
           warnings = TRUE,
           src = "yahoo",
           symbol.lookup = TRUE,
           auto.assign = getOption('getSymbols.auto.assign',TRUE))
DeltaS=dailyReturn(SPY)
