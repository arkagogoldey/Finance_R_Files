install.packages("moments")
library(stockPortfolio)
returns<-getReturns("SPY",freq="week",start="2011-01-01")
returns$full$SPY$Adj.Close
library(moments)
#Pearsons 
excess=kurtosis(returns$full$SPY$Adj.Close)-3
dat<-rnorm(5000,0,1)
kurtosis(dat)
excess
skewness(returns$full$SPY$Adj.Close)
#Now fit that kurtosis matches up
library(fitdistrplus)
f1<-fitdist(returns$full$SPY$Adj.Close,"norm")
f1
#function to minimizie excess curtosis and minimize skewness to zero by changing dates. 
plotdist(returns$full$SPY$Adj.Close,"norm",para=list(mean=f1$estimate[1],sd=f1$estimate[2]))
# a match of skewness and kurtosis does not necessarily make a good fit. CDF and Quantile are way off..