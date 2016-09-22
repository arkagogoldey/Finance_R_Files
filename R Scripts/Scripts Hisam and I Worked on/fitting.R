install.packages("fitdistrplus")
install.packages("MASS")
install.packages("poweRlaw")
library(fitdistrplus)
library(stockPortfolio)
library(poweRlaw)
#http://cran.r-project.org/web/packages/poweRlaw/poweRlaw.pdf
returns<-getReturns("SPY",freq="week", start="2000-01-01",end="2014-12-01")
summary(returns)
head(returns)
returns$full$SPY$Adj.Close
set.seed(1)
dat<-rnorm(50,0,1)
f1<-fitdist(returns$full$SPY$Adj.Close,"norm")
f2<-fitdist(returns$full$SPY$Adj.Close,"logis")
f3<-fitdist(returns$full$SPY$Adj.Close,"cauchy")
f4<-fitdist(returns$full$SPY$Adj.Close,"exp")
f5<-fitdist(returns$full$SPY$Adj.Close,"unif")
f6<-estimate_pars(returns$full$SPY$Adj.Close,parse=True)
f1
f2
f3
f4
f5
#f1 plot
plotdist(returns$full$SPY$Adj.Close,"norm",para=list(mean=f1$estimate[1],sd=f1$estimate[2]))
#f2 plot
plotdist(returns$full$SPY$Adj.Close,"logis",para=list(location=f2$estimate[1],scale=f2$estimate[2]))
#f3 plot
plotdist(returns$full$SPY$Adj.Close,"cauchy",para=list(location=f3$estimate[1],scale=f3$estimate[2]))
#Using Normal dist info (F1)
	#Pr(C>210)
	1-pnorm(205,193.56,9.36)
	1-plogis(205,108.87,16.16)
	
library(moments)
kurtosis(returns$full$SPY$Adj.Close)-3
skewness(returns$full$SPY$Adj.Close)