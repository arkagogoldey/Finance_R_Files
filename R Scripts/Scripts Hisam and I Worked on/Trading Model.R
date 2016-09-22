##Calculate Volatility
library(quantmod)
getSymbols("BAC", from='2010-01-01')
head(y)
y<-BAC$BAC.Adjusted
EWMA<-function(x,lambda)
{
	returns<-Delt(x,type="log")
	return_sq<-returns^2
	
	y<-as.matrix(x)
	
	n=(1:nrow(y)-1)
	z<-as.matrix(n)
	
	weights<-(1-lambda)*lambda^z
	weights<-sort(weights,decreasing=FALSE)
	
	product<-weights*return_sq
	product<-as.matrix(product)
	product<-na.omit(product)
	
	Variance<-colSums(product)
	Volatility<-sqrt(Variance)
	
	final<-cbind(Variance,Volatility)
}

a<-EWMA(y,.94)
a

#simulate lognormal random walk
rwalk<-function(mu, sigma,lambda, p)
{
	#daily drift and volatility
	days<-250
	mu.daily <- mu
	sigma.daily<-sigma
	lambda.daily<-lambda/days
	
	r<-rnorm(78,mu.daily,sigma.daily) #100 random normals
	a<-rpois(78,lambda.daily)
	logPrice<-log(p)+cumsum(r)+rnorm(250,0,2)*a
	Prices<- exp(logPrice)
}

f<-replicate(10000,rwalk(.08,.18,1,10))
head(f)
y<-t(f)
plot(density(apply(y,2,mean)))
dist=c(.01,.05,.31,.52,.6,.7,.95,.99)
tile=sapply(1:ncol(y),function(x)quantile(y[,x], dist))
tile

rm(list=ls())