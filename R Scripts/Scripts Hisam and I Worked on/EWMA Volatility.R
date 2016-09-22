#EWMA (Exponentially Weighted Moving Average Vol)
library(quantmod)
getSymbols("UNP", from='2010-01-01')
head(x)
x<-UNP$UNP.Adjusted
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

a<-EWMA(x,.94)
a
