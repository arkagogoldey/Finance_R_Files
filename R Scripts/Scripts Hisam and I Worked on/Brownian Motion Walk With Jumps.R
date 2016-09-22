#simulate lognormal random walk
rwalk<-function(mu, sigma,lambda, p)
{
	#daily drift and volatility
	days<- 250
	mu.daily <- mu/days
	sigma.daily<-sigma/sqrt(days)
	lambda.daily<-lambda/days
	
	r<-rnorm(250,mu.daily,sigma.daily) #100 random normals
	a<-rpois(250,lambda.daily)
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