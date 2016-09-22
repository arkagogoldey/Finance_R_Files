#simulate lognormal random walk
rwalk<-function(mu, sigma,lambda, p, days)
{
	#daily drift and volatility
	mu.daily <- mu/days
	sigma.daily<-sigma/sqrt(days)
	lambda.daily<-lambda/days
	
	r<-rnorm(days,mu.daily,sigma.daily) #100 random normals
	a<-rpois(days,lambda.daily)
	b<-dpois(a,lambda.daily)
	c<-rnorm(days,0,.25)
	d<-b*cumsum(c)
	logPrice<-log(p)+cumsum(r)+.03*d
	Prices<- exp(logPrice)
}

f<-replicate(10000,rwalk(ret,vol,1,16.54,300))
head(f)
y<-t(f)
z<-apply(y,2,mode)
dist=c(.01,.05,.31,.52,.6,.7,.95,.99)
tile=sapply(1:ncol(y),function(x)quantile(y[,x], dist))
blab<-t(tile)
head(blab)
plot(tile)
plot(f[,1],type="l")
rm(list=ls())


############
library(quantmod)
getSymbols("BAC",from='2014-01-06')
x<-BAC$BAC.Adj
x<-as.numeric(x)
alpha<-Delt(x,type="log")
alpha<-as.numeric(alpha)
alpha<-na.omit(alpha)
ret<-mean(alpha)
alpha<-alpha^2
var(alpha)
vol<-sd(alpha)
sqrt(var(alpha))


zeta<-cbind(x,f[0:253,0:10000])
class(zeta)
zeta<-as.data.frame(zeta)
head(a)
a<-cor(zeta)
jaber<-a[1,]
jaber<-as.data.frame(jaber)
head(jaber)
class(jaber)
jaba<-jaber[order(-jaber$V1),,drop=FALSE]
head(jaba,10)

csgo<-cov(zeta)
head(csgo)
csgo<-as.data.frame(csgo)
csgo1<-csgo[1,]
head(csgo1)
csd<-t(csgo1)
class(csd)
csd<-as.data.frame(csd)
csd<-csd[order(-csd$x),,drop=FALSE]
head(csd)
plot(x,type='l',xlab="Days",ylab="Price",main="BAC",xlim=c(0,300),ylim=c(14,19),col=1)
lines(f[,5851],type="l",col=2)
