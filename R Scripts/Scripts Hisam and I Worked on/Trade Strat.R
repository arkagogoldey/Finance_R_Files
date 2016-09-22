library(quantmod)
getSymbols("BAC",from='2014-01-06')
bac<-BAC$BAC.Adj
ret<-Delt(bac,type="log")
ret<-as.numeric(ret)
ret<-na.omit(ret)
m_ret<-mean(ret)
m_ret ##Mean daily return
std.dev<-sd(ret) ##Daily Volatility
bacn<-as.numeric(bac)
######simulate lognormal random walk#######
rwalk<-function(mu, sigma, p)
{
	#daily drift and volatility
	days<- 300
	mu.daily <- mu
	sigma.daily<-sigma
	
	r<-rnorm(days,mu.daily,sigma.daily) #100 random normals
	logPrice<-log(p)+cumsum(r)
	
	Prices<- exp(logPrice)
}
####test once#####
f<-rwalk(m_ret,std.dev,bacn[1,])
plot(f, type='l')
###simulation#####
simnumber<-10000
f<-replicate(simnumber,rwalk(m_ret,std.dev,bac[1,]))
ncol(f)
y<-t(f)
z<-apply(y,2,mode)
dist=c(.01,.05,.3,.5,.7,.95,.99)
tile=sapply(1:ncol(y),function(x)quantile(y[,x], dist))
blab<-t(tile)
head(blab)

###Probability Plots#####
plot(bacn, xlab="Days",ylab="Price",xlim=c(0,300),ylim=c(14,19),type='l')
for (i in 1:8){
	lines(blab[,i],col=i+1)
}
#######Correlations#####
ff<-as.data.frame(f)
bacc<-as.data.frame(bac)
colnames(bacc)<-c("BAC")
head(bacc)
head(ff)
zeta<-cbind(bacc,ff[0:253,])
head(zeta)
tail(zeta)
class(zeta)
zeta<-na.omit(zeta)
a<-cor(zeta)
head(a)
jaber<-a[1,]
jaber<-as.data.frame(jaber)
head(jaber)
class(jaber)
jaba<-jaber[order(-jaber$jaber),,drop=FALSE]
head(jaba,10)
tail(jaba)

######test the top ten#####
plot(bacn, xlab="Days",ylab="Price",xlim=c(0,255),ylim=c(14,19),type='l')
lines(f[,1014],col=2)
hisam<-f[253:260,1014]
sam<-as.data.frame(hisam)
sam


####Covariance#####
csgo<-cov(zeta)
head(csgo)
csgo<-as.data.frame(csgo)
csgo1<-csgo[1,]
head(csgo1)
csd<-t(csgo1)
class(csd)
csd<-as.data.frame(csd)
head(csd)
csd<-csd[order(-csd$BAC),,drop=FALSE]
head(csd)
plot(bacn,type='l',xlab="Days",ylab="Price",main="BAC",xlim=c(0,300),ylim=c(14,19),col=1)
lines(ghgh,type="l",col=2)
ghgh<-f[,46]-rep(3,300)

##Correlation is better suited..