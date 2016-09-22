#simulate lognormal random walk

?sapply
rwalk<-function(mu, sigma,lambda, p)
{
	#daily drift and volatility
	days<- 250
	mu.daily <- mu/days
	sigma.daily<-sigma/sqrt(days)
	lambda.daily<-lambda/days
	
	r<-rnorm(250,mu.daily,sigma.daily) #100 random normals
	a<-rpois(250,lambda.daily)
	logPrice<-log(p)+cumsum(r)+cumsum(a)
	
	Prices<- exp(logPrice)
}
str(sapply)
f<-rwalk(.1,.12,1,10)
plot(f, type='l')


