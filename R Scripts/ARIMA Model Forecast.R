#ARIMA Model Forecasts
#http://www.r-bloggers.com/forecasts-with-arima-models/
#http://freakonometrics.hypotheses.org/48264

n = 95
set.seed(1)
E = rnorm(n)
X = rep(0,n)
phi=.85
for(t in 2:n) X[t]=phi*X[t-1]+E[t]
plot(X,type = "l")
model=arima(X,order=c(1,0,0), include.mean = FALSE)

P=predict(model,n.ahead=20)
plot(P$pred)
lines(P$pred+2*P$se,col="red")
lines(P$pred-2*P$se,col="red")
abline(h=0,lty=2)
abline(h=2*P$se[20],lty=2,col="red")
abline(h=-2*P$se[20],lty=2,col="red")autoplot(y)
