S<-read.csv(file.choose(),header=TRUE)
#### Histogram with a Normal Distribution Overlay
hist(S$PHG.Close, freq=FALSE, xlab="Close", col="blue", main="Histogram with Normal Density")
Smin<-min(S$PHG.Close)
Smax<-max(S$PHG.Close)
Smean<-mean(S$PHG.Close)
Ssd<-sd(S$PHG.Close)
X1<-seq(Smin, Smax, by=.01)
lines(X1, dnorm(X1, Smean, Ssd), col="red")
#### Plot a probability distribution
X1<- seq(-4, 4, by=0.1)
plot(X1, dnorm(X1, 0, 1), type="l", lty=1, xlab="x", ylab="Density")
lines(X1, dt(X1, 1), lty=2)
lines(X1, dt(X1, 5), lty=3)
lines(X1, dt(X1, 10), lty=4)
#### add legend
legend(2, 0.4, c("N(0,1)","t df-1", "t df-5", "t df-10"), lty=c(1,2,3,4))
