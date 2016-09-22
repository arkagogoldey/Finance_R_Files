#Brownian motion
#Random Walk
	#A stochastic process that starts off with a score of 0
	#At each *discrete* event, there is a probability p chance you will increse your score by +1 and a (1-p) chance that you will decrese your score by 1.
	#The event happens T times.
#What is the expected value of T?
	# 0 + T[p(+1)+(1-p)(-1)]

#Markov Process
	# A particular type of stochastic process where only the present value of a variable is relevant for predicting the future.
	#The history of the variable and the way that the present has emerged from the past are irrelevant

#Martingale Process
	#A stochastic process where at any time=t the expected value of the final value is the current value
	#E[X(t)|X(t)=x]=x
	#EX: A tandom walk with p=.5
		#Note: all martingales are markovian
		#Whats going to happen in the future has no expected gain or loss. 

#Brownian Motion Def:
	#A stochastic Process, {W(t): 0<=T<=inf}, is a standard brownian motion if
		#(1) W(0)=0
		#(2) It has continuous sample paths
		#(3) It has independnt, normall-distributed incremements

#Wiener Process
	#The Wiener process W(t) is characterized by three facts:
		#(1) W(0)=0
		#(2) W(t) is almost surely continuous ( has continuous sample paths)
		#(3) W(t) has independent increments with distribution W(t)- W(s)~N(0,t-s)
		#Note 1: recall that N(mu, sigma^2) denotes normal distribution with expected value mu and variance sigma^2
		#Note 2: the condition of independent increments means that if 0<=S1<=t1<=S2<=t2 then W(t1)~W(s1) and W(t2)~W(S2) are independent random varialbes

#Brownian motion is basically the samething as wiener process


#Convert Random Walk into brownian motion
	#Divide the interval t into n parts each of size t/n
	#Each increment would be R(i)=sqrt(t/n)
	#the total increment over S(i) = sum from i=1 to n of Ri
	#E[Si]=0  E[Ri]=0    E[R^(2)i]=t/n
	#E[S^(2)i]=E[(R1+.....+Ri)(R1+.....+Ri)]
	#When i doesnt = j E[RiRj]=0 because they are uncorrelated
	#E[S^(2)i]=R^(2)1+...+	R^(2)i=i(t/n)
	#E[S^(2)n]=R^(2)1+....R^(2)i=t
	
#Wiener process with Drift
	#dx= a dt + bdW(t)
	#where a and b are constants
	#The dx= adt can be integrated to x = x0 + at
	#where x is the initial value and then if the time period is T, the variable increases by aT
	#bdz accounts for the noise or variability to the path followed by x, the amount of this noise or variability is b times a weiner process. 
	
brownian<-function(n=10,plot=T,fun=rnorm)
{
	x<-cumsum(fun(n))
	y<-cumsum(fun(n))
	plot(x,y,type="l")
}
brownian(fun=rnorm)

#Geometric Brownian Motion
#A stochastic process St is said to follow a GBM if it satisfies the following stochastic differential equation:
	#dSt = uStdt + sigmaStdWt
#Where Wt is a wiener process (brownian motion) and u, sigma re constants

#normally, u is called the percentage drift and sigma is called the percentage volatility. So, consider a brownian motion trajectory that satisfy the differential equation, the right hand term uStdt controls the "trend" of this trajectory and the term sigmaStdWt controls the "random noise" efect in the trajectory.

#Solve the S.D.E.
	#technique of seperation of variables:
		# dSt/St = udt + sigmadWt
	#integrate both sides
		#integral(dSt/St) =integral(udt + sigmadWt)dt
	#Since, dSt/St relates to the derivative of ln(St), so the next step involving the Ito calculus and arriving the following equation:
		#ln(dSt/St) = (u - sigma^(2)/2)t + sigmaWt 
	#take exp
		#St= S(0)exp(u-sigma^(2)/2)t + sigmaWt
		
#GBM
GBM = function(N,sigma,u,S0)
{
Wt = cumsum(rnorm(N,0,1))
t=(1:N)/365
p1=(u−0.5∗(sigma^2))*t 
p2=sigma∗Wt
St=S0∗exp(p1 + p2)
return (St);
}
#E(S(t))= S*exp(u+sigma^2/2)t
library(sde)
plot(GBM(x=17.5, r=.01, sigma=.01, T=1, N=78),col=1)
lines(GBM(x=17.5, r=.01, sigma=.01, T=1, N=78),col=2)
lines(GBM(x=17.5, r=.01, sigma=.01, T=1, N=78),col=3)
lines(GBM(x=17.5, r=.01, sigma=.01, T=1, N=78),col=4)
lines(GBM(x=17.5, r=.01, sigma=.01, T=1, N=78),col=5)
d<-c(1:100)
alpha<-data.frame(GBM(x=17.5, r=.03, sigma=.01, T=1, N=78),GBM(x=17.5, r=.03, sigma=.01, T=1, N=78),GBM(x=17.5, r=.03, sigma=.01, T=1, N=78),GBM(x=17.5, r=.03, sigma=.01, T=1, N=78),GBM(x=17.5, r=.03, sigma=.01, T=1, N=78),GBM(x=17.5, r=.03, sigma=.01, T=1, N=78),GBM(x=17.5, r=.03, sigma=.01, T=1, N=78),GBM(x=17.5, r=.03, sigma=.01, T=1, N=78),GBM(x=17.5, r=.03, sigma=.01, T=1, N=78),GBM(x=17.5, r=.03, sigma=.01, T=1, N=78),GBM(x=17.5, r=.03, sigma=.01, T=1, N=78),GBM(x=17.5, r=.03, sigma=.01, T=1, N=78),GBM(x=17.5, r=.03, sigma=.01, T=1, N=78),GBM(x=17.5, r=.03, sigma=.01, T=1, N=78),GBM(x=17.5, r=.03, sigma=.01, T=1, N=78),GBM(x=17.5, r=.03, sigma=.01, T=1, N=78),GBM(x=17.5, r=.03, sigma=.01, T=1, N=78),GBM(x=17.5, r=.03, sigma=.01, T=1, N=78),GBM(x=17.5, r=.03, sigma=.01, T=1, N=78),GBM(x=17.5, r=.03, sigma=.01, T=1, N=78))
ncol(alpha)
names(alpha)[1]<-"one"
names(alpha)[2]<-"two"
names(alpha)[3]<-"three"
names(alpha)[4]<-"four"
names(alpha)[5]<-"five"
names(alpha)[6]<-"six"
names(alpha)[7]<-"seven"
names(alpha)[8]<-"eight"
names(alpha)[9]<-"nine"
names(alpha)[10]<-"ten"
names(alpha)[11]<-"eleven"
names(alpha)[12]<-"twelve"
names(alpha)[13]<-"thirteen"
names(alpha)[14]<-"fourteen"
names(alpha)[15]<-"fifteen"
names(alpha)[16]<-"sixteen"
names(alpha)[17]<-"seventeen"
names(alpha)[18]<-"eighteen"
names(alpha)[19]<-"nineteen"
names(alpha)[20]<-"twenty"


lines(density(alpha$one))
lines(density(alpha$two),col=2)
lines(density(alpha$three),col=3)

