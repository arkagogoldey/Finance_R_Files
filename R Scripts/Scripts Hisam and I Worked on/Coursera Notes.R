#FutureValue: FV=V(1+R)^n for compounding interest once per year
	#V= initial investment
	#R= interest Rate
	#N= number of periods
#Present Value: V = FV/(1+R)^n
#Compound Annual return R = (FV/V)^(1/n) -1
#Investment Horizon N= ln(FV/V)/ln(1+R)
##How long does it take for your money to double?
	# 2V=V(1+R)^n
	#ln(2)=nln(1+R)
	#ln(2)/ln(1+R)=n
	#ln(2)~.7
	#ln(1+R)~R
	#Rule of 70 -> if R =0.01, takes 70years for money to double
##Compounding m times per year
#fv=v(1+r/m)^(nm) 
#continuous compounding
#FV= lim m->infinity (v(1+r/m)^nm) = $Ve^(Rn)

#effective annual rate
# V(1+R/m)^(mn) = v(1+R(a))^n
#Solve for R(a)= (1+R/m)^m -1
#for continuous compounding R(a)=e^R - 1
#Credit card companies compound interest daily

## Asset return calculations
#Pt= price at the end of month t on an asset that pays no dividends
#Pt-1 = price at the end of month t-1
# Rt = return on asset = (Pt - Pt-1)/Pt-1
# now lets look at multiple periods
#Rt(2) = 2 month return= (Pt - Pt-2)/Pt-2
# or (Pt)/Pt-1 * Pt-1/Pt-2 -1 = (1+Rt)*(1+Rt-1) -1 
#for K months Rt(k)=(Pt- Pt-k)/Pt-k
# 1+Rt(k)=Pt/Pt-k
# 1+Rt(k)= (1+Rt)(1+Rt-1)(1+Rt-2)....(1+Rt-k+1) #called a geometric average
# =Product of j=0 to k-1 (1+Rt-j)

##Portfolio Returns
#collection of assets you're going to invest in
#invest $V in two assets A and B for 1 period
#xA = share of $V invested in A. $V *xA = $amount
#xB = share of $V invested in B. $V *xB = $amount
#Assume xA + xB = 1
#Rate of return on portfolio?
	#$V(1+Rp,t) = $V{xA(1+RA,t) + xB(1+RB,t)}
				#= $V{xA + xB + xARA,t+ xBRB,t}
				#= $V{1+ xARA,t+ xBRB,t }
				#Rp,t= xARA,t+ xBRB,t 
	# the simple portfolio return is a share weighted average of the simple returns of individual shares
# for n assets
	# 1 + Rp,t = sum from i=1 to n of xi(1+Ri,t)
	#Rp,t = sum from i=1 to n of xiRi,t

#lets now adjust for dividends
#Dt = dividend payment between months t-1 and t
#Rt(total)= (Pt+Dt-Pt-1)/Pt-1 
#1+Rt(total)= (Pt+Dt)/Pt-1

##Everything up to now has been in nominal dollars
#now adjust for inflation -> real rate of return
#Pt(Real) = Pt/CPIt
#Rt(Real) =(Pt(Real)-Pt-1(Real))/Pt-1(Real)
#Rt(Real)= Pt/Pt-1 * CPIt-1/CPIt -1
#Alternatively, define inflation as
	#Pit = (CPIt-CPIt-1)/CPIt-1
	#Then,
	#Rt(Real)= (1+Rt)/(1+PIt) -1

##Annualizing Returns
#simple

##continuously comopounded (cc) Returns
#rt=ln(1+Rt)= ln(Pt/Pt-1) (given Rt we can solve for rt)
#Rt= e^rt -1 (given rt we can solve for Rt)
#rt < Rt because you are continuously compounding throughout the period, so the rate doesnt need to be as high. 
#e^rt = e^ln(1+Rt) = e^ln(Pt/Pt-1)
#=Pt/Pt-1 = e^rt
#Pt-1 e^rt = Pt
#if Rt is small then rt = ln(1+Rt) ~Rt First order Taylor series approximation
#ln(Pt)-ln(Pt-1)=rt
##multiple periods
#cc returns are additive
#rt(2)= ln(Pt/Pt-1)+ln(Pt-1/Pt-2)
#in general
	#rt(k)= ln(1+Rt(k))=ln(Pt/Pt-k)
	# = sum from j=0 to k-1 of rt-j
	# = rt + rt-1 + ...+rt-k+1

##back to portfolio returns
# Rp,t = Sum from i=1 to n of xiRi,t
# rp,t = ln(1+Rp,t)= ln(1+Sum from i=1 to n of xiRi,t ) which doesnt equal Sum from i=1 to n of xiRi,t
		#Portfolio returns are not additive!
# if Rp,t = Sum from i=1 to n of xiRi,t is not to large, then rp,t ~ Rp,t otherwise, Rp,t >rp,t

##adjusting for inflation with cc
#rt(Real)= ln(1+Rt(Real))
#1+Rt(Real)= Pt/Pt-1 * CPIt-1/CPIt 
#follows that
	#rt(Real)=ln(Pt/Pt-1 * CPIt-1/CPIt )
	#rt(Real)= ln(Pt)-ln(Pt-1)+ln(CPIt-1)-ln(CPIt)
	#rt-pit(cc)
	#rt= ln(Pt)-ln(Pt-1)
	#pit(cc)= ln(CPIt-1)-ln(CPIt)
	
	#jump to excel file..
	
##univariate random variable: A random variable (rv) X is a variable that can take on a given set of values, called the sample space Sx, where the likelihood of the values in Sx is dertermined by the variable probability distribution function(pdf)
#example of a discrete distribution plot
#5 states of the economy and there returns
r.msft = c(-.3,0,.1,.2,.5)
prob.vals=c(.05,.2,.5,.2,.05)
barplot(prob.vals, names.arg= as.character(r.msft), xlab="return")
title("Annual Return on Microsoft")

#Bernouli Dist
	#Consider two mutually exclusinve events geneeriaclly "success" and "failure"
	#Let X=1 if success occurs and X=0 if failure occurs
	#Let Pr(X=1)=pi, where 0<pi<1, denote the probability of succcess. Then Pr(X=0)= 1-pi is the probability of failure. A mathematical model descibing this distribution:
	#p(x)=Pr(X=x)=(pi^(x))((1-pi)^(1-x)), x=0,1

#Continuous Random Variable
	#a continuous rv X is one that can take on any real value
	#the pdf of a continuous rv X is a nonnegative function f(x) such that for any interval A on the real line:
		#Pr(X in A)=integralA f(x)dx
	#Pr(X in A)= Area under probability curve over the interval A
#touches on uniform distribution

##Cumulative Distribution Function (CDF)
	#The CDF, F, of a rv X is F(x)=Pr(X<= x) 
	# for microsoft
r.msft = c(-.3,0,.1,.2,.5)
prob.vals=c(.05,.2,.5,.2,.05)
cdf.msft=c(.05,.25,.75,.95,1)
plot(cdf.msft,r.msft, type="l")
#this is not a step function need to learn how to plot those..

##Quantiles: they tell us about the probabilities of loss in finance

##Standard Normal Distribution: X~N(0,1). center = 0, stanard dev = 1. 
	#f(x)=phi(x)=(1/sqrt(2pi))exp(-(x^2)/2), -inf to +inf
	#phi(x)= Pr(x<=x)=integral from -inf to x of phi(z)dz
#Excel Functions:
	#NORMSDIST computes CDF For SND Pr(X<=z)=phi(z) or p(z), also gives density
	#NORMSINV computes the quantile
##R Functions
	#pnorm computes Pr(x<=z)=phi(z)
	#qnorm computes quantiles
	#dnorm computes the density 
#Examples:
	#N(0,1): 
		#Pr(X<=z)=1-Pr(X>=z)
		#Pr(X>=z)= Pr(X<=-z)
		#Pr(X>=0)=Pr(X<=0)=.5
#in R PR(-1<=X<=2)=Pr(X<=2)-Pr(X<=-1)
pnorm(2)-pnorm(-1)
#[1] 0.8185946
#Quantiles for 1%, 2.5%, 5%
qnorm(.01)
#[1] -2.326348
qnorm(.025)
#[1] -1.959964
qnorm(.05)
#[1] -1.644854
##Plotting Standard Normal Distribution
x.vals=seq(-4,4,length=150)
plot(x.vals,dnorm(x.vals), type="l", lwd=2, col="blue", xlab="x", ylab="pdf")

##Expected Value and Standard Devs
#shape characteristics of pdfs
	#Expected Value or Mean- Center of Mass
		#Discrete Case:
			#Sum of all probabilities times values
				#probability weighted average of all the elements in the sample space
		#Continuous Case:
			#Integral from -inf to +inf of xf(x)dx
	#Variance and Standard Deviations- Spread about the mean
		#V(X)=(X-E(X))^2 = E(X^2)-(E(X))^2
		#SD(X)=Sqrt(V(X))
			#For "bell-shaped" data, SD(x) measures the size of the typical deviation from the mean value. Only use for symmetric distribution!
	#Skewness- symmetry about the mean
	#Kurtosis- tail thickness

##General Normal Distribution:
# X~N(uX,Sigma(X)^2)
#f(x)=(1/sqrt(2pisigma^2))*exp((-1/2)((x-u)/sigma)^2, -inf<=x<=+inf
	#centered at uX
	#Symmetric
	#quantiles of the general normal distribution:
		#qa=uX+sigma(x)*phi^-1(a)=ux+sigma(x)*za
#Excel:
	#NORMDIST(x,Ux,sigma(x),cumulative)
	#NORMINV for quantiles
#R
	#Simulate data: rnorm(n,mean,sd)
	#Compute CDF: pnorm(q, mean, sd)
	#Compue Quantiles: qnorm(q, mean, sd)
	#Compute Density: dnorm(x, mean, sd)

##Standard Deviation as a Measure of Risk
# Example
	#RA = monthly return on asset A
	#RA = monthly return on asset B
	#RA~ N(UA, sigma^2(A)), RB~ N(UB, sigma^2(B))
	#UA= E[RA]=expected monthly return on asset A
	#Sigma(A)=SD(RA)= std. dev of monthly reutn on asset A
	#Typically, if 
		#UA>UB
	#Then
		#SD(A)>SD(B)
#Pick up at normal dist: appropriate for simple returns?
#Rt=(Pt-Pt-1)/Pt-1, for one year
# Assume Rt~N(.05,(.5)^2). Mean of return =.05. Std Dev.=.5
	#defined for returns between -inf and +inf
		#however, smallest return can only be -100%
	#Pt>=0 -> Rt>=-1. However, based on the assumed normal distriubution:
		#P(Rt<-1)= pnorm(-1, .05, .5)
		pnorm(-1, .05, .5)
		#[1] 0.01786442
		#This implies that there is a 1.8% chance that the asset price will be negative.
		##This is why the normal distribution may not be appropriate for simple returns. 
##The normal distribution is more appropriate for cc returns
#rt=ln(1+Rt)=cc return
#Rt=e^(rt)-1= simple return
#Assume rt~N(.05,.5^2)
#Heres an example of why it is more suitable:
	#Unlike Rt, rt can take on values less than -1. For example,
		#rt=-2 -> Rt=e^(-2)-1=-.865
		#Pr(rt<=2)=Pr(Rt<-.865)=
		pnorm(-2,0.05,0.5)
		#[1] 2.065751e-05
		#at rt=-inf -> Rt will be -1
	#Hence, we will often be doing probability modeling with cc instead of simple return

##The log-normal distribution
#X~N(Ux,Sigma^2(x)), -inf<x<inf
#Y=exp(x)~lognormal(Ux,Sigma^2(x)), o<Y<inf
#E[Y]=Uy=exp(Ux+(Sigma^2(x))/2)
#Var(Y)=sigma^2(Y)=exp(2Ux+sigma^2(x))(exp(sigma^2(x)-1))
	#Example
	# rt~N(0.05,(.5)^2)
	#1+Rt~lognormal(.05,(.5)^2)
	#U(1+Rt)=exp(0.05+(.5)^2))/2)= 1.191
	#U(Rt)=.191
	#Sigma^2(1+R)=exp(2(.05)+(.5)^2)(exp(.5^2)-1)=.563
////
###Lognormal distribution in R
	#Simulate Data: rlnorm(n, mean, sd)
	#Compute CDF: plnorm(q, mean, sd)
	#Compute Quantiles: qlnorm(p, mean, sd)
	#Compute Density: dlnorm(y, mean, sd)
////

##Skewness and Kurtosis
#Skweness: Measure of symmetry
#g(X)=((X-UX)/sigma(X))^(3)
#Skew(X)=E(((X-UX)/sigma(X))^(3))
	# = integral from -inf to inf (((X-UX)/sigma(X))^(3))f(x)dx if if is continuous
	#Summation of (((X-UX)/sigma(X))^(3))p(x) if X is discrete
#if skew(X)>0 most of data is to the left of the mean but big numbers are to the right of the mean (long right tail)
#if skew(X)<0 most of data is to the right of the mean but big numbers are to the left of the mean (long left tail)
#in symmetric dist, Skew(X)=0
	#X~N(UX,Sima^2(x)) then
		#Skew(X)=0
	#Y~lognormal(UX,Sigma^2(X)) then
		#Skew>0
#Kurtosis
#g(X)=((X-UX)/sigma(X))^(4)
#Kurtosis(X)=E(((X-UX)/sigma(X))^(4))
	# = integral from -inf to inf (((X-UX)/sigma(X))^(4))f(x)dx if if is continuous
	#Summation of (((X-UX)/sigma(X))^(4))p(x) if X is discrete
#Values of x far from UX get blown up resulting in large values of kurtosis
#Two extreme cases: fat tails(large kurtosis); thin tails(small kurtosis)
#You need a benchmark for comparison; normally the normal dist is used
	###the kurtosis for a nomral distribution is 3
	#if kurtosis is >3 there are more extreme values than the noraml dist
	#If kurtosis is <3 there are less extreme values than the normal dist
#noramlly we look at excess kurtosis i.e. kurtosis(X)-3

##Students-t Distribution
# A distribution similar to the standard normal dist but with fatter tailes, and hence larger kurtosis, is the students-t distribution. If X has a students t distribution with degrees of freedom paramter v(controles the thinkness of the tails), denoted X~tv, then its pdf has the form:
# f(x)=(gamma((v+1)/2)/(sqrt(vpi)*gamma(v/2)))(1+(x^2)/v)^(-(v+1/2)), -inf<x<inf, v>0
#where gamma(z)=integral from 0 to inf of t^(z-1)e^(-t)dt denots the gamma function.
#smaller the df the fatter the tails,
#if df = inf we get the normal distribution
#in this function df can be any number i.e. 19.89
#it can be shown that
	#E(X)= 0 , v>1
	#Var(X)= v/(v-2), v>2
	#Skew(X)= 0, v>3
	#Excess Kurtosis(X)= 6/(v-4)-3, v>4
	## The parameter v controls the scale and tail thickness of distribution. If v is close to four, then the kurtosis is large and the tails are thick. if v<4, then kurt(X)=inf. As v->inf the students t pdf approaches that of a standard normal random variable

#////Students t in R
#Simulate Data: rt(n,df,ncp)
#Compute CDF pt(q, df, ncp, lower.tail=T, log.p=F)
#Compute quantiles qt(p,df,ncp,lower.tail=T,log.p=F)
#Compute the Density dt(x,df,ncp,log=F)
#df=v
#Plot Example:	
	x<-seq(-4,4,length=100)
	hx<-dnorm(x)
	degf<-c(1,5,10,60)
	colors<-c("red","blue","darkgreen","gold","black")
	labels<-c("df=1","df=3","df=8","df=30")
	plot(x, hx, type="l", lty=2, xlab="x value",
  ylab="Density", main="Comparison of t Distributions")
	for (i in 1:4){lines(x, dt(x,degf[i]), lwd=2, col=colors[i])}
	legend("topright", inset=.05, title="Distributions", labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)
	#code from http://www.statmethods.net/advgraphs/probability.html

##Pick up at Linear functions of random variables 
	#let X be a discrete or continuous rv with ux= E[X] and sigma^2(x)=var(x). Define a new rv Y to be a linear function of X:
	#Y=g(X)=a*X+b
		#a and b a re known constants
	#then
		#UY=E[Y]=E[a*X+b]
		# = a*E[X]+b=a*UX+b
		#sigma^2(Y)=Var(Y)=Var(a*X +b)
		#= a^2 Var(x)
		#=a^2 * sigma^2(x)
		#sigma(Y)=a*sigma(X)
#Standardizing a Normal rv
	#Let X~N(UX, Sigma^2(X)). The standardized rv Z is crated using:
	# Z= (X-UX)/sigma(X)=X/sigma(X)-UX/sigma(X)
	# Z= aX + b
	#a= 1/sigma(X), b=-UX/sigma(X)
	#Z~N(0,1)
	
###Value at Risk
#Consider a #10,000 invesmtent in Microsoft for 1 Month. Assume:
	#R= simple monthly return on Microsoft
	#R~(0.05,(.1)^2), UR=.05, Sigma(R)=.10
#Goal: Calulate how much we can lose with a specified probability alpha. 
#Work in a few steps:
	#(1) What is the probability distribution of end of month wealth, W1=$10,000*(1+R)?
	#(2)What is Pr(W1<$9,000)?
	#(3) What value of R produces W1= $9,000?
	#(4) What is the monthly value-at-risk (VaR) on the $10,000 invesmtent with 5% probability? that is, how much can we lose if R<=q.05
#Answers:
	#(1)W1=$10,000(1+R) is  linear function of R and R is normally distributed rv. Therefore, W1 is normally distributed with:
		#E[W1]=10000*(1+E[R])
		#=10000*(1+.05)=$10,500
		#Var(W1)=($10,000)^2Var(R)
		#= 10000^2(.1)^2=1,000,000
		# W1~N($10,500,($1,000)^2)
	#(2) Using W1~N($10,500,($1,000)^2)
		#Pr(W1<9000)=.067
		#NORMDIST(9000,10500,1000)
		pnorm(9000,10500,1000)
		#[1] 0.0668072
	#(3) To find R that produces W1=$9,000 solve
		#R=(9000-10000)/10000=-.1
		#Notice that -.1 is the 6.7% quantile of the distribution of R:
		#q.067 = PR(R<-.1)=.067
	#(4) Use R~N(.05,.1^2) and solve for the 5% quantile:
		#Pr(R<q.05)=.05 run in R
		qnorm(.05,.05,.1)
		#[1] -0.1144854
		#If R=-11.4% the loss in the investment value is at least: 10000*(-0.114)= -$1,144 = 5% VaR
#In general, the alpa*100%(VaR) for an initial investment of $W0 is computed as 
	#VaR(alpha)=$W0*qalpha
	#qalpha=alpha*100% quantile of simple return dist
	
	#REMARK: Because VaR represents a loss, it is often reported as a postivie number. For example, -$1,144 represents a loss of $1144. so the Var is reported as $1,144
	#essential just taking a quantile of a distribution. simple when done with normal but thats not always the case. 
	
	#VaR for continuously compounded returns
	#r= ln(1+R), cc monthly return
	#R=e^r -1, simple mothly return
	#Assume:
		# r~N(Ur,sigma^2(r))
		#W0= initial investment
	#100*alpha% Var Computation
		#Compute alpha quantile of normal distribution for r:
			#qr=Ur + sigma(r)z(alpha)
		#Convert alpha quantile for r into alpha for R:
			#qR= e^(qr)-1
		#compute 100*alpha% Var using qRL
			#VaR(alpha)=$W0*qR
	#Example:
		#rt~N(0.05,(.1^2)), W0=10000
	#the 5% cc return quantile is:
		#qr(.05)=Ur+sigma(r)z(.05)
			#=0.05+.1*(-1.645)=-0.114
	#the 5% simple return quantile is 
		#qR(0.05)=e^(qr(.05)) -1 = e^(-.114)-1=-.108
	#the 5% VaR based on a $10,000 intial investment is:
		#VaR(.05)=$10000*(-0.108)= -$1077
		#tends to be a little bit smaller
			#
		
		
		
		
		
		

