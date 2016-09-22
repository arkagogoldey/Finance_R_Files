###Markowitz With Brownian Motion
##Step one load the quantmod library
library(quantmod)
##Step two download stock data for portfolio 
getSymbols(c("SPY","COST"), from='2010-01-01')
#Step three convert to weekly data
SPY<-to.weekly(SPY)
head(SPY)
COST<-to.weekly(COST)
#Step four define variables for adjusted close
SPY_Adj<-SPY$SPY.Adjusted
COST_Adj<-COST$COST.Adjusted
#Step five the exponentially weighted moving average volatility forecast.
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
##Step six use the EWMA Function to calculate volatility
	#Volatility is the second column of these functions
SPY_VOL<-EWMA(SPY_Adj,.94)
COST_VOL<-EWMA(COST_Adj,.94)

##Step seven caluclate the weekly log returns 
SPY_R<-Delt(SPY_Adj,type="log")
COST_R<-Delt(COST_Adj, type="log")
##Step eight convert the log returns to numeric vectors and remove the NA's in the data
SPY_R.n<-as.numeric(na.omit(SPY_R))
COST_R.n<-as.numeric(na.omit(COST_R))
##Step nine calculate the mean weekly return
SPY_M<-mean(SPY_R.n)
COST_M<-mean(COST_R.n)

#The mean returns and the volatility wil be used as the drift and volatility in our brownian motion model.
##Brownian motion Starts here
#Step ten, download only one year of weekly historical data in a new environment this will be used in the markowitz model
spy_52<-new.env()
cost_52<-new.env()
getSymbols("SPY",from='2014-01-13',env=spy_52)
getSymbols("COST", from ='2014-01-13',env=cost_52)
spy_52_weekly<-to.weekly(spy_52$SPY)
cost_52_weekly<-to.weekly(cost_52$COST)
spy_52_weekly.n<-as.numeric(spy_52_weekly[,6])
cost_52_weekly.n<-as.numeric(cost_52_weekly[,6])

######Step eleven simulate lognormal random walk#######
rwalk<-function(mu, sigma, p)
{
	#daily drift and volatility
	days<- 104
	mu.daily <- mu
	sigma.daily<-sigma
	
	r<-rnorm(days,mu.daily,sigma.daily) #100 random normals
	logPrice<-log(p)+cumsum(r)
	
	Prices<- exp(logPrice)
}
####Step twelve test the random walk once#####
f<-rwalk(SPY_M,SPY_VOL[,2],spy_52_weekly.n[[1]])
g<-rwalk(COST_M,COST_VOL[,2],cost_52_weekly.n[[1]])
#test plot
plot(f, type='l')
plot(g, type='l')
###Step thirteen number of simulations to run#####
simnumber<-12
##Step fourteen run the simulation
f<-replicate(simnumber,rwalk(SPY_M,SPY_VOL[,2],spy_52_weekly.n[[1]]))
g<-replicate(simnumber,rwalk(COST_M,COST_VOL[,2],cost_52_weekly.n[[1]]))
#Check to see how many columns(should be the same as the amount of simulations)
ncol(f)
ncol(g)
##Step fifteen transpose the simulations to have each row represent one simulation and each column to represent one week.
alpha<-t(f)
beta<-t(g)
#Step sixteen Calulate the mean accross columns to get the mean forecasted close per week
gamma<-apply(alpha,2,mean)
theta<-apply(beta, 2, mean)
##Step seventeen set the quantiles you want to find
dist=c(0.3,0.5,0.7)
#Step eighteenCalculate the quantiles at the levels set in step seventeen
tile=sapply(1:ncol(alpha),function(x)quantile(alpha[,x], dist))
tile1=sapply(1:ncol(beta),function(x)quantile(beta[,x], dist))
#Step eighteenTranspose to get the quantiles in columns and weeks in rows
blab<-t(tile)
blab1<-t(tile1)
##Step nineteenmake all data in data frame
s<-as.data.frame(spy_52_weekly.n)
c<-as.data.frame(cost_52_weekly.n)
##Step twenty set variables to represent the quantiles
thirty<-as.data.frame(blab[,1])
thirty1<-as.data.frame(blab1[,1])
fifty<-as.data.frame(blab[,2])
fifty1<-as.data.frame(blab1[,2])
seventy<-as.data.frame(blab[,3])
seventy1<-as.data.frame(blab1[,3])
##Step twenty one Correlations for each quantile and squared correlations
sct<-cor(s,thirty[1:nrow(s),])
sct
sct^2

scf<-cor(s,fifty[0:nrow(s),])
scf
scf^2

scs<-cor(s,seventy[0:nrow(s),])
scs
scs^2

cct<-cor(c,thirty1[0:nrow(c),])
cct
cct^2

ccf<-cor(c,fifty1[0:nrow(c),])
ccf
ccf^2

ccs<-cor(c,seventy1[0:nrow(c),])
ccs
ccs^2
##Step twenty two Merge the brownian motion with the actual data
colnames(fifty)<-colnames(s)
binded<-rbind(s,fifty)
binded<-binded[-c(54:106),]

colnames(fifty1)<-colnames(c)
binded1<-rbind(c,fifty1)
binded1<-binded1[-c(54:106),]

##Step twenty three Plot the extension
plot(binded,type='l')
plot(binded1, type='l')
##line showing where forecast begins
jag<-c(53)
jag1<-c(0:300)
lines(rep(jag,301),jag1,col=2)

####Markowitz portfolio optimization

##Markowitz's Mordern Portfolio Theory
install.packages("stockPortfolio")
#we will use this package to download stock data
install.packages("quadprog")
#Step twenty four we will use this package to optimize the portfolio
#load in the packages
library(stockPortfolio)
library(quadprog)
#Step twenty five get the weekly returns:
binded_returns<- as.data.frame(na.omit(Delt(binded, type="log")))
colnames(binded_returns)<-"weekly returns"
binded1_returns<- as.data.frame(na.omit(Delt(binded1, type="log")))
#Step twenty six merge the returns together by column
#View a summary of the data
binded_returns<-cbind(binded_returns,binded1_returns)
colnames(binded_returns)<-c("SPY","COST")
head(binded_returns)
eff.frontier <- function (returns, short="no", max.allocation=NULL, risk.premium.up=.5, risk.increment=.005){
# return argument should be a m x n matrix with one column per security
# short argument is whether short-selling is allowed; default is no (short selling prohibited)
# max.allocation is the maximum % allowed for any one security (reduces concentration)
# risk.premium.up is the upper limit of the risk premium modeled (see for loop below)
# risk.increment is the increment (by) value used in the for loop
 
covariance <- cov(returns)
print(covariance)
n <- ncol(covariance)
 
# Create initial Amat and bvec assuming only equality constraint (short-selling is allowed, no allocation constraints)
Amat <- matrix (1, nrow=n)
bvec <- 1
meq <- 1
 
# Then modify the Amat and bvec if short-selling is prohibited
if(short=="no"){
Amat <- cbind(1, diag(n))
bvec <- c(bvec, rep(0, n))
}
 
# And modify Amat and bvec if a max allocation (concentration) is specified
if(!is.null(max.allocation)){
if(max.allocation > 1 | max.allocation <0){
stop("max.allocation must be greater than 0 and less than 1")
}
if(max.allocation * n < 1){
stop("Need to set max.allocation higher; not enough assets to add to 1")
}
Amat <- cbind(Amat, -diag(n))
bvec <- c(bvec, rep(-max.allocation, n))
}
 
# Calculate the number of loops based on how high to vary the risk premium and by what increment
loops <- risk.premium.up / risk.increment + 1
loop <- 1
 
# Initialize a matrix to contain allocation and statistics
# This is not necessary, but speeds up processing and uses less memory
eff <- matrix(nrow=loops, ncol=n+3)
# Now I need to give the matrix column names
colnames(eff) <- c(colnames(returns), "Std.Dev", "Exp.Return", "sharpe")
 
# Loop through the quadratic program solver
for (i in seq(from=0, to=risk.premium.up, by=risk.increment)){
dvec <- colMeans(returns) * i # This moves the solution up along the efficient frontier
sol <- solve.QP(covariance, dvec=dvec, Amat=Amat, bvec=bvec, meq=meq)
eff[loop,"Std.Dev"] <- sqrt(sum(sol$solution *colSums((covariance * sol$solution))))
eff[loop,"Exp.Return"] <- as.numeric(sol$solution %*% colMeans(returns))
eff[loop,"sharpe"] <- eff[loop,"Exp.Return"] / eff[loop,"Std.Dev"]
eff[loop,1:n] <- sol$solution
loop <- loop+1
}
 
return(as.data.frame(eff))
}
#possible combinations of weights
eff <- eff.frontier(returns=binded_returns, short="no", max.allocation=1, risk.premium.up=.5, risk.increment=.001)

head(eff)

#optimized portfolio weights
eff.optimal.point <- eff[eff$sharpe==max(eff$sharpe),]
eff.optimal.point
