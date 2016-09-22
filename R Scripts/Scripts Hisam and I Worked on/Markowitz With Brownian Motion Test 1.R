###Markowitz With Brownian Motion
##Step one load the quantmod library
library(quantmod)
##Step two download stock data for portfolio 
getSymbols(c("AGG","HYG","EMD","TIP","BNDX","SPY","MDY","IWM","EFA","EEM","AMLP","RWO","DJP"), from='2010-01-01')
#Step three convert to weekly data
AGG<-to.weekly(AGG)
HYG<-to.weekly(HYG)
EMD<-to.weekly(EMD)
TIP<-to.weekly(TIP)
BNDX<-to.weekly(BNDX)
SPY<-to.weekly(SPY)
MDY<-to.weekly(MDY)
IWM<-to.weekly(IWM)
EFA<-to.weekly(EFA)
EEM<-to.weekly(EEM)
AMLP<-to.weekly(AMLP)
RWO<-to.weekly(RWO)
DJP<-to.weekly(DJP)

#Step four define variables for adjusted close
AGG_Adj<-AGG$AGG.Adj
HYG_Adj<-HYG$HYG.Adj
EMD_Adj<-EMD$EMD.Adj
TIP_Adj<-TIP$TIP.Adj
BNDX_Adj<-BNDX$BNDX.Adj
SPY_Adj<-SPY$SPY.Adj
MDY_Adj<-MDY$MDY.Adj
IWM_Adj<-IWM$IWM.Adj
EFA_Adj<-EFA$EFA.Adj
EEM_Adj<-EEM$EEM.Adj
AMLP_Adj<-AMLP$AMLP.Adj
RWO_Adj<-RWO$RWO.Adj
DJP_Adj<-DJP$DJP.Adj
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
AGG_Vol<-EWMA(AGG_Adj,.94)
AGG_Vol*sqrt(52)
HYG_Vol<-EWMA(HYG_Adj,.94)
EMD_Vol<-EWMA(EMD_Adj,.94)
TIP_Vol<-EWMA(TIP_Adj,.94)
BNDX_Vol<-EWMA(BNDX_Adj,.94)
SPY_Vol<-EWMA(SPY_Adj,.94)
SPY_Vol*sqrt(52)
MDY_Vol<-EWMA(MDY_Adj,.94)
IWM_Vol<-EWMA(IWM_Adj,.94)
EFA_Vol<-EWMA(EFA_Adj,.94)
EEM_Vol<-EWMA(EEM_Adj,.94)
AMLP_Vol<-EWMA(AMLP_Adj,.94)
RWO_Vol<-EWMA(RWO_Adj,.94)
DJP_Vol<-EWMA(DJP_Adj,.94)
##Step seven caluclate the weekly log returns 
AGG_R<-Delt(AGG_Adj,type="log")
HYG_R<-Delt(HYG_Adj, type="log")
EMD_R<-Delt(EMD_Adj, type="log")
TIP_R<-Delt(TIP_Adj, type="log")
BNDX_R<-Delt(BNDX_Adj, type="log")
SPY_R<-Delt(SPY_Adj, type="log")
MDY_R<-Delt(MDY_Adj, type="log")
IWM_R<-Delt(IWM_Adj, type="log")
EFA_R<-Delt(EFA_Adj, type="log")
EEM_R<-Delt(EEM_Adj, type="log")
AMLP_R<-Delt(AMLP_Adj, type="log")
RWO_R<-Delt(RWO_Adj, type="log")
DJP_R<-Delt(DJP_Adj, type="log")
##Step eight convert the log returns to numeric vectors and remove the NA's in the data
AGG_R.n<-as.numeric(na.omit(AGG_R))
HYG_R.n<-as.numeric(na.omit(HYG_R))
EMD_R.n<-as.numeric(na.omit(EMD_R))
TIP_R.n<-as.numeric(na.omit(TIP_R))
BNDX_R.n<-as.numeric(na.omit(BNDX_R))
SPY_R.n<-as.numeric(na.omit(SPY_R))
MDY_R.n<-as.numeric(na.omit(MDY_R))
IWM_R.n<-as.numeric(na.omit(IWM_R))
EFA_R.n<-as.numeric(na.omit(EFA_R))
EEM_R.n<-as.numeric(na.omit(EEM_R))
AMLP_R.n<-as.numeric(na.omit(AMLP_R))
RWO_R.n<-as.numeric(na.omit(RWO_R))
DJP_R.n<-as.numeric(na.omit(DJP_R))

##Step nine calculate the mean weekly return
AGG_M<-mean(AGG_R.n)
HYG_M<-mean(HYG_R.n)
EMD_M<-mean(EMD_R.n)
TIP_M<-mean(TIP_R.n)
BNDX_M<-mean(BNDX_R.n)
SPY_M<-mean(SPY_R.n)
MDY_M<-mean(MDY_R.n)
IWM_M<-mean(IWM_R.n)
EFA_M<-mean(EFA_R.n)
EEM_M<-mean(EEM_R.n)
AMLP_M<-mean(AMLP_R.n)
RWO_M<-mean(RWO_R.n)
DJP_M<-mean(DJP_R.n)

#The mean returns and the volatility wil be used as the drift and volatility in our brownian motion model.
##Brownian motion Starts here
#Step , download only one year of weekly historical data in a new environment this will be used in the markowitz model
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
a<-replicate(simnumber,rwalk(AGG_M,AGG_Vol[,2],104.69))
b<-replicate(simnumber,rwalk(HYG_M,HYG_Vol[,2],88.79))
c<-replicate(simnumber,rwalk(EMD_M,EMD_Vol[,2],10.96))
d<-replicate(simnumber,rwalk(TIP_M,TIP_Vol[,2],109.61))
e<-replicate(simnumber,rwalk(BNDX_M,BNDX_Vol[,2],49.27))
f<-replicate(simnumber,rwalk(SPY_M,SPY_Vol[,2],180.12))
g<-replicate(simnumber,rwalk(MDY_M,MDY_Vol[,2],242.22))
h<-replicate(simnumber,rwalk(IWM_M,IWM_Vol[,2],114.45))
i<-replicate(simnumber,rwalk(EFA_M,EFA_Vol[,2],64.58))
j<-replicate(simnumber,rwalk(EEM_M,EEM_Vol[,2],38.92))
k<-replicate(simnumber,rwalk(AMLP_M,AMLP_Vol[,2],16.4))
l<-replicate(simnumber,rwalk(RWO_M,RWO_Vol[,2],40.60))
m<-replicate(simnumber,rwalk(DJP_M,DJP_Vol[,2],36.47))
#Check to see how many columns(should be the same as the amount of simulations)
ncol(f)
ncol(g)
##Step fifteen transpose the simulations to have each row represent one simulation and each column to represent one week.
a1<-t(a)
b1<-t(b)
c1<-t(c)
d1<-t(d)
e1<-t(e)
f1<-t(f)
g1<-t(g)
h1<-t(h)
i1<-t(i)
j1<-t(j)
k1<-t(k)
l1<-t(l)
m1<-t(m)
#Step sixteen Calulate the mean accross columns to get the mean forecasted close per week
gamma<-apply(alpha,2,mean)
theta<-apply(beta, 2, mean)
##Step seventeen set the quantiles you want to find
dist=c(0.3,0.5,0.7)
#Step eighteenCalculate the quantiles at the levels set in step seventeen
tilea=sapply(1:ncol(a1),function(x)quantile(a1[,x], dist))
tileb=sapply(1:ncol(b1),function(x)quantile(b1[,x], dist))
tilec=sapply(1:ncol(c1),function(x)quantile(c1[,x], dist))
tiled=sapply(1:ncol(d1),function(x)quantile(d1[,x], dist))
tilee=sapply(1:ncol(e1),function(x)quantile(e1[,x], dist))
tilef=sapply(1:ncol(f1),function(x)quantile(f1[,x], dist))
tileg=sapply(1:ncol(g1),function(x)quantile(g1[,x], dist))
tileh=sapply(1:ncol(h1),function(x)quantile(h1[,x], dist))
tilei=sapply(1:ncol(i1),function(x)quantile(i1[,x], dist))
tilej=sapply(1:ncol(j1),function(x)quantile(j1[,x], dist))
tilek=sapply(1:ncol(k1),function(x)quantile(k1[,x], dist))
tilel=sapply(1:ncol(l1),function(x)quantile(l1[,x], dist))
tilem=sapply(1:ncol(m1),function(x)quantile(m1[,x], dist))
#Step eighteenTranspose to get the quantiles in columns and weeks in rows
blaba<-t(tilea)[,2]
blabb<-t(tileb)[,2]
blabc<-t(tilec)[,2]
blabd<-t(tiled)[,2]
blabe<-t(tilee)[,2]
blabf<-t(tilef)[,2]
blabg<-t(tileg)[,2]
blabh<-t(tileh)[,2]
blabi<-t(tilei)[,2]
blabj<-t(tilej)[,2]
blabk<-t(tilek)[,2]
blabl<-t(tilel)[,2]
blabm<-t(tilem)[,2]
##Step nineteenmake all data in data frame
blaba.df<-as.data.frame(blaba)
blabb.df<-as.data.frame(blabb)
blabc.df<-as.data.frame(blabc)
blabd.df<-as.data.frame(blabd)
blabe.df<-as.data.frame(blabe)
blabf.df<-as.data.frame(blabf)
blabg.df<-as.data.frame(blabg)
blabh.df<-as.data.frame(blabh)
blabi.df<-as.data.frame(blabi)
blabj.df<-as.data.frame(blabj)
blabk.df<-as.data.frame(blabk)
blabl.df<-as.data.frame(blabl)
blabm.df<-as.data.frame(blabm)
colnames(blaba.df)<-"AGG"
colnames(blabb.df)<-"HYG"
colnames(blabc.df)<-"EMD"
colnames(blabd.df)<-"TIP"
colnames(blabe.df)<-"BNDX"
colnames(blabf.df)<-"SPY"
colnames(blabg.df)<-"MDY"
colnames(blabh.df)<-"IWM"
colnames(blabi.df)<-"EFA"
colnames(blabj.df)<-"EEM"
colnames(blabk.df)<-"AMLP"
colnames(blabl.df)<-"RWO"
colnames(blabm.df)<-"DJP"

final<-cbind(blaba.df,blabb.df,blabc.df,blabd.df,blabe.df,blabf.df,blabg.df,blabh.df,blabi.df,blabj.df,blabk.df,blabl.df,blabm.df)
colnames(final)

library(xlsx)
write.xlsx(final,"final.xlsx")







####
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
