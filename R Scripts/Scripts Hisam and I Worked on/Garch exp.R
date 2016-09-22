install.packages("rugarch")
library(rugarch)
library(RCurl)
library(xts)
URL<-"http://unstarched.net/wp-content/uploads/2013/03/C_2008_1minret.csv"
x<-getURL(URL)
data<-read.csv(textConnection(x))
head(data)

Sys.setenv(TZ = 'GMT')
R_i= xts(data[,2], as.POSIXct(data[,1]))
library(quantmod)
getSymbols('C', from='2000-01-01')
C=adjustOHLC(C,use.Adjusted=TRUE)
R_d=ROC(Cl(C), na.pad=FALSE)

#Correlogram of absolute 1-min returns for citigroup during the sample periods in question:

par(cex.main=.85, col.main='black')
acf(abs(as.numeric(R_i)), lag.max=4000, main ="1-min absolute returns")

#There is a pattern in the acf repeating every 390 periods and showing an increase in volatility around the opening and closing times. 

#Estimation of GARCh Model

#Find the unique days in the intraday sample
n = length(unique(format(index(R_i), '%Y-%m-$d')))
#define a daily spec
spec_d=ugarchspec(mean.model=list(armaOrder=c(1,1)), variance.model=list(model='eGARCH', garchOrder=c(2,1)), distribution='nig')
#Use the ugarchroll method to create a rolling forecast for the period in question

roll = ugarchroll(spec_d, data = R_d['/2008-02-29'], forecast.length = n, refit.every= 1, refit.window = 'moving', moving.size = 2000, calculate.VaR = FALSE)

df=as.data.frame(roll)

f_sigma=as.xts(df[,'Sigma', drop=FALSE])
#now estimate the intraday model

spec = ugarchspec(mean.model = list(armaOrder = c(1, 1), include.mean = TRUE), variance.model = list(model = 'mcsGARCH'), distribution = 'nig')

# DailyVar is the required xts object of the forecast daily variance
fit = ugarchfit(spec=spec,data=R_i, DailyVar=(f_sigma^(2)))