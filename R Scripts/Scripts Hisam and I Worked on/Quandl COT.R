install.packages("Quandl")
library(Quandl)
Quandl.auth("1zm1xSnnoqFeAGksg3S1")


CG<-Quandl("CFTC/GC_FO_ALL", start_date="2007-01-01")
####GOLD#####
NpM<-CG[,8]-CG[,9]
NpC<-CG[,3]-CG[,4]
NpT<-CG[,14]-CG[,15]
NPT<-CG[,14]/CG[,2]
NPTS<-CG[,14]/CG[,15]

NetpositioningM<-(NpM[367:419]-min(NpM[367:419]))/(max(NpM[367:419])-min(NpM[367:419]))

NetpositioningC<-(NpC[367:419]-min(NpC[367:419]))/(max(NpC[367:419])-min(NpC[367:419]))

NetpositioningT<-(NpT[367:419]-min(NpT[367:419]))/(max(NpT[367:419])-min(NpT[367:419]))

##plot indicators
plot(CG[367:419,1],NetpositioningT,type='l',ylim=c(0,1.1))
lines(CG[367:419,1],NetpositioningM, col=2)
lines(CG[367:419,1],NetpositioningC, col=3)

#correlations
cor(NetpositioningM,NetpositioningC)

##Oil
CO<-Quandl("CFTC/WS_FO_ALL",start_date="2007-01-01")
colnames(CO)
NpM.o<-CO[,8]-CO[,9]
length(NpM.o)
NpC.o<-CO[,3]-CO[,4]
NpT.o<-CO[,14]-CO[,15]
NPT.o<-CO[,14]/CO[,2]
NPTS.o<-CO[,14]/CO[,15]
####Managers
NetpositioningM.o<-(NpM.o[299:351]-min(NpM.o[299:351]))/(max(NpM.o[299:351])-min(NpM.o[299:351]))

###Commerical
NetpositioningC.o<-(NpC.o[299:351]-min(NpC.o[299:351]))/(max(NpC.o[299:351])-min(NpC.o[299:351]))

###total reportable
NetpositioningT.o<-(NpT.o[299:351]-min(NpT.o[299:351]))/(max(NpT.o[299:351])-min(NpT.o[299:351]))

##plot
plot(CO[299:351,1],NetpositioningM.o,type='l',ylim=c(0,1.1))
lines(CO[299:351,1],NetpositioningT.o, col=2)
lines(CO[299:351,1],NetpositioningC.o, col=2)

##Correlations
cor(NetpositioningM.o,NetpositioningC.o)