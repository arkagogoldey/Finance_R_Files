#Consolidated yahoo Feed

library(quantmod)
library(tseries)
startdate = "2016-01-01"
A <- "EEM"
p.A <- get.hist.quote(A, quote="Adj", start=startdate, retclass="zoo")
p1.A <- na.locf(p.A)
r.A <- exp(diff(log(p.A)))-1
Aret <- data.frame(r.A)
p.A <- get.hist.quote(A, quote="Adj", start=startdate, retclass="zoo")
p1.A <- na.locf(p.A)
nextfri.Date <- function(x) 7 * ceiling(as.numeric(x - 1)/7) + as.Date(1)
pwk.A <- aggregate(p1.A, nextfri.Date,tail,1)
pmo.A <- aggregate(p1.A, as.yearmon, tail, 1)
rwk.A <- diff(log(pwk.A))
rwk1.A <- exp(rwk.A)-1
Aretwk <- data.frame(rwk1.A)
rmo.A <- diff(log(pmo.A))
rmo1.A <- exp(rmo.A)-1
Aretmo <- data.frame(rmo1.A)
EEM.dy <- Aret
EEM.mo <- Aretmo
EEM.wk <- Aretwk


################ Quick Pull ################
ief <- get.hist.quote(instrument="ief", start="2003-01-01",
                      +                       end=Sys.Date(), quote="AdjClose",
                      +                       provider="yahoo", origin="1970-01-01",
                      +                       compression="d", retclass="zoo")