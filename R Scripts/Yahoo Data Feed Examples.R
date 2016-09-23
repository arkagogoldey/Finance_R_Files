# Yahoo Stock Data Feed
library(quantmod)
# Function to get symbols
getSymbols(Symbols = NULL, 
           env = .GlobalEnv,
           reload.Symbols = FALSE,
           verbose = FALSE,
           warnings = TRUE,
           src = "yahoo",
           symbol.lookup = TRUE,
           auto.assign = TRUE,
           ...)

showSymbols(env=.GlobalEnv)
removeSymbols(Symbols=NULL,env=.GlobalEnv)
saveSymbols(Symbols = NULL,
            file.path=stop("must specify 'file.path'"),
            env = .GlobalEnv)
#### Only Use this after above is entered
getSymbols.yahoo("AAPL",
                 env=.GlobalEnv,
                 return.class = 'xts',
                 from = "2007-01-01",
                 to = Sys.Date(),)

###### To quickly adjust the start date, first enter
startdate = "2016-01-01"
#### To add multiple symbols at once, enter



###########################################################################
###########################################################################
# http://www.calculatinginvestor.com/2011/05/12/downloading-return-data/
# Goal: download adjusted price data for selected security, convert to returns, and write to output file

library(tseries)
# Longer Explanation#
#Base info
startdate = "2016-01-01"
A <- "EEM"
#####Daily Returns############
p.A <- get.hist.quote(A, quote="Adj", start=startdate, retclass="zoo")
p1.A <- na.locf(p.A)               # Copy last traded price when NA
r.A <- exp(diff(log(p.A)))-1
Aret <- data.frame(r.A)

################Converting into Weekly or Monthly
# Using....
p.A <- get.hist.quote(A, quote="Adj", start=startdate, retclass="zoo")
p1.A <- na.locf(p.A)               # Copy last traded price when NA

# To make week end prices:
nextfri.Date <- function(x) 7 * ceiling(as.numeric(x - 1)/7) + as.Date(1)
pwk.A <- aggregate(p1.A, nextfri.Date,tail,1)

# To convert month end prices:
pmo.A <- aggregate(p1.A, as.yearmon, tail, 1)

# Convert weekly prices into weekly returns
rwk.A <- diff(log(pwk.A)) # convert prices to log returns
rwk1.A <- exp(rwk.A)-1          # back to simple returns
Aretwk <- data.frame(rwk1.A)

# Convert monthly prices into monthly returns
rmo.A <- diff(log(pmo.A))  # convert prices to log returns
rmo1.A <- exp(rmo.A)-1          # back to simple returns
Aretmo <- data.frame(rmo1.A)
##############
# Now Rename to Ticker
EEM <- Aretmo

