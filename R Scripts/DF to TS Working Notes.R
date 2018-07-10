#### Working File ####
library(readxl)
ConsBenchRetsEQLDates <- read_excel("C:/Users/Micha/Downloads/ConsBenchRetsEQLDates.xlsx", 
                                    col_types = c("date", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric"))
BRT <- ConsBenchRetsEQLDates
View(BRT)
print(BRT[1,])

dates <- BRT$date
values <- BRT[,-1]

## Start = 2000-02-01
## End = 2017-08-01
## total count of 211
## Frequency = month or 12
## Base package uses "YYYY-MM-DD" format
dates <- seq(as.Date("2000-02-01"), length=211, by="month")


BRT_ts <- ts(BRT, frequency=211, start=c("2000-02-01", 12))

attributes()
# Code Value Example
# %d Day of the month (decimal number) 23
# %m Month (decimal number) 11
# %b Month (abbreviated) Jan
# %B Month (full name) January
# %y Year (2 digit) 90
# %Y Year (4 digit) 1990


###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################



# https://faculty.washington.edu/ezivot/econ424/Working%20with%20Time%20Series%20Data%20in%20R.pdf
# pg 25
td = seq(as.Date("1993/3/1"), as.Date("2003/3/1"), "months")
class(td)
# Returns "date"

######## WORKS ##############
## Start = 2000-02-01
## End = 2017-08-01

dates = seq(as.Date("2000/02/01"), as.Date("2017/08/01"), "months")
GSCIdata.df <- C(GSCIdata, dates)
td2 = as.Date(GSCIdata.df$Date, format="%m/%d/%Y") 
### GOOD ^^^


### Before BRT.data <- BRT[,-1] is executed, I need to convert the rownames into formatted dates "1985-03-15"
# https://www.datacamp.com/community/blog/r-xts-cheat-sheet
# https://stackoverflow.com/questions/19835587/r-transform-data-frame-to-time-series
library(zoo)
ZOO <- zoo(df$Close, order.by=as.Date(as.character(df$Date), format='%m/%d/%Y'))
ZOO
## Customized:
ZOO <- zoo(BRT.df$GSCI, order.by=as.Date(as.character(BRT.df$Date), format='%m/%d/%Y'))

##
# http://www.finance-r.com/s/csv_to_xts_conversion/complete/
# https://cran.r-project.org/web/packages/xts/xts.pdf
# https://cran.r-project.org/web/packages/zoo/zoo.pdf

###############################################################################################################
# https://stackoverflow.com/questions/10562663/creating-ts-objects-in-r?rq=1
data <- rnorm(3650, m=10, sd=2)
# change is below, use ts() to create time series
data_ts <- ts(data, frequency=365, start=c(1919, 1))
attributes(data_ts)
dcomp<-decompose(data_ts, type=c("additive"))
plot(dcomp)

###############################################################################################################
###############################################################################################################
########################################## MONTHLY EXAMPLE WORKS !!!!!!! ######################################
###############################################################################################################
###############################################################################################################
# Monthly Example
# http://www.statmethods.net/advstats/timeseries.html
# save a numeric vector containing 72 monthly observations
# from Jan 2009 to Dec 2014 as a time series object
myts <- ts(myvector, start=c(2009, 1), end=c(2014, 12), frequency=12) 

# subset the time series (June 2014 to December 2014)
myts2 <- window(myts, start=c(2014, 6), end=c(2014, 12))

###############################################################################################################
# https://stackoverflow.com/questions/20253696/make-my-date-column-row-label-index-in-r-data-frame
####### Code from site 
library(xts)
dat.xts <- xts(x=dat[,-1],order.by= as.POSIXct(dat$Date))

###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################


BRT.data  <- BRT[,-1]
BRTdates <- BRT[,1]
row.names(BRT.data) <- BRTdates
row.names(BRT.data)

###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################


my.date = as.Date("1970/1/1")
my.date
#"1970-01-01"
class(my.date)
# "Date"
as.numeric(my.date)
# 0
myDates = c("2013-12-19", "2003-12-20")
as.Date(myDates)
# "2013-12-19" "2003-12-20"


###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################



#https://www.datacamp.com/community/blog/r-xts-cheat-sheet
xts1 <- xts(x=1:10, order.by=Sys.Date()-1:10)
data <- rnorm(5)
dates <- seq(as.Date("2017-05-01"), length=5, by="days")
xts2 <- xts(x=data, order.by=dates)
xts3 <- xts(x=rnorm(10), order.by=as.POSIXct(Sys.Date()+1:10), born=as.POSIXct("1899-05-08")
            xts4 <- xts(x=1:10, order.by=Sys.Date()+1:10)