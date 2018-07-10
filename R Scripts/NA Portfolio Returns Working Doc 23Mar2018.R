#### Short Form (Tested on 2018.02.27) ########
setwd("~/GitHub/Finance_R_Files/R Scripts")
getwd()

library(readxl)
library(PortfolioAnalytics)
library(quantmod)
library(PerformanceAnalytics)
library(zoo)
library(plotly)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)
####################################################################################################################################################
##### FILES REQUIRED:
# C:/Users/Micha/Downloads/ModelTR.xlsx
## Model TR must be updated with the most recent daily RETURNS from C:/Users/Micha/Downloads/MF Daily TR Data.xlsx
# C:/Users/Micha/Downloads/Model Alloc Import.xlsx
############# Step 1 #############
### Import Holdings Returns (This uses daily price returns and is suitable for short term return analysis)
Hrets <- read_excel("C:/Users/Micha/Downloads/ModelTR.xlsx")
head(Hrets)

############# Step 2 #############
### Convert Returns to xts
Hrets.df <- (Hrets)
# column names must equal the below $CLASS names
R <- xts(Hrets[-1], Hrets.df$Period)

### MAY NEED TO MULTIPLY BY 100 TO TURN VALUES INTO A PERCENT

############# Step 3 #############
weights <- read_excel("C:/Users/Micha/Downloads/Model Alloc Import.xlsx")
str(weights)
# Check if Numeric, if not execute below
weights <- as.numeric(weights)
# Remove Row name & Select which Model Allocation via the Row
wMatrix <- as.matrix(weights[,-1])
str(wMatrix)
## Create for each of the NA allocation Weights
w85 <- wMatrix[1,]
w75 <- wMatrix[2,]
w65 <- wMatrix[3,]
w55 <- wMatrix[4,]
w55CAM <- wMatrix[5,]
w45 <- wMatrix[6,]
w45CAM <- wMatrix[7,]
w45NatM <- wMatrix[8,]
w35 <- wMatrix[9,]
w35CAM <- wMatrix[10,]
w35NatM <- wMatrix[11,]
w25 <- wMatrix[12,]
w25CAM <- wMatrix[13,]

####################################################################################################################################################
####################################################################################################################################################

############# Step 4 - Calculate Portfolio Returns vis Return.portfolio() #############

r85 <- Return.portfolio(R, weights = w85, wealth.index = FALSE, contribution = FALSE, geometric = TRUE, 
                          rebalance_on = c(NA), value = 1, verbose = FALSE)
r75 <- Return.portfolio(R, weights = w75, wealth.index = FALSE, contribution = FALSE, geometric = TRUE, 
                            rebalance_on = c(NA), value = 1, verbose = FALSE)
r65 <- Return.portfolio(R, weights = w65, wealth.index = FALSE, contribution = FALSE, geometric = TRUE, 
                            rebalance_on = c(NA), value = 1, verbose = FALSE)
r55 <- Return.portfolio(R, weights = w55, wealth.index = FALSE, contribution = FALSE, geometric = TRUE, 
                          rebalance_on = c(NA), value = 1, verbose = FALSE)
r55CAM <- Return.portfolio(R, weights = w55CAM, wealth.index = FALSE, contribution = FALSE, geometric = TRUE, 
                          rebalance_on = c(NA), value = 1, verbose = FALSE)
r45 <- Return.portfolio(R, weights = w45, wealth.index = FALSE, contribution = FALSE, geometric = TRUE, 
                          rebalance_on = c(NA), value = 1, verbose = FALSE)
r45CAM <- Return.portfolio(R, weights = w45CAM, wealth.index = FALSE, contribution = FALSE, geometric = TRUE, 
                          rebalance_on = c(NA), value = 1, verbose = FALSE)
r45NatM <- Return.portfolio(R, weights = w45NatM, wealth.index = FALSE, contribution = FALSE, geometric = TRUE, 
                           rebalance_on = c(NA), value = 1, verbose = FALSE)
r35 <- Return.portfolio(R, weights = w35, wealth.index = FALSE, contribution = FALSE, geometric = TRUE, 
                          rebalance_on = c(NA), value = 1, verbose = FALSE)
r35CAM <- Return.portfolio(R, weights = w35CAM, wealth.index = FALSE, contribution = FALSE, geometric = TRUE, 
                          rebalance_on = c(NA), value = 1, verbose = FALSE)
r35NatM <- Return.portfolio(R, weights = w35NatM, wealth.index = FALSE, contribution = FALSE, geometric = TRUE, 
                          rebalance_on = c(NA), value = 1, verbose = FALSE)
r25 <- Return.portfolio(R, weights = w25, wealth.index = FALSE, contribution = FALSE, geometric = TRUE, 
                          rebalance_on = c(NA), value = 1, verbose = FALSE)
r25CAM <- Return.portfolio(R, weights = w25CAM, wealth.index = FALSE, contribution = FALSE, geometric = TRUE, 
                          rebalance_on = c(NA), value = 1, verbose = FALSE)

###################################################################################################################################################
### Combine then Export
NAModelRets <- cbind(r85,r75,r65,r55,r55CAM,r45,r45CAM,r45NatM,r35,r35CAM,r35NatM,r25,r25CAM)
head(NAModelRets)

### Success, but I'll need to rename the column headers.
colnames(NAModelRets) <- c("NA85","NA75","NA65","NA55","NA55CAM","NA45","NA45CAM","NA45NatM","NA35","NA35CAM","NA35NatM","NA25","NA25CAM")
head(NAModelRets)

## PerformanceAnalytics Package - Cumulative Returns for a period
CNAMRets <- Return.cumulative(NAModelRets, geometric = TRUE)
head(CNAMRets)

## I may want to select specific periods of study, specifically by referring to a date range
## Conversely, i may want to calculate a table of cumulative returns using Return.cumulative()
### It looks like all the major functions are present in the package so that I don't need the 
#    intermediary step of getting the cumulative return chain built and stored.

chart.CumReturns(NAModelRets,wealth.index = FALSE, geometric = TRUE)


#### Risk Metrics ###
% UpsideFrequency()
% DownsideFrequency()
VaR()99-99%
StdDev.annualized()
DownsideDeviation()

#Reference Pertrac docs and or Advisoryworld fact sheets for ideas

###################################################################################################################################################
###################################################################################################################################################
## The functions below are meant for a single column or TS object 

## Try apply.rolling() in addition to cumulative potentially
## Creates the cumulative rollign return ie. Rolling 30 day
Cr85 <- apply.rolling(r85, 30, FUN = Return.cumulative)
head(Cr85)

## Return Table (Function expets one return series rather than the combined TS object)
RetTable <- table.CalendarReturns(NAModelRets, digits = 4, as.perc = TRUE)



###################################################################################################################################################
###################################################################################################################################################
## Write to Excel
# https://www.statmethods.net/input/exportingdata.html
library(xlsx)

# Replace "mydata" or the first variable with the desired variable to export
write.xlsx2(RetTable, "c:/Users/Micha/Downloads/RetTable.xlsx")
