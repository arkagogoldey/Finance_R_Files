## Open and Run on Startup
setwd("~/GitHub/Finance_R_Files/R Scripts")
getwd()

################################################################################################################
#################################################### Part 0 ####################################################
################################################# Excel Upload #################################################
################################################################################################################


## REQUIRED DOCUMENTS in DOWNLOADS FOLDER
# AnnBenchRets
# NABaseConstraints
# GSAMBenchRets
# GSAMAnnBenchRets


##Upload Excel Benchamrk Tables
## BRT short for Bench Return Table
## read.excel formula is executed utilizing the Rstudio GUI ##
## I'll want to review the table data and designate the data types of each column.
library(readxl)
ConsBenchRetsEQLDates <- read_excel("C:/Users/Michael/Downloads/AnnBenchRets.xlsx", 
                                    col_types = c("date", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric",
                                                  "numeric", "numeric", "numeric",
                                                  "numeric", "numeric"))
BRT <- ConsBenchRetsEQLDates
View(BRT)
print(BRT[1,])

################################################################################################################
#################################################### Part 1 ####################################################
######################### Calculating Std Deviation for Standard benchmark Allocations #########################
################################################################################################################

# THIS sEGMENT IS ISOLATED FROM FURTHER CODE

## Want to isolate each of the columns
#ie. Isolating Benchmarks from Table
GSCI <- (BRT[,2])
USHY <- (BRT[,3])
USAGG <- (BRT[,4])
USLC <- (BRT[,5])
USMC <- (BRT[,6])
USSC <- (BRT[,7])
EAFE <- (BRT[,8])
EM <- (BRT[,9])
USRE <- (BRT[,10])
CASH <- (BRT[,11])
INTLFI <- (BRT[,12])
GLBLRE <- (BRT[,13])
INTLSC <- (BRT[,14])
INFRA <- (BRT[,15])
BKLOANS <- (BRT[,16])
EMDEBT <- (BRT[,17])

# Dates Can be added back to the portfolios after

##Base Risk Portfolio Allocations
##Allocations based on Risk Number Targets - Goal is to get Sample Std Dev
Risk25<- ((USLC*0.17)+(USAGG*0.83))
Risk35<- ((USLC*0.38)+(USAGG*0.62))
Risk45<- ((USLC*0.54)+(USAGG*0.46))
Risk55<- ((USLC*0.69)+(USAGG*0.31))
Risk65<- ((USLC*0.83)+(USAGG*0.17))
Risk75<- ((USLC))
Risk85<- ((EAFE*0.90)+(USAGG*0.17))

##### Calculate Standard Deviation for Each
## if monthly data, then Risk25sd <- sqrt(var(Risk25))*sqrt(12)
## if Annual, then Risk25sd <- sqrt(var(Risk25))

Risk25sd <- sqrt(var(Risk25))
Risk35sd <- sqrt(var(Risk35))
Risk45sd <- sqrt(var(Risk45))
Risk55sd <- sqrt(var(Risk55))
Risk65sd <- sqrt(var(Risk65))
Risk75sd <- sqrt(var(Risk75))
Risk85sd <- sqrt(var(Risk85))

print(Risk25sd)
print(Risk35sd)
print(Risk45sd)
print(Risk55sd)
print(Risk65sd)
print(Risk75sd)
print(Risk85sd)

################################################################################################################
#################################################### Part 2 ####################################################
########################### Running the Optimizer with Orion Exported data in Excel ############################
################################################################################################################

#Dependent on BRT object imported from above

library(PortfolioAnalytics)
library(quantmod)
library(PerformanceAnalytics)
library(zoo)
library(plotly)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)
library(foreach)

####### The Data below is for monthly data
# http://www.statmethods.net/advstats/timeseries.html
# Check the Dates before utilising

BRT_ts <- ts(BRT[,-1], start=c(2002, 12), end=c(2018, 6), frequency=12)

port <- portfolio.spec(assets = c("GSCI", "USHY", "USAGG", "USLC", "USMC", "USSC", 
                                  "EAFE", "EM", "USRE", "CASH", "INTLFI","GLBLRE",
                                  "INTLSC","INFRA","BKLOANS","EMDEBT"))

## TEST IF IMPORTED MIN & MAX objects work
NABaseConstraints <- read_excel("C:/Users/Michael/Downloads/NABaseConstraints.xlsx", 
                                    col_types = c("text", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric",
                                                  "numeric", "numeric", "numeric",
                                                  "numeric", "numeric"))

str(NABaseConstraints)
# Check if Numeric, if not execute below
#BaseWeights <- as.numeric(NABaseConstraints)
# Remove Row name & Select which Model Allocation via the Row
wBase <- as.matrix(NABaseConstraints[,-1])
str(wBase)
## Create for each of the GSAM allocation Weights
w85max <- wBase[1,]
w85min <- wBase[2,]
w75max <- wBase[3,]
w75min <- wBase[4,]
w65max <- wBase[5,]
w65min <- wBase[6,]
w55max <- wBase[7,]
w55min <- wBase[8,]
w45max <- wBase[9,]
w45min <- wBase[10,]
w35max <- wBase[11,]
w35min <- wBase[12,]
w25max <- wBase[13,]
w25min <- wBase[14,]

BasePort <- portfolio.spec(assets = c("GSCI", "USHY", "USAGG", "USLC", "USMC", "USSC", 
                                  "EAFE", "EM", "USRE", "CASH", "INTLFI","GLBLRE",
                                  "INTLSC","INFRA","BKLOANS","EMDEBT"))
BasePort <- add.objective(BasePort, type='return',name='mean')
n <- 5000 #Search Size in optimize.portfolio
Base85 <- add.constraint(BasePort, type = "box", min=w85min, max=w85max)
opt_Base85 <- optimize.portfolio(R=BRT_ts, portfolio=Base85,
                                 optimize_method="random",
                                 search_size= n,
                                 trace=TRUE, traceDE=5)
Base75 <- add.constraint(BasePort, type = "box", min=w75min, max=w75max)
opt_Base75 <- optimize.portfolio(R=BRT_ts, portfolio=Base75,
                                 optimize_method="random",
                                 search_size= n,
                                 trace=TRUE, traceDE=5)
Base65 <- add.constraint(BasePort, type = "box", min=w65min, max=w65max)
opt_Base65 <- optimize.portfolio(R=BRT_ts, portfolio=Base65,
                                 optimize_method="random",
                                 search_size= n,
                                 trace=TRUE, traceDE=5)
Base55 <- add.constraint(BasePort, type = "box", min=w55min, max=w55max)
opt_Base55 <- optimize.portfolio(R=BRT_ts, portfolio=Base55,
                                 optimize_method="random",
                                 search_size= n,
                                 trace=TRUE, traceDE=5)
Base45 <- add.constraint(BasePort, type = "box", min=w45min, max=w45max)
opt_Base45 <- optimize.portfolio(R=BRT_ts, portfolio=Base45,
                                 optimize_method="random",
                                 search_size= n,
                                 trace=TRUE, traceDE=5)
Base35 <- add.constraint(BasePort, type = "box", min=w35min, max=w35max)
opt_Base35 <- optimize.portfolio(R=BRT_ts, portfolio=Base35,
                                 optimize_method="random",
                                 search_size= n,
                                 trace=TRUE, traceDE=5)
Base25 <- add.constraint(BasePort, type = "box", min=w25min, max=w25max)
opt_Base25 <- optimize.portfolio(R=BRT_ts, portfolio=Base25,
                                 optimize_method="random",
                                 search_size= n,
                                 trace=TRUE, traceDE=5)

summary(opt_Base85)
summary(opt_Base75)
summary(opt_Base65)
summary(opt_Base55)
summary(opt_Base55)
summary(opt_Base45)
summary(opt_Base35)
summary(opt_Base25)

extractStats(opt_Base25)
print(opt_Base25)


#####################################################################################################################
# GSAM EXAMPLES (Our Benchmark data and timeframes)
#####################################################################################################################
# GSAM Base Allocations
library(readxl)
weights <- read_excel("C:/Users/Michael/Downloads/GSAMBaseWeights.xlsx")
str(weights)
# Check if Numeric, if not execute below
weights <- as.numeric(weights)
# Remove Row name & Select which Model Allocation via the Row
wMatrix <- as.matrix(weights[,-1])
str(wMatrix)
## Create for each of the GSAM allocation Weights
w100_0 <- wMatrix[1,]
w90_10 <- wMatrix[2,]
w80_20 <- wMatrix[3,]
w70_30 <- wMatrix[4,]
w60_40 <- wMatrix[5,]
w50_50 <- wMatrix[6,]
w40_60 <- wMatrix[7,]
w30_70 <- wMatrix[8,]

GSAMRets <- read_excel("C:/Users/Michael/Downloads/GSAMAnnBenchRets.xlsx", 
                       col_types = c("date", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "numeric",
                                     "numeric", "numeric"))
View(GSAMRets)
View(GSAMRets[1,])
GSAM_ts <- ts(GSAMRets[,-1], start=c(2002, 12), end=c(2018, 6), frequency=12)

GSAM100_0 <- Return.portfolio(GSAM_ts, weights = w100_0,geometric = FALSE,verbose=FALSE)
GSAM90_10 <- Return.portfolio(GSAM_ts, weights = w90_10,geometric = FALSE,verbose=FALSE)
GSAM80_20 <- Return.portfolio(GSAM_ts, weights = w80_20,geometric = FALSE,verbose=FALSE)
GSAM70_30 <- Return.portfolio(GSAM_ts, weights = w70_30,geometric = FALSE,verbose=FALSE)
GSAM60_40 <- Return.portfolio(GSAM_ts, weights = w60_40,geometric = FALSE,verbose=FALSE)
GSAM50_50 <- Return.portfolio(GSAM_ts, weights = w50_50,geometric = FALSE,verbose=FALSE)
GSAM40_60 <- Return.portfolio(GSAM_ts, weights = w40_60,geometric = FALSE,verbose=FALSE)
GSAM30_70 <- Return.portfolio(GSAM_ts, weights = w30_70,geometric = FALSE,verbose=FALSE)

print(GSAM100_0)
summary(GSAM100_0)

meanGSAM100_0 <- mean(GSAM100_0)
meanGSAM90_10 <- mean(GSAM90_10)
meanGSAM80_20 <- mean(GSAM80_20)
meanGSAM70_30 <- mean(GSAM70_30)
meanGSAM60_40 <- mean(GSAM60_40)
meanGSAM50_50 <- mean(GSAM50_50)
meanGSAM40_60 <- mean(GSAM40_60)
meanGSAM30_70 <- mean(GSAM30_70)

volGSAM100_0 <- StdDev(GSAM100_0)
volGSAM90_10 <- StdDev(GSAM90_10)
volGSAM80_20 <- StdDev(GSAM80_20)
volGSAM70_30 <- StdDev(GSAM70_30)
volGSAM60_40 <- StdDev(GSAM60_40)
volGSAM50_50 <- StdDev(GSAM50_50)
volGSAM40_60 <- StdDev(GSAM40_60)
volGSAM30_70 <- StdDev(GSAM30_70)

print(meanGSAM100_0)
print(meanGSAM90_10)
print(meanGSAM80_20)
print(meanGSAM70_30)
print(meanGSAM60_40)
print(meanGSAM50_50)
print(meanGSAM40_60)
print(meanGSAM30_70)

print(volGSAM100_0)
print(volGSAM90_10)
print(volGSAM80_20)
print(volGSAM70_30)
print(volGSAM60_40)
print(volGSAM50_50)
print(volGSAM40_60)
print(volGSAM30_70)

#Portfolios are not XTS or zoo objects, ,summary works, but extract stats does not
##ERROR
extractStats(GSAM100_0)


################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
######## FUNCTIONS BELOW ARE FOR REFERENCE
##
port25 <- add.constraint(port, type = "box", 
                       min=c(0.00, 0.00, 0.50, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,  0.00,   0.00,   0.00,   0.00,    0.00),
                       max=c(0.15, 0.30, 0.80, 0.30, 0.20, 0.20, 0.20, 0.20, 0.30, 0.10, 0.40,  0.20,   0.15,   0.20,   0.20,    0.30))

#######                    c(GSCI, USHY, USAGG, USLC, USMC, USSC, EAFE, EM , USRE, CASH, INTLFI, GLBLRE, INTLSC, INFRA, BKLOANS, EMDEBT)
structure(port25)
## TARGETED RISK (Risk Budget) ##
### SEE IF THE TWO Objectives need to be added previous to this objective
### SEE ABOUT REPLACING STD DEV FOR ETL AND ADD A TARGET STD DEV LEVEL

# Testing for Std instead of ETL
port25 <- add.objective(portfolio=port25, type='risk',name='StdDev')
port25 <- add.objective(portfolio=port25, type='return',name='mean')
# Use this to determine the max ret portfolios given a defined risk limit

## Correct
opt_port25 <- optimize.portfolio(R=BRT_ts, portfolio=port25,
                                     optimize_method="random",
                                     search_size=50000,
                                     trace=TRUE, traceDE=5)
summary(opt_port25)
extractStats(opt_port25)
print(opt_port25)

#####################################################################################################################
# Different Examples
#####################################################################################################################
port1 <- portfolio.spec(assets = c("GSCI", "USHY", "USAGG", "USLC", "USMC", "USSC", 
                                  "EAFE", "EM", "USRE", "CASH", "INTLFI","GLBLRE",
                                  "INTLSC","INFRA","BKLOANS","EMDEBT"))
port1 <- add.constraint(portfolio = port1, type = "full_investment")
port1 <- add.constraint(port1, type = "box", 
                       min=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                       max=c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
#Adding max_risk doesn't cause an error, but I'm not sure if ii works
port1 <- add.objective(portfolio=port1, type='risk',name='var', target=0.4)
port1 <- add.objective(portfolio=port1, type='return',name='mean')
opt_port1 <- optimize.portfolio(R=BRT_ts, portfolio=port1,
                               optimize_method="random",
                               search_size=1000,
                               trace=TRUE, traceDE=5)
print(opt_port1)
#######################################
# Different Approach
#######################################
port2 <- portfolio.spec(assets = c("GSCI", "USHY", "USAGG", "USLC", "USMC", "USSC", 
                                   "EAFE", "EM", "USRE", "CASH", "INTLFI","GLBLRE",
                                   "INTLSC","INFRA","BKLOANS","EMDEBT"))
port2 <- add.constraint(portfolio = port1, type = "full_investment")
port2 <- add.constraint(port2, type = "box", 
                        min=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0.00, 0, 0, 0, 0, 0, 0),
                        max=c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0.05, 1, 1, 1, 1, 1, 1))
rportfolios <- random_portfolios(port2, permutations = 10000, rp_method = "sample")

minmax.port <- add.objective(port2, type = "risk", name = "var")
minmax.port <- add.objective(port2, type = "return", name = "mean")
minmax.opt <- optimize.portfolio(BRT_ts, minmax.port, optimize_method = "random", rp = rportfolios)
print(minmax.opt)

minvar.port <- add.objective(port2, type = "risk", name = "var", target=0.04)
minvar.opt <- optimize.portfolio(BRT_ts, minvar.port, optimize_method = "random", rp = rportfolios)
print(minvar.opt)

maxret.port <- add.objective(port2, type = "return", name = "mean")
maxret.opt <- optimize.portfolio(BRT_ts, maxret.port, optimize_method = "random", rp = rportfolios)

#######################################
############# (GSCI, USHY, USAGG, USLC, USMC, USSC, EAFE,  EM,  USRE, CASH, INTLFI, GLBLRE, INTLSC, INFRA, BKLOANS, EMDEBT)
weights85 <- c(0.00, 0.05,  0.00, 0.25, 0.20, 0.20, 0.15, 0.10, 0.03, 0.02, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00)
weights75 <- c(0.00, 0.05,  0.05, 0.20, 0.16, 0.17, 0.15, 0.10, 0.05, 0.02, 0.05, 0.00, 0.00, 0.00, 0.00, 0.00)
weights65 <- c(0.00, 0.10,  0.10, 0.18, 0.13, 0.13, 0.13, 0.09, 0.07, 0.02, 0.05, 0.00, 0.00, 0.00, 0.00, 0.00)
weights55 <- c(0.00, 0.10,  0.20, 0.15, 0.07, 0.08, 0.12, 0.08, 0.07, 0.03, 0.10, 0.00, 0.00, 0.00, 0.00, 0.00)
weights45 <- c(0.00, 0.10,  0.35, 0.12, 0.06, 0.07, 0.10, 0.05, 0.06, 0.04, 0.05, 0.00, 0.00, 0.00, 0.00, 0.00)
weights35 <- c(0.00, 0.10,  0.45, 0.10, 0.00, 0.05, 0.10, 0.05, 0.05, 0.05, 0.05, 0.00, 0.00, 0.00, 0.00, 0.00)
weights25 <- c(0.00, 0.10,  0.60, 0.08, 0.00, 0.00, 0.07, 0.00, 0.00, 0.05, 0.10, 0.00, 0.00, 0.00, 0.00, 0.00)

#if data is monthly, Ann.var85 <- (sqrt(12)*(StdDev(BRT_ts, weights=weights85)))
Ann.var85 <- (StdDev(BRT_ts, weights=weights85))
Ann.var75 <- (StdDev(BRT_ts, weights=weights75))
Ann.var65 <- (StdDev(BRT_ts, weights=weights65))
Ann.var55 <- (StdDev(BRT_ts, weights=weights55))
Ann.var45 <- (StdDev(BRT_ts, weights=weights45))
Ann.var35 <- (StdDev(BRT_ts, weights=weights35))
Ann.var25 <- (StdDev(BRT_ts, weights=weights25))

print(Ann.var85)
print(Ann.var75)
print(Ann.var65)
print(Ann.var55)
print(Ann.var45)
print(Ann.var35)
print(Ann.var25)

################################################################################################################
################################################################################################################
#################################################### Part 3 ####################################################
######################################## Plotting / Building an Efficient Frontier #############################
################################################################################################################
################################################################################################################

## Iterations of random portfolios (Resampled Efficiency Technique)
##### minvar and maxret may not be neccesary unless we need to design the efficient frontier
##### A single efficient frontier is not being used across all the portfolio strategies
##### because the constraints and objectives are unique across portfolios
rportfolios <- random_portfolios(port, permutations = 5000, rp_method = "sample")
minvar.port <- add.objective(port, type = "risk", name = "var")
minvar.opt <- optimize.portfolio(BRT_ts, minvar.port, optimize_method = "random", rp = rportfolios)
maxret.port <- add.objective(port, type = "return", name = "mean")
maxret.opt <- optimize.portfolio(BRT_ts, maxret.port, optimize_method = "random", rp = rportfolios)

#### TO get the target allocation, adjust the Min return variable, then get the value from minvol.opt
## Min Return ###
minret <- 0.50/100
maxret <- maxret.opt$weights %*% meanReturns
vec <- seq(minret, maxret, length.out = 100)
eff.frontier <- data.frame(Risk = rep(NA, length(vec)),
                           Return = rep(NA, length(vec)), 
                           SharpeRatio = rep(NA, length(vec)))

frontier.weights <- mat.or.vec(nr = length(vec), nc = ncol(BRT_ts))
colnames(frontier.weights) <- colnames(BRT_ts)

for(i in 1:length(vec)){
  eff.port <- add.constraint(port, type = "return", name = "mean", return_target = vec[i])
  eff.port <- add.objective(eff.port, type = "risk", name = "var")
  eff.port <- optimize.portfolio(BRT_ts, eff.port, optimize_method = "ROI")
  eff.frontier$Risk[i] <- sqrt(t(eff.port$weights) %*% covMat %*% eff.port$weights)
  eff.frontier$Return[i] <- eff.port$weights %*% meanReturns
  eff.frontier$Sharperatio[i] <- eff.port$Return[i] / eff.port$Risk[i]
  frontier.weights[i,] = eff.port$weights
  print(paste(round(i/length(vec) * 100, 0), "% done..."))
}
feasible.sd <- apply(rportfolios, 1, function(x){
  return(sqrt(matrix(x, nrow = 1) %*% covMat %*% matrix(x, ncol = 1)))
})

feasible.means <- apply(rportfolios, 1, function(x){
  return(x %*% meanReturns)
})

feasible.sr <- feasible.means / feasible.sd

###############################
# USE summary()
# USE extractStats()

# Plots random feasible portfolios 
plot(x=feasible.sd, y=feasible.means, col="gray", main="Random Portfolio Methods", ylab="mean", xlab="StdDev")