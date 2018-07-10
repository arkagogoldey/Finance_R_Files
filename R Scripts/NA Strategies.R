## Open and Run on Startup
setwd("~/GitHub/Finance_R_Files/R Scripts")
getwd()


################################################################################################################
################################################################################################################
#################################################### Part 0 ####################################################
################################################# Excel Upload #################################################
################################################################################################################
################################################################################################################

##Upload Excel Benchamrk Tables
## BRT short for Bench Return Table
## read.excel formula is executed utilizing the Rstudio GUI ##
## I'll want to review the table data and designate the data types of each column.
library(readxl)
ConsBenchRetsEQLDates <- read_excel("C:/Users/Micha/Downloads/ConsBenchRetsEQLDates.xlsx", 
                                    col_types = c("date", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric"))
BRT <- ConsBenchRetsEQLDates
View(BRT)
print(BRT[1,])

################################################################################################################
################################################################################################################
#################################################### Part 1 ####################################################
######################### Calculating Std Deviation for Standard benchmark Allocations #########################
################################################################################################################
################################################################################################################

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
Risk25sd <- sqrt(var(Risk25))*sqrt(12)
Risk35sd <- sqrt(var(Risk35))*sqrt(12)
Risk45sd <- sqrt(var(Risk45))*sqrt(12)
Risk55sd <- sqrt(var(Risk55))*sqrt(12)
Risk65sd <- sqrt(var(Risk65))*sqrt(12)
Risk75sd <- sqrt(var(Risk75))*sqrt(12)
Risk85sd <- sqrt(var(Risk85))*sqrt(12)

### Now I have (incorrect, but functional) Annualized Std Deviation
print(Risk25sd)
print(Risk35sd)
print(Risk45sd)
print(Risk55sd)
print(Risk65sd)
print(Risk75sd)
print(Risk85sd)

################################################################################################################
################################################################################################################
#################################################### Part 2 ####################################################
########################### Running the Optimizer with Orion Exported data in Excel ############################
################################################################################################################
################################################################################################################

## These Standard Deviations have been used to equate the Risk NUmber to an Annualized Std Deviation
## Then I used this optimizor to maximize return for the given Risk number usign the Benchamrks listed
## ie. GSCI, USHY, USAGG, USLC, USMC, USSC, EAFE, EM, USRE, CASH, INTLFI

## Get the Optimizer functions and Add them here Incl. Libraries Required

library(PortfolioAnalytics)
library(quantmod)
library(PerformanceAnalytics)
library(zoo)
library(plotly)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)

####### THE Data below is for monthly data
# http://www.statmethods.net/advstats/timeseries.html
# Check the Dates before utilising
BRT_ts <- ts(BRT[,-1], start=c(2000, 2), end=c(2017, 8), frequency=12)

#ie. Isolating Benchmarks from Table
GSCI <- (BRT_ts[,1])
USHY <- (BRT_ts[,2])
USAGG <- (BRT_ts[,3])
USLC <- (BRT_ts[,4])
USMC <- (BRT_ts[,5])
USSC <- (BRT_ts[,6])
EAFE <- (BRT_ts[,7])
EM <- (BRT_ts[,8])
USRE <- (BRT_ts[,9])
CASH <- (BRT_ts[,10])
INTLFI <- (BRT_ts[,11])

## Starting Code of Optimizer ###
meanReturns <- colMeans(BRT_ts)
covMat <- cov(BRT_ts)
port <- portfolio.spec(assets = c("GSCI", "USHY", "USAGG", "USLC", "USMC", "USSC", "EAFE", "EM", "USRE", "CASH", "INTLFI"))

################################################################################################
################################################################################################
############################## Adjust the Constraints if neccesary #############################
################################################################################################
# https://cran.r-project.org/web/packages/PortfolioAnalytics/PortfolioAnalytics.pdf
# https://cran.r-project.org/web/packages/PortfolioAnalytics/vignettes/portfolio_vignette.pdf
################################################################################################


##### Create Max Return Markowitz portfolios to max return for a given risk level.

### Careful to consider the return period length and its impact on minret
#### The data is based on monthly returns
############ TO determine the appropriate allocation for each risk level,
############ use the monthly std dev for each annual portfolio target, then use that allocation.
## Reference Available: https://www.portfoliovisualizer.com/optimize-portfolio
############ RIsk Budgets for ETL are also based around monthly return volatility

## Start Here
port <- add.constraint(portfolio = port, type = "full_investment")
port <- add.constraint(port, type = "box", 
                       min=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                       max=c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))

####### c(GSCI, USHY, USAGG, USLC, USMC, USSC, EAFE, EM, USRE, CASH, INTLFI)

## TARGETED RISK (Risk Budget) ##
## Use the target std targets and compare each equivalent level to the ETL target to transfer risk targets
# page 30 Portfolio Analytics VIgnette #
### SEE IF THE TWO Objectives need to be added previous to this objective
rb_meanETL <- add.objective(portfolio=port, type='risk',name='ETL', arguments=list(p=0.95))
opt_rb_meanETL <- optimize.portfolio(R=BRT_ts, portfolio=rb_meanETL,
                                     optimize_method="random",
                                     search_size=5000,
                                     trace=TRUE, traceDE=5)

summary(opt_rb_meanETL)
extractStats(opt_rb_meanETL)

############# (GSCI, USHY, USAGG, USLC, USMC, USSC, EAFE,  EM,  USRE, CASH, INTLFI)
weights85 <- c(0.00, 0.05,  0.00, 0.25, 0.20, 0.20, 0.15, 0.10, 0.03, 0.02, 0.00)
weights75 <- c(0.00, 0.05,  0.05, 0.20, 0.16, 0.17, 0.15, 0.10, 0.05, 0.02, 0.05)
weights65 <- c(0.00, 0.10,  0.10, 0.18, 0.13, 0.13, 0.13, 0.09, 0.07, 0.02, 0.05)
weights55 <- c(0.00, 0.10,  0.20, 0.15, 0.07, 0.08, 0.12, 0.08, 0.07, 0.03, 0.10)
weights45 <- c(0.00, 0.10,  0.35, 0.12, 0.06, 0.07, 0.10, 0.05, 0.06, 0.04, 0.05)
weights35 <- c(0.00, 0.10,  0.45, 0.10, 0.00, 0.05, 0.10, 0.05, 0.05, 0.05, 0.05)
weights25 <- c(0.00, 0.10,  0.60, 0.08, 0.00, 0.00, 0.07, 0.00, 0.00, 0.05, 0.10)

Ann.var85 <- (sqrt(12)*(StdDev(BRT_ts, weights=weights85)))
Ann.var75 <- (sqrt(12)*(StdDev(BRT_ts, weights=weights75)))
Ann.var65 <- (sqrt(12)*(StdDev(BRT_ts, weights=weights65)))
Ann.var55 <- (sqrt(12)*(StdDev(BRT_ts, weights=weights55)))
Ann.var45 <- (sqrt(12)*(StdDev(BRT_ts, weights=weights45)))
Ann.var35 <- (sqrt(12)*(StdDev(BRT_ts, weights=weights35)))
Ann.var25 <- (sqrt(12)*(StdDev(BRT_ts, weights=weights25)))

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
######################################## Building an Efficient Frontier ########################################
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