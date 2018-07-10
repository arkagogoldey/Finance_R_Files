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
####################################################################################################################################################
# GOAL 1: (Method 2 SUCCESS- SEE BELOW)
# Create a simple 2 holding portfolio model
# There are two ways to calculate portfolio returns from holdings returns:
#   1) calculate the return matrix normally, then convert to XTS
#   2) Convert the holdings returns to XTS, then use Return.portfolio()
#   The 2 Holdings are SPY and EEM and I will use the YTD Period
# GOAL 2:
#  I will need to calculate holdings returns from holdings prices
####################################################################################################################################################
####################################################################################################################################################
################################################# GOAL 1 ###########################################################################################
####################################################################################################################################################
####################################################################################################################################################

####################################################################################################################################################
# METHOD 1) calculate the return matrix normally, then convert to XTS
####################################################################################################################################################


####################################################################################################################################################
# METHOD 2) Convert the holdings returns to XTS, then use Return.portfolio()
####################################################################################################################################################

# I may need to add na.omit intermittently 

############# Step 1 #############
### Import Holdings Returns (This uses daily price returns and is suitable for short term return analysis)
Hrets <- read_excel("C:/Users/Micha/Downloads/Test_Returns.xlsx")
head(Hrets)

############# Step 2 #############
### Convert Hrets to xts

# Change Column Names (MAY NOT BE NECCESARY)
colnames(Hrets) <- c("DATE","EEM","SPY")
## CREATING A DF MAY NOT BE NECCESARY
Hrets.df <- data.frame(Hrets)
# column names must equal the below $CLASS names
H <- xts(Hrets[-1], Hrets.df$DATE)

############# Step 3 #############
## Add Portfolio Weights for EEM & SPY
w <- c("0.5","0.5")
w <- as.numeric(w)

############# Step 4 #############
## Calculate Portfolio Returns vis Return.portfolio()
# Default
Pret <- Return.portfolio(H, weights = w, wealth.index = FALSE, contribution = FALSE, geometric = TRUE, 
                 rebalance_on = c(NA), value = 1, verbose = FALSE)
#### Goal 1, Method 2 completed
######### WORKED on 3/23/2018 @ 8:53pm ########################################################################################

####################################################################################################################################################
####################################################################################################################################################
## Write to Excel
# https://www.statmethods.net/input/exportingdata.html
library(xlsx)
write.xlsx(mydata, "c:/mydata.xlsx")

####################################################################################################################################################
####################################################################################################################################################
################################################# GOAL 2 ###########################################################################################
####################################################################################################################################################
####################################################################################################################################################

### Import Holdings Prices
TRPrices <- read_excel("C:/Users/Micha/Downloads/m_total_return_price_data.xlsx")
BRT <- TRPrices
head(BRT)
## Non date colums show up as <dbl> 


### I don't think we need a dataframe
BRT.df <- as.data.frame(BRT)

#################### Calculate Holdings Returns from Total Return Prices ################
#https://quant.stackexchange.com/questions/2909/calculating-log-returns-using-r

lret <- diff(log(BRT[,-1]), lag(1))
## Error in if (length(lag) != 1L || length(differences) > 1L || lag < 1L ||  : 
#      missing value where TRUE/FALSE needed

####################################################################################################################################################
####################################################################################################################################################
####################################################################################################################################################
# Reset this example for a simple portfolio that has a 0 % allocation, a matirx of allocations and multiple returns
# Use SPY EEM AGG with allocation 1 of 0.50, 0.00, and 0.50
# for the second allocation use 0.50, 0.25, and 0.25 
####################################################################################################################################################
####################################################################################################################################################
####################################################################################################################################################
############# Test Model  for Importing an excel sheet with multiple allocations #############
####################################################################################################################################################

############# Step 1 #############
### Import Holdings Returns (This uses daily price returns and is suitable for short term return analysis)
TestR <- read_excel("C:/Users/Micha/Downloads/Test_Returns2.xlsx")
head(TestR)

############# Step 2 #############
### Convert Returns to xts
TestR.df <- (TestR)
# column names must equal the below $CLASS names
R <- xts(TestR[-1], TestR.df$Period)

### MAY NEED TO MULTIPLY BY 100 TO TURN VALUES INTO A PERCENT

############# Step 3 #############
weights <- read_excel("C:/Users/Micha/Downloads/Test_Alloc.xlsx")
str(weights)
# Check if Numeric, if not execute below
weights <- as.numeric(weights)
# Remove Row name & Select which Model Allocation via the Row
testw1 <- weights[1,-1]
testw2 <- weights[2,-1]
w1  <- c("0.5","0","0.5")
w2  <- c("0.5","0.25","0.25")
w1 <- as.numeric(w1)
w2 <- as.numeric(w2)
### Testw1 and Testw2 failed
# W1 and w2 were successful
## Converting the data into a matrix was Successful
testw <- as.matrix(weights[,-1])
str(testw)
testw1a <- testw[1,]

# Testing Matrix conversion for testw1 and testw2
testw1 <- testw[1,]
testw2 <- testw[2,]


####################################################################################################################################################
####################################################################################################################################################

### LOOKS LIKE THERE IS A PROBLEM WITH THE WEIGHTS....
### Class names are different in Returns and Weights, maybe that is a contributing factor 

### Error in checkData(weights, method = "xts") : 
#      The data cannot be converted into a time series.  If you are trying to pass in names 
#      from a data object with one column, you should use the form 'data[rows, columns, drop = FALSE]'. 
#      Rownames should have standard date formats, such as '1985-03-15'. 
#  In addition: Warning message:
#      In if (class(x) == "numeric") { : the condition has length > 1 and only the first element will be used

# Look into the data error


############# Step 4 #############
## Calculate Portfolio Returns vis Return.portfolio()
# Pret1 Includes zero value holdings <FAILED>
Pret1 <- Return.portfolio(R, weights = testw1, wealth.index = FALSE, contribution = FALSE, geometric = TRUE, 
                          rebalance_on = c(NA), value = 1, verbose = FALSE)

# Pret2 Includes all allocated holdings <FAILED>
Pret2 <- Return.portfolio(R, weights = testw2, wealth.index = FALSE, contribution = FALSE, geometric = TRUE, 
                          rebalance_on = c(NA), value = 1, verbose = FALSE)

# Manual weight 1 including a zero value holding <SUCCESS>
Pret1m <- Return.portfolio(R, weights = w1, wealth.index = FALSE, contribution = FALSE, geometric = TRUE, 
                           rebalance_on = c(NA), value = 1, verbose = FALSE)
# Manual weight 2 including all allocated holdings <SUCCESS>
Pret2m <- Return.portfolio(R, weights = w2, wealth.index = FALSE, contribution = FALSE, geometric = TRUE, 
                           rebalance_on = c(NA), value = 1, verbose = FALSE)
# Test Vector using "Testw1a" 
testPret <- Return.portfolio(R, weights = testw1a, wealth.index = FALSE, contribution = FALSE, geometric = TRUE, 
                             rebalance_on = c(NA), value = 1, verbose = FALSE)


####################################################################################################################################################
####################################################################################################################################################
########################################## Previous Code ###########################################################################################
####################################################################################################################################################
####################################################################################################################################################
####################################################################################################################################################
### Import Holdings Prices
TRPrices <- read_excel("C:/Users/Micha/Downloads/m_total_return_price_data.xlsx")
BRT <- TRPrices
head(BRT)
BRT.df <- as.data.frame(BRT)

### Import Model Allocation of Above Holdings (Use Import Dataset in Rstudio if struggling)
Modelalloc <- read_excel("C:/Users/Micha/Downloads/Model Alloc Import.xlsx")


### Import Holdings Returns
ModelTR <- read_excel("C:/Users/Micha/Downloads/Test_Returns.xlsx")
head(ModelTR)
# Split Dates and Returns
R <- ModelTR[,-1]
head(R)
Dates <- ModelTR[,1]
head(Dates)

# Convert R Into an XTS object (Time Series)
# column names must equal the below $CLASS names
BRTdone <- xts(BRT.df$DATE,BRT.df$SWSXX,BRT.df$LALD)
### Import Holdings Prices
TRPrices <- read_excel("C:/Users/Micha/Downloads/m_total_return_price_data.xlsx")
BRT <- TRPrices
head(BRT)
BRT.df <- as.data.frame(BRT)

### Import Model Allocation of Above Holdings (Use Import Dataset in Rstudio if struggling)
Modelalloc <- read_excel("C:/Users/Micha/Downloads/Model Alloc Import.xlsx")


### Import Holdings Returns
ModelTR <- read_excel("C:/Users/Micha/Downloads/ModelTR.xlsx")
head(ModelTR)
# Split Dates and Returns
R <- ModelTR[,-1]
head(R)
Dates <- ModelTR[,1]
head(Dates)

# Convert R Into an XTS object (Time Series)
# column names must equal the below $CLASS names
BRTdone <- xts(BRT.df$DATE,BRT.df$SWSXX,BRT.df$LALDX,BRT.df$DODIX,BRT.df$ONIAX,BRT.df$PONDX,BRT.df$ANGLX,
               BRT.df$AFLEX,BRT.df$FLTDX,BRT.df$GSMIX,BRT.df$FRCZX,BRT.df$VKLMX,BRT.df$MMHAX,BRT.df$VKLMX,
               BRT.df$MMHAX,BRT.df$LHYAX,BRT.df$LFRAX,BRT.df$TPINX,BRT.df$DODGX,BRT.df$GTLLX,BRT.df$POAGX,
               BRT.df$PARMX,BRT.df$WFPAX,BRT.df$JTUAX,BRT.df$BCSIX,BRT.df$OAKIX,BRT.df$AEGFX,BRT.df$OSMAX,
               BRT.df$OSMAx,BRT.df$ODMAX,BRT.df$BEXFX,BRT.df$CSEIX,BRT.df$GIREX,BRT.df$SGOL,BRT.df$PCYAX)



##############################################################################################################
##############################################################################################################
#################### Calculate Portfolio Weights ################
### Import Model Allocation Table
Modelalloc <- read_excel("C:/Users/Micha/Downloads/Model Alloc Import.xlsx")
# Newport 85 Weights with strategy names removed
W85 <- Modelalloc[1,-1]


##############################################################################################################
##############################################################################################################
#################### Calculate Portfolio Returns ################
## To Use Return.portfolio(), we need an XTS for the daily returns of the holdings
# https://www.rdocumentation.org/packages/PerformanceAnalytics/versions/1.4.3541/topics/Return.portfolio
# Default
Return.portfolio(R, weights = NULL, wealth.index = FALSE, contribution = FALSE, geometric = TRUE, 
                 rebalance_on = c(NA, "years", "quarters", "months", "weeks", "days"), 
                 value = 1, verbose = FALSE, ...)
## TESTING for Newport 85
Return.portfolio(R, weights = W85, wealth.index = FALSE, contribution = FALSE, geometric = TRUE, 
                 rebalance_on = NA, 
                 value = 1, verbose = FALSE)
## FAIL because R was not in a tiem series



##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
########## Calculating Portfolio Return Manually #############
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
### Import Holdings Prices
TRPrices <- read_excel("C:/Users/Micha/Downloads/m_total_return_price_data.xlsx")
BRT <- TRPrices
head(BRT)
BRT.df <- as.data.frame(BRT)

#################### Calculate Holdings Returns from Total Return Prices ################
#https://quant.stackexchange.com/questions/2909/calculating-log-returns-using-r
### LIKELY SOLUTION ###
prices <- data$cl
log_returns <- diff(log(prices), lag(1))

### Import Model Allocation Table
Modelalloc <- read_excel("C:/Users/Micha/Downloads/Model Alloc Import.xlsx")

### Execute for each Model
N85alloc <- Modelalloc[1,]
N85 <- N85alloc[,-1]*BRT.df[,-1]
### Calculates model return but removes the date from the BRT dataframe
### I must add the date back in, or conversely, seprate the date of BRt into a separate array, then combine via C() together afterwards
# Check that the calculation works this way in excel as well

### Test of Broken function above
N85A <- N85alloc[,-1]
BRTclean <- BRT.df[,-1]

## Creates a Matrix of rows
N85M <- matrix(N85A,nrow=826,ncol=length(N85A),byrow=TRUE)
### maybe make Nrow  equal to Nrow-1 of BRT


### Then sum all coulmns for each row, and create into a 1 column df with dates


# Do for each strategy
colnames(BRT.df) <- c("DATE","CLOSE")
## Check to make sure headers below are not backwards
BRTdone <- xts(BRT.df$DATE,BRT.df$CLOSE)



##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
## Write to Excel
# https://www.statmethods.net/input/exportingdata.html
library(xlsx)
write.xlsx(mydata, "c:/mydata.xlsx")


#################### Other Code Under Development ############################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################

## With my column names
colnames(BRT.df) <- c("DATE","SWSXX","LALDX","DODIX","ONIAX","PONDX","ANGLX","AFLEX","FLTDX","GSMIX","FRCZX","VKLMX",
                      "MMHAX","LHYAX","LFRAX","TPINX","DODGX","GTLLX","POAGX","PARMX","WFPAX","JTUAX","BCSIX","OAKIX",
                      "AEGFX","OSMAX","ODMAX","BEXFX","CSEIX","GIREX","SGOL","PCYAX")

#### See if this can be shortened such that only Period is replaced, or maybe not even replace it

#### See if this can be shortened in some way to call all classes insteaad of manually adding them individually
BRTdone <- xts(BRT.df$DATE,BRT.df$SWSXX,BRT.df$LALDX,BRT.df$DODIX,BRT.df$ONIAX,BRT.df$PONDX,BRT.df$ANGLX,
               BRT.df$AFLEX,BRT.df$FLTDX,BRT.df$GSMIX,BRT.df$FRCZX,BRT.df$VKLMX,BRT.df$MMHAX,BRT.df$VKLMX,
               BRT.df$MMHAX,BRT.df$LHYAX,BRT.df$LFRAX,BRT.df$TPINX,BRT.df$DODGX,BRT.df$GTLLX,BRT.df$POAGX,
               BRT.df$PARMX,BRT.df$WFPAX,BRT.df$JTUAX,BRT.df$BCSIX,BRT.df$OAKIX,BRT.df$AEGFX,BRT.df$OSMAX,
               BRT.df$OSMAx,BRT.df$ODMAX,BRT.df$BEXFX,BRT.df$CSEIX,BRT.df$GIREX,BRT.df$SGOL,BRT.df$PCYAX)