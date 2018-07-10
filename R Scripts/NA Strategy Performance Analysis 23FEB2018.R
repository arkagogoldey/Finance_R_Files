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

### Import Holdings Prices
TRPrices <- read_excel("C:/Users/Micha/Downloads/m_total_return_price_data.xlsx")
BRT <- TRPrices
View(BRT)
print(BRT[1,])
BRT.df <- as.data.frame(BRT)
colnames(BRT.df) <- c("DATE","CLOSE")
BRTdone <- xts(BRT.df$CLOSE,BRT.df$DATE)
##############################################################################################################
############## Performance Calculations from Prices Should Occur Here or in the Y Charts Spreadsheet
### Import Model Allocation Table
Modelalloc <- read_excel("C:/Users/Micha/Downloads/Model Alloc Import.xlsx")

### THis is multiplying a row in the Modelalloc Matrix against the matrix for price/return
### Then we'll need to transfer and rename the performance for each of the model allocations for 85, 75, etc
N85alloc <- Modelalloc[1,]
N85 <- N85alloc[,-1]*BRT.df[,-1]
### I removed the first column in each of the two DFs, leaving only the values in the dataframes.

## Write to Excel
# https://www.statmethods.net/input/exportingdata.html
library(xlsx)
write.xlsx(mydata, "c:/mydata.xlsx")


#################### Code Under Development ##################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
## Open and Run on Startup
setwd("~/GitHub/Finance_R_Files/R Scripts")
getwd()

library(readxl)
##### DOn't Add ".Xlsx" to the file name, or you will have to duplicate it in the file path
SampleTRPrices <- read_excel("C:/Users/Micha/Downloads/example.xlsx", 
                                    col_types = c("date", "numeric"))
BRT <- SampleTRPrices
View(BRT)
print(BRT[1,])

library(PortfolioAnalytics)
library(quantmod)
library(PerformanceAnalytics)
library(zoo)
library(plotly)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)

#### Need to turn a tibble into a dataframe
BRT.df <- as.data.frame(BRT)

colnames(BRT)
## Returns columns for all funds

### Old Function
colnames(BRT.df) <- c("DATE","SWSXX")


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
