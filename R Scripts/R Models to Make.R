
###############################IMPORTING AND EXPORTING FILES FROM R##################
read.csv("nameoffile.csv") #importing or read.table()
write.csv(variablename, "filename.csv", sep = "\t", rownames = FALSE) #exporting files- "\t" is tab delim & "," is for Comma delim

####################TRANSFORM PRICE INTO RETURN######################################
https://cran.r-project.org/web/packages/zoo/vignettes/zoo-quickref.pdf
http://people.duke.edu/~rnau/411log.htm
https://quantivity.wordpress.com/2011/02/21/why-log-returns/

#To compute log-difference returns in %, the following convenience function is defined:
> prices2returns <- function(x) 100*diff(log(x))

#which can be used to convert all columns (of prices) into returns.
> r <- prices2returns(m)

#################PROPERLY FORMATTING TIME SERIES PARAMETERS##############################
as.Date(12/13/2016, format = "%d/%b/%Y")
# %y is year w/o century and %Y is year with century (ie.15 vs 2015)
# %m is month as decimal number (01-12)
# %B Full month name in current locale & %b is abbreviated month name in current locale
# %d Day of the months as a decimal number (01-31)
# %A Full weekday name in current locale & %a is abbreviated weekday in current locale
# %w Weekday as decimal number (0-6, with Sunday being 0)

#################Dimensions of Data#########

nrow(baskets.df) and length(baskets.df)

src() #can be used to better understand the matrix or array

#########################GET DATA FEED#########################################
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

getSymbols.yahoo("AAPL",
                 env=.GlobalEnv,
                 return.class = 'xts',
                 from = "2007-01-01",
                 to = Sys.Date(),)

##############################################################################
# Basic Things to Model

#Correlation of Returns across holdings and the entire portfolio
#BARRA Model
#GARCh Model
#VAR modeling for Covariance Matrix
#Estiamted future vol of portfolio (weights * Covar matrix)* Returns

SPYday1 <- (SPYytd[,2]/(lag(SPYytd[,2],-1))-1)