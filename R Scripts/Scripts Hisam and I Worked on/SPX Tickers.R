##S&P 500 tickers/sectors

library(xlsx)
SPX<-read.xlsx("/Users/hisamsabouni2/Desktop/R/Stock Data/SP500.xlsx",sheetName="Sheet1",header=TRUE)
colnames(SPX)<-c("Symbol","Security","Sector","Sub.Sector")
colnames(SPX)
class(SPX$Symbol)
SPX$Symbol<-as.character(SPX$Symbol)
library(quantmod)
getSymbols("GOOG",from='2015-01-01')[,6]

head(GOOG)