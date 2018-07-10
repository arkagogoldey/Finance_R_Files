# Ruey Tsay - Analysis of Financial Time Series
# Chapter 1
# Page 12
library(fBasics)
da=read.table("d-ibm3dx7008", header=T)
# header=T means 1st row of the data file contains variable names. The default is "F", or no names
dim(da)
# Find size of the data for object 'da'
da[1,]
# See the first row of the data
ibm = da[,2]
