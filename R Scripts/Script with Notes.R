#Notes to Remember
c()
#Construct a vector pg 64
# Matrices pg 120~

#Importing Via CSV
nameOfDataFrame <- read.csv("nameOfCSVFile.CSV")
str(nameOfDataFrame)

#Exporting fromR Via CSV File- Only Works with RforDummies package
save("elements.xlsx")

# other import techniques: read.table() or read.delim()

# XLconnect is a great package to write excel files
install.packages("XLConnect")
library("XLConnect")
#Which Packages are must have on Rstudio Startup?
#Or, better question, write in for each script the neccesary packages with each librabry() command
#+ also add a not saying which are required so I can look also in the GUI and quickly check

#Packages to select
# PerformanceAnalytics
# zoo
# xts
# stats
# 