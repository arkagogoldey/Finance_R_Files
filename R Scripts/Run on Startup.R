## Open and Run on Startup
setwd("~/GitHub/Finance_R_Files/R Scripts")
getwd()
# Time and Dates: Sorting and Formmating in R with 

# Intro to Time Series Data in R
#http://neondataskills.org/R/brief-tabular-time-series-qplot/

# How to sort a data frame by date in R
#http://stackoverflow.com/questions/6246159/how-to-sort-a-data-frame-by-date-in-r

# Rearranging/Recognizing Date Variables in R
#http://stackoverflow.com/questions/13906469/rearranging-recognizing-date-variable-in-r



# https://stat.ethz.ch/R-manual/R-devel/library/utils/html/write.table.html
write.table(x, file = "", append = FALSE, quote = TRUE, sep = " ",eol = "\n", na = "NA", dec = ".", row.names = TRUE,col.names = TRUE, qmethod = c("escape", "double"),fileEncoding = "")
#
write.csv(x, file = "Samplefile")
#
#https://stat.ethz.ch/R-manual/R-devel/library/base/html/write.html
write(x, file = "data",ncolumns = if(is.character(x)) 1 else 5,append = FALSE, sep = " ")

#Use the Import Dataset button in the Environment Tab in the Top right window of the R Studio Program
# or use the following commonds, substituting the file path and file names from "Sampleimport":
library(readxl)
Sampleimport <- read_excel("~/GitHub/Finance_R_Files/R Scripts/Sampleimport.xlsx")
View(Sampleimport)
print(Sampleimport)

# R Data Import/Export
#  Table of Contents
#https://cran.r-project.org/doc/manuals/r-release/R-data.html
#
#Downloading xlsreadwrite package
#https://github.com/swissr/xlsreadwrite
#
#
#http://www.statmethods.net/input/exportingdata.html
library(xlsx)
write.xlsx(mydata, "c:/mydata.xlsx")