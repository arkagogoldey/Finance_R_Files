#https://onlinecourses.science.psu.edu/stat510/node/79
install.packages("vars") #If not already installed
install.packages("astsa") #If not already installed

library(vars)
library(astsa)
## Cmort, tempr, and part are from the library that is taking an example from a text book
x = cbind(cmort, tempr, part)
plot.ts(x , main = "", xlab = "")
summary(VAR(x, p=1, type="both"))
#The ASTSA package is the Applied Time Series Analisis Package. I beleive its from Penn State.
#To use different variables in the model, replace "cmort", "tempr", "part"
#Residuals are also available for analysis. 
#For example, if we assign the VAR command to an object titled fitvar2 in our program,
fitvar2 = VAR(x, p=2, type="both")
acf(residuals(fitvar2)[,1])
acf(residuals(fitvar2)[,2])
acf(residuals(fitvar2)[,3])
acf(residuals(fitvar2))
## Pure Code

x = cbind(cmort, tempr, part)
plot.ts(x , main = "", xlab = "")
summary(VAR(x, p=1, type="both"))
fitvar2 = VAR(x, p=2, type="both")
acf(residuals(fitvar2)[,1])
acf(residuals(fitvar2)[,2])
acf(residuals(fitvar2)[,3])
acf(residuals(fitvar2))