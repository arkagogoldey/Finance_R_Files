#https://onlinecourses.science.psu.edu/stat510/node/79
library(vars)
library(astsa)
x = cbind(cmort, tempr, part)
plot.ts(x , main = "", xlab = "")
summary(VAR(x, p=1, type="both"))
#The ASTSA package is the Applied Time Series Analisis Package. I beleive its from Penn State.
#To use different variables in the model, replace "cmort", "tempr", "part"
#Residuals are also available for analysis. 
#For example, if we assign the VAR command to an object titled fitvar2 in our program,
fitvar2 = VAR(x, p=2, type="both")
acf(residuals(fitvar2)[,1])