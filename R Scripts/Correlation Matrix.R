# Correlation Matrix

### Oberserved Correlation Matrix
d <- data.frame(x1,x2,x3)
cor(d) # get correlations (returns matrix)

#####
http://stackoverflow.com/questions/10680658/how-can-i-create-a-correlation-matrix-in-r
## Estimated Correlation Matrix Plot ##
d <- data.frame(x1=rnorm(10),
                x2=rnorm(10),
                x3=rnorm(10))
M <- cor(d) # get correlations

library('corrplot') #package corrplot
corrplot(M, method = "circle") #plot matrix


### Another source and method ##
http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
