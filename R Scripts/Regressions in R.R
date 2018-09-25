## Regression Tutorial in R
# https://www.princeton.edu/~otorres/Regression101R.pdf
# Review of Regression Analysis

#######Useful Links:
# DSS Online Training Section http://dss.princeton.edu/training/
# Princeton DSS Libguides http://libguides.princeton.edu/dss
# John Fox's site http://socserv.mcmaster.ca/jfox/
# Quick-R http://www.statmethods.net/
# UCLA Resources to learn and use R http://www.ats.ucla.edu/stat/R/
# UCLA Resources to learn and use Stata http://www.ats.ucla.edu/stat/stata/
# DSS - Stata http://dss/online_help/stats_packages/stata/
# DSS - R http://dss.princeton.edu/online_help/stats_packages/r
# Regression Significance http://blog.minitab.com/blog/statistics-and-quality-data-analysis/what-are-t-values-and-p-values-in-statistics 
# P and T Stats http://statisticsbyjim.com/regression/interpret-coefficients-p-values-regression/

library(car)
# If not already installed, then input
install.packages("car")
# Type help(Prestige) to access the codebook

###############################################################################
# Basic Linear Regression
reg1 <- lm(prestige ~ education + log2(income) + women, data=Prestige)
summary(reg1)

# Linear regression (heteroskedasticity-robust standard errors) 
library(lmtest)
library(sandwich)
reg1$robse <- vcovHC(reg1, type="HC1")
coeftest(reg1,reg1$robse) 

#Predicted Values/Residuals (Once Regression is Run)
prestige_hat <- fitted(reg1) # predicted values
as.data.frame(prestige_hat)
Prestige_resid <- residuals(reg1) #residuals
as.data.frame(prestige_resid)


###############################################################################
# Dummy regression with no interactions (analysis of covariance, fixed effects)
reg2 <- lm(prestige ~ education + log2(income) +
             type, data = Prestige)
summary(reg2)

# Reordering factor variables
Prestige$type <- with(Prestige, factor(type,
                                       levels=c("bc", "wc", "prof")))

###############################################################################
# Diagnostics for linear regression (residual plots, see next page for the graph)
library(car)
reg1 <- lm(prestige ~ education + income + type,
           data = Prestige)
residualPlots(reg1)

# Using 'income' as is.
# Variable 'income' shows some patterns.
# Other options:
residualPlots(reg1, ~ 1, fitted=TRUE) #Residuals vs fitted only
residualPlots(reg1, ~ education, fitted=FALSE) # Residuals vs education only

###############################################################################
# Influential variables - Added-variable plots (see next page for the graph) 
library(car)
reg1 <- lm(prestige ~ education + income + type, data = Prestige)
avPlots(reg1, id.n=2, id.cex=0.7)
# id.n - id most influential observation
# id.cex - font size for id.

###############################################################################
# Outliers - QQ-Plots
library(car)
reg1 <- lm(prestige ~ education + income + type, data = Prestige)
qqPlot(reg1, id.n=3)
# id.n - id observations with high residuals

###############################################################################
# Outliers - Bonferonni test 
library(car)
reg1 <- lm(prestige ~ education + income + type, data = Prestige)
outlierTest(reg1)
# Null for the Bonferonni adjusted outlier test is the observation is an outlier.
#   Here observation related to 'medical.technicians' is an outlier.

###############################################################################
# High leverage (hat) points (graph next page)
library(car)
reg1 <- lm(prestige ~ education + income + type, data = Prestige)
influenceIndexPlot(reg1, id.n=3)
# Cook's distance measures how much an observation influences the overall model or predicted values
# Studentizided residuals are the residuals divided by their estimated standard deviation as a way to standardized
# Bonferroni test to identify outliers
# Hat-points identify influential observations (have a high impact on the predictor variables)

# NOTE: If an observation is an outlier and influential (high leverage) then that observation can change the fit
# of the linear model, it is advisable to remove it. To remove a case(s) type
reg1a <- update(prestige.reg4, subset=rownames(Prestige) != "general.managers")
reg1b <- update(prestige.reg4, subset= !(rownames(Prestige) %in% c("general.managers","medical.technicians")))

###############################################################################
# Influence Plots
library(car)
reg1 <- lm(prestige ~ education + income + type, data = Prestige)
influencePlot(reg1, id.n=3)
# Creates a bubble-plot combining the display of Studentized residuals, hat-values, and Cook's
# distance (represented in the circles).

###############################################################################
# Testing for normality

library(car)
reg1 <- lm(prestige ~ education + income + type, data = Prestige)
qqPlot(reg1)
# Look for the tails, points should be close to the line or within the confidence intervals.
# Quantile plots compare the Studentized residuals vs a t-distribution
# Other tests: shapiro.test(), mshapiro.test() in library(mvnormtest)-library(ts)

###############################################################################
# Testing for heteroskedasticity
library(car)
reg1 <- lm(prestige ~ education + income + type, data = Prestige)
ncvTest(reg1)

# Breush/Pagan and Cook/Weisberg score test for non-constant error variance. Null is constant variance
# See also residualPlots(reg1).

###############################################################################
# Testing for multicolinearity

library(car)
reg1 <- lm(prestige ~ education + income + type, data = Prestige)
vif(reg1)

# A gvif> 4 suggests collinearity.
# "When there are strong linear relationships among the predictors in a regression analysis, the
# precision of the estimated regression coefficients in linear models declines compared to what it
# would have been were the predictors uncorrelated with each other" (Fox:359)

###############################################################################
# Linear regression (cluster-robust standard errors) 
library(car)
library(lmtest)
library(multiwayvcov)
# Need to remove missing before clustering
p = na.omit(Prestige)
# Regular regression using lm()
reg1 = lm(prestige ~ education + log2(income)
          + women, data = p)
# Cluster standard errors by 'type'
reg1$clse <-cluster.vcov(reg1, p$type)
coeftest(reg1, reg1$clse) 



###############################################################################
# EXAMPLE
IUSRSG_data <- read_excel("C:/Users/Michael/Downloads/IUSRSG_data.xlsx", col_types = c("date", "numeric", "numeric"))
Test2 <- lm(US_Retail_Sales ~ ISM_Mfg, data=Data)
summary(Test2)

#Success!
# Data taken from colum heads of the data frame

#Example of Return Components
PIMIX_TR <- read_excel("C:/Users/Michael/Downloads/PIMIX TR.xlsx")
lmPIMIX <- lm(PIMIX ~ SPY + AGG + AGZ + LQD + HYG + EMB, data=PIMIX_TR)
summary(lmPIMIX)

