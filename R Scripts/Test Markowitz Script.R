####################################################################################################################
#Shortened Code Without Notes
library(PortfolioAnalytics)
library(quantmod)
library(PerformanceAnalytics)
library(zoo)
library(plotly)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)
getSymbols(c("SPY", "IWM", "EEM", "EFA", "GDX", "IEF"))
prices.data <- merge.zoo(SPY[,6], IWM[,6], EEM[,6], EFA[,6], GDX[,6], IEF[,6])
returns.data <- CalculateReturns(prices.data)
returns.data <- na.omit(returns.data)
colnames(returns.data) <- c("SPY", "IWM", "EEM", "EFA", "GDX", "IEF")
meanReturns <- colMeans(returns.data)
covMat <- cov(returns.data)
port <- portfolio.spec(assets = c("SPY", "IWM", "EEM", "EFA", "GDX", "IEF"))
port <- add.constraint(port, type = "box", min = 0.05, max = 0.8)
port <- add.constraint(portfolio = port, type = "full_investment")
rportfolios <- random_portfolios(port, permutations = 5000, rp_method = "sample")
minvar.port <- add.objective(port, type = "risk", name = "var")
minvar.opt <- optimize.portfolio(returns.data, minvar.port, optimize_method = "random", rp = rportfolios)
maxret.port <- add.objective(port, type = "return", name = "mean")
maxret.opt <- optimize.portfolio(returns.data, maxret.port, optimize_method = "random", rp = rportfolios)
minret <- 0.03/100
maxret <- maxret.opt$weights %*% meanReturns
vec <- seq(minret, maxret, length.out = 100)
eff.frontier <- data.frame(Risk = rep(NA, length(vec)),
                           Return = rep(NA, length(vec)), 
                           SharpeRatio = rep(NA, length(vec)))

frontier.weights <- mat.or.vec(nr = length(vec), nc = ncol(returns.data))
colnames(frontier.weights) <- colnames(returns.data)

for(i in 1:length(vec)){
  eff.port <- add.constraint(port, type = "return", name = "mean", return_target = vec[i])
  eff.port <- add.objective(eff.port, type = "risk", name = "var")
  eff.port <- optimize.portfolio(returns.data, eff.port, optimize_method = "ROI")
  eff.frontier$Risk[i] <- sqrt(t(eff.port$weights) %*% covMat %*% eff.port$weights)
  eff.frontier$Return[i] <- eff.port$weights %*% meanReturns
  eff.frontier$Sharperatio[i] <- eff.port$Return[i] / eff.port$Risk[i]
  frontier.weights[i,] = eff.port$weights
  print(paste(round(i/length(vec) * 100, 0), "% done..."))
}
feasible.sd <- apply(rportfolios, 1, function(x){
  return(sqrt(matrix(x, nrow = 1) %*% covMat %*% matrix(x, ncol = 1)))
})

feasible.means <- apply(rportfolios, 1, function(x){
  return(x %*% meanReturns)
})

feasible.sr <- feasible.means / feasible.sd


##### Create Max Return Markowitz portfolios to max return for a given risk level.

maxret.port <- add.objective(port, type = "return", name = "mean")
maxret.opt <- optimize.portfolio(returns.data, maxret.port, optimize_method = "random", rp = rportfolios)

maxvar <- 
  
  print(eff.port)
### Edited
rb_meanETL <- add.objective(port, type="return", name="mean")
rb_meanETL <- add.objective(portfolio=rb_meanETL, type="risk", name="ETL",
                            + arguments=list(p=0.95))
rb_meanETL <- add.objective(portfolio=rb_meanETL, type="risk_budget",
                            + name="ETL", max_prisk=0.4, arguments=list(p=0.95))
### Fixed
rb_meanETL <- add.objective(portfolio=init, type="return", name="mean")
rb_meanETL <- add.objective(portfolio=rb_meanETL, type="risk", name="ETL",
                            + arguments=list(p=0.95))
rb_meanETL <- add.objective(portfolio=rb_meanETL, type="risk_budget",
                            + name="ETL", max_prisk=0.4, arguments=list(p=0.95))

######## Plot is below #######

plot(x=tmp1.StdDev, y=tmp1.mean, col="gray", main="Random Portfolio Methods", ylab="mean", xlab="StdDev")
points(x=tmp2.StdDev, y=tmp2.mean, col="red", pch=2)
points(x=tmp3.StdDev, y=tmp3.mean, col="lightgreen", pch=5)
legend("bottomright", legend=c("sample", "simplex", "grid"),
       + col=c("gray", "red", "lightgreen"),
       + pch=c(1, 2, 5), bty="n")

########
p <- plot_ly(x = feasible.sd, y = feasible.means, color = feasible.sr, 
             mode = "markers", type = "scattergl", showlegend = F,
             
             marker = list(size = 3, opacity = 0.5, 
                           colorbar = list(title = "Sharpe Ratio"))) %>% 
  
  add_trace(data = eff.frontier, x = Risk, y = Return, mode = "markers", 
            type = "scattergl", showlegend = F, 
            marker = list(color = "#F7C873", size = 5)) %>% 
  
  layout(title = "Random Portfolios with Plotly",
         yaxis = list(title = "Mean Returns", tickformat = ".2%"),
         xaxis = list(title = "Standard Deviation", tickformat = ".2%"),
         plot_bgcolor = "#434343",
         paper_bgcolor = "#F8F8F8",
         annotations = list(
           list(x = 0.4, y = 0.75, 
                ax = -30, ay = -30, 
                text = "Efficient frontier", 
                font = list(color = "#F6E7C1", size = 15),
                arrowcolor = "white")
         ))

frontier.weights.melt <- reshape2::melt(frontier.weights)
q <- plot_ly(frontier.weights.melt, x = Var1, y = value, group = Var2, type = "bar") %>%
  layout(title = "Portfolio weights across frontier", barmode = "stack",
         xaxis = list(title = "Index"),
         yaxis = list(title = "Weights(%)", tickformat = ".0%"))
###################################################################################################################
#Calculates Monthly Returns



###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
#http://moderndata.plot.ly/portfolio-optimization-using-r-and-plotly/
library(PortfolioAnalytics)
library(quantmod)
library(PerformanceAnalytics)
library(zoo)
library(plotly)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)
# Explain what is being done with the code
# Get data / # This pulls the data from yachoo
getSymbols(c("SPY", "IWM", "EEM", "EFA", "GDX", "IEF"))

# Assign to dataframe
# Get adjusted prices & Combines them into a single data frame
prices.data <- merge.zoo(SPY[,6], IWM[,6], EEM[,6], EFA[,6], GDX[,6], IEF[,6])

# Calculate returns & omit the NA Values
returns.data <- CalculateReturns(prices.data)
returns.data <- na.omit(returns.data)

# Set names
colnames(returns.data) <- c("SPY", "IWM", "EEM", "EFA", "GDX", "IEF")

# Save mean return vector and sample covariance matrix
meanReturns <- colMeans(returns.data)
covMat <- cov(returns.data)

# Start with the names of the assets
port <- portfolio.spec(assets = c("SPY", "IWM", "EEM", "EFA", "GDX", "IEF"))

# Box
port <- add.constraint(port, type = "box", min = 0.05, max = 0.8)

# Leverage
port <- add.constraint(portfolio = port, type = "full_investment")

# Generate random portfolios
rportfolios <- random_portfolios(port, permutations = 500000, rp_method = "sample")

# Get minimum variance portfolio
minvar.port <- add.objective(port, type = "risk", name = "var")

# Optimize
minvar.opt <- optimize.portfolio(returns.data, minvar.port, optimize_method = "random", 
                                 rp = rportfolios)

# Generate maximum return portfolio
maxret.port <- add.objective(port, type = "return", name = "mean")

# Optimize
maxret.opt <- optimize.portfolio(returns.data, maxret.port, optimize_method = "random", 
                                 rp = rportfolios)

#Assign the min and max constraints for the efficient frontier
# Generate vector of returns
minret <- 0.06/100
maxret <- maxret.opt$weights %*% meanReturns

vec <- seq(minret, maxret, length.out = 100)

eff.frontier <- data.frame(Risk = rep(NA, length(vec)),
                          Return = rep(NA, length(vec)), 
                          SharpeRatio = rep(NA, length(vec)))

frontier.weights <- mat.or.vec(nr = length(vec), nc = ncol(returns.data))
colnames(frontier.weights) <- colnames(returns.data)

for(i in 1:length(vec)){
  eff.port <- add.constraint(port, type = "return", name = "mean", return_target = vec[i])
  eff.port <- add.objective(eff.port, type = "risk", name = "var")
  # eff.port <- add.objective(eff.port, type = "weight_concentration", name = "HHI",
  #                            conc_aversion = 0.001)
  
  eff.port <- optimize.portfolio(returns.data, eff.port, optimize_method = "ROI")
  
  eff.frontier$Risk[i] <- sqrt(t(eff.port$weights) %*% covMat %*% eff.port$weights)
  
  eff.frontier$Return[i] <- eff.port$weights %*% meanReturns
  
  eff.frontier$Sharperatio[i] <- eff.port$Return[i] / eff.port$Risk[i]
  
  frontier.weights[i,] = eff.port$weights
  
  print(paste(round(i/length(vec) * 100, 0), "% done..."))
}
feasible.sd <- apply(rportfolios, 1, function(x){
  return(sqrt(matrix(x, nrow = 1) %*% covMat %*% matrix(x, ncol = 1)))
})

feasible.means <- apply(rportfolios, 1, function(x){
  return(x %*% meanReturns)
})

feasible.sr <- feasible.means / feasible.sd

p <- plot_ly(x = feasible.sd, y = feasible.means, color = feasible.sr, 
             mode = "markers", type = "scattergl", showlegend = F,
             
             marker = list(size = 3, opacity = 0.5, 
                           colorbar = list(title = "Sharpe Ratio"))) %>% 
  
  add_trace(data = eff.frontier, x = Risk, y = Return, mode = "markers", 
            type = "scattergl", showlegend = F, 
            marker = list(color = "#F7C873", size = 5)) %>% 
  
  layout(title = "Random Portfolios with Plotly",
         yaxis = list(title = "Mean Returns", tickformat = ".2%"),
         xaxis = list(title = "Standard Deviation", tickformat = ".2%"),
         plot_bgcolor = "#434343",
         paper_bgcolor = "#F8F8F8",
         annotations = list(
           list(x = 0.4, y = 0.75, 
                ax = -30, ay = -30, 
                text = "Efficient frontier", 
                font = list(color = "#F6E7C1", size = 15),
                arrowcolor = "white")
         ))


frontier.weights.melt <- reshape2::melt(frontier.weights)

q <- plot_ly(frontier.weights.melt, x = Var1, y = value, group = Var2, type = "bar") %>%
  layout(title = "Portfolio weights across frontier", barmode = "stack",
         xaxis = list(title = "Index"),
         yaxis = list(title = "Weights(%)", tickformat = ".0%"))


