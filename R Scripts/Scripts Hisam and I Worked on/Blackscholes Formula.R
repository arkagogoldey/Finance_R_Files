
#blackscholes calculation
#time is expressed as a percent of the year
# Returns a 2 Value Vector Containing the Call and Put Price for a Given Strike

blackscholes <- function(S, X, rf, T, sigma) {
    values <- c(2)

    d1 <- (log(S/X)+(rf+sigma^2/2)*T)/sigma*sqrt(T)
    d2 <- d1 - sigma * sqrt(T)

    values[1] <- S*pnorm(d1) - X*exp(-rf*T)*pnorm(d2)
    values[2] <- X*exp(-rf*T) * pnorm(-d2) - S*pnorm(-d1)

    values
}

#volatility smile - As shown by larger sigmas in lower strikes
blackscholes(17.89,14,.01,1/52,1.989)
blackscholes(17.89,15,.01,1/52,1.745)
blackscholes(17.89,16,.01,1/52,1.079)
blackscholes(17.89,17,.01,1/52,0.498)
blackscholes(17.89,18,.01,1/52,0.225)
blackscholes(17.89,19,.01,1/52,0.535)

smile<-c(1.989,1.745,1.079,0.498,0.225,0.535)
strike<-c(14,15,16,17,18,19)
flat<-c(1,1,1,1,1,1)
plot(strike,smile,type="l",col=4)
lines(strike,flat)
#Arguments
# S = Underlying Price
# X = Strike Price
# rf = Risk Free rate (Are they in percent already?)
# T = Time until Expiration (Expressed as a percent of years)
# Sigma = implied volatility (standard Deviation) of the contract
S <- c(18)
X <- c(20)
rf <- c(0.35)
T <- c(22/52)
sigma <- c(2)
blackscholes(S, X, rf, T, sigma)

# I also want to be able to solve for the Greeks given a contract
#See Pages 3-5 of RQuantlib Pdf
library(RQuantLib)
#value - Value of option
#delta - Sensitivity of the option value for a change in the underlying
#gamma - Sensitivity of the option delta for a change in the underlying
#vega - Sensitivity of the option value for a change in the underlying's volatility
#theta - Sensitivity of the option value for a change in t, the remaining time to maturity
#rho - Sensitivity of the option value for a change in the risk-free interest rate
#dividendRho - Sensitivity of the option value for a change in the dividend yield

#AmericanOption(type, underlying, strike, dividendYield, riskFreeRate, maturity, volatility, timeSteps=150, gridPoints=149, engine="BaroneAdesiWhaley")
#Note that under the new pricing framework used in QuantLib, pricers do not provide analytics for
# all 'Greeks'. When "CrankNicolson" is selected, then at least delta, gamma and vega are available.
# With the default pricing engine of "BaroneAdesiWhaley", no greeks are returned.

# simple call with unnamed parameters
AmericanOption("call", 100, 100, 0.02, 0.03, 0.5, 0.4)
# simple call with some explicit parameters
AmericanOption("put", strike=100, volatility=0.4, 100, 0.02, 0.03, 0.5)
# simple call with unnamed parameters, using Crank-Nicolons
AmericanOption("put", strike=100, volatility=0.4, 100, 0.02, 0.03, 0.5, engine="CrankNicolson")

