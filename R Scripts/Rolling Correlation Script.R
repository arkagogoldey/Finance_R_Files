#Rolling Correlation Code
ibrary(zoo)
library(ggplot2)
library(tseries)

spy <- get.hist.quote(instrument="SPY", start="2003-01-01",
                      end=Sys.Date(), quote="AdjClose",
                      provider="yahoo", origin="1970-01-01",
                      compression="d", retclass="zoo")
ief <- get.hist.quote(instrument="IEF", start="2003-01-01",
                      end=Sys.Date(), quote="AdjClose",
                      provider="yahoo", origin="1970-01-01",
                      compression="d", retclass="zoo")
z <- merge.zoo(spy,ief)

z.logrtn <- diff(log(z))
gld <- get.hist.quote(instrument="GLD", start="2003-01-01",
                      end=Sys.Date(), quote="AdjClose",
                      provider="yahoo", origin="1970-01-01",
                      compression="d", retclass="zoo")
z <- merge.zoo(spy,ief,gld)
colnames(z) <- c("SPY","IEF","GLD")
z.logrtn <- diff(log(z))

c <- cor(z.logrtn,use="complete.obs")
ut <- upper.tri(c)
n <- paste(rownames(c)[row(c)[ut]],rownames(c)[col(c)[ut]])

rollingcorr.1m <- rollapply(z.logrtn,
                            width=30,
                            FUN = function(Z)
                            {
                              return(cor(Z,use="pairwise.complete.obs")[ut])
                            },
                            by.column=FALSE, align="right")
colnames(rollingcorr.1m) <- n

rollingcorr.1m.df <- fortify(rollingcorr.1m,melt=TRUE)

ggplot(rollingcorr.1m.df,aes(x=Index)) +
  geom_ribbon(aes(ymin=0,ymax=Value)) +
  facet_grid(Series~.) +
  ylim(c(-1,1)) +
  theme_bw()
c
#######################################################################
#######################################################################
# Notes
#######################################################################
#######################################################################

http://nakisa.org/rolling-regression-and-rolling-correlation/
  
library(zoo)
library(ggplot2)
library(tseries)

spy <- get.hist.quote(instrument="SPY", start="2003-01-01",
                      end=Sys.Date(), quote="AdjClose",
                      provider="yahoo", origin="1970-01-01",
                      compression="d", retclass="zoo")
ief <- get.hist.quote(instrument="IEF", start="2003-01-01",
                      end=Sys.Date(), quote="AdjClose",
                      provider="yahoo", origin="1970-01-01",
                      compression="d", retclass="zoo")
z <- merge.zoo(spy,ief)

z.logrtn <- diff(log(z))
############# rollingbeta or rollapply seems broken
rollingbeta <- rollapply(z.logrtn,
                         width=262,
                         FUN = function(Z)
                         {t = lm(formula=spy~ief, data = as.data.frame(Z), na.rm=T);return(t$coef)},
                         by.column=FALSE, align="right")

rollingbeta.df <- fortify(rollingbeta,melt=TRUE)
ggplot(rollingbeta.df) + geom_line(aes(x=Index,y=Value)) + facet_grid(Series~.) + theme_bw()
##############
#pick up back here for rolling correlations

gld <- get.hist.quote(instrument="GLD", start="2003-01-01",
                      end=Sys.Date(), quote="AdjClose",
                      provider="yahoo", origin="1970-01-01",
                      compression="d", retclass="zoo")
z <- merge.zoo(spy,ief,gld)
colnames(z) <- c("SPY","IEF","GLD")
z.logrtn <- diff(log(z))

c <- cor(z.logrtn,use="complete.obs")
c
#### Correct Results
#SPY        IEF        GLD
#SPY  1.00000000 -0.4124237 0.06083499
#IEF -0.41242374  1.0000000 0.10786177
#GLD  0.06083499  0.1078618 1.00000000

ut <- upper.tri(c)
n <- paste(rownames(c)[row(c)[ut]],rownames(c)[col(c)[ut]])

rollingcorr.1m <- rollapply(z.logrtn,
                            width=30,
                            FUN = function(Z)
                            {
                              return(cor(Z,use="pairwise.complete.obs")[ut])
                            },
                            by.column=FALSE, align="right")
colnames(rollingcorr.1m) <- n

rollingcorr.1m.df <- fortify(rollingcorr.1m,melt=TRUE)

ggplot(rollingcorr.1m.df,aes(x=Index)) +
  geom_ribbon(aes(ymin=0,ymax=Value)) +
  facet_grid(Series~.) +
  ylim(c(-1,1)) +
  theme_bw()

#######################################################################
#######################################################################
# Notes
#######################################################################
#######################################################################

# http://nakisa.org/rolling-regression-and-rolling-correlation/

#http://www.r-bloggers.com/new-r-code-for-moving-correlations/
# 2011-03-04
# v0.01

MovingCor <- function(x, y, window.size=21, method="pearson") {
  # Computes moving correlations between two vectors with symmetrical windows.
  #
  # Args:
  #   x: One of the two vectors whose correlation is to be calculated.
  #   y: The other vector. Note that it must be of the same length as x.
  #   window.size: The size of windows to be used for each calculated
  #                correlation. Note that if even numbers are chosen, the
  #                window will not be skewed as there will be one extra value
  #                on the upper-side of the window. Default size is 21.
  #   method: The method of correlation. May be: "pearson", "kendall", or
  #           "spearman". Default is "pearson".
  #
  # Returns:
  #   A vector of the moving correlations.
  n <- length(x)
  # Setup a few catches for error handling.
  if (TRUE %in% is.na(y) || TRUE %in% is.na(x)) {
    stop("Arguments x and y cannot have missing values.")
  }
  if (n &lt;= 1 || n != length(y)) {
    stop("Arguments x and y have different lengths: ",
         length(x), " and ", length(y), ".")
  }
  out <- rep(NA, round(window.size/2))  # Stuffing the returned vector.
  for (value in seq(from = 1, to = n - (window.size - 1))) {
    value.end &lt;- value + (window.size - 1)
    out <- append(out, cor(x[value:value.end],
                           y[value:value.end],
                           method = method))
  }
  out <- append(out, rep(NA, n - length(out)))  # Finish stuffing.
  return(out)
}
