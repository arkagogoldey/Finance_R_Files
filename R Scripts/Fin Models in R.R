
# Minimum Variance Portfolio
# The following is from: Financial Risk Models in R: Factor Models for Asset Returns and Interest Rate Models


Compute Single Index Covariance

# compute single index model covariance/correlation
> cov.si =
  as.numeric(var(market.mat))*beta.hat%*%t(beta.hat)
+ diag(diagD.hat)
> cor.si = cov2cor(cov.si)
# plot correlation matrix using plotcorr() from
# package ellipse
d < d ( i[1 ])

ord <- order(cor.si[1,])
ordered.cor.si <- cor.si[ord, ord]
plotcorr(ordered.cor.si,
           + col=cm.colors(11)[5*ordered.cor.si + 6])

# use single index covariance
w.gmin.si > w.gmin.si = solve(cov.si)% (cov.si)% %* rep(1,nrow(cov.si)) (1,nrow(cov.si))
w.gmin.si = w.gmin.si/sum(w.gmin.si)
colnames(w.gmin.si) = "single.index"
# use sample covariance
w.gmin.sample = solve(var(returns.mat))%*%rep(1,nrow(cov.si))
w.gmin.sample = w.gmin.sample/sum(w.gmin.sample)
colnames(w.gmin.sample) = "sample"
