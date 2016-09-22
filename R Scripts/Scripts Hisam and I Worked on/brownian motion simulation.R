install.packages("yuima")
library(yuima)
m1=setModel(drift="mu*s",diffusion="sigma*s", state.var="s", time.var="t", solve.var="s", xinit=54.47)
simnum=10000
dist=c(.01,.05,.31,.52,.6,.7,.95,.99)
newsim=function(i){simulate(m1, true.param=list(mu=.05, sigma=0.03675))@data@original.data}
newsim(1)
#simulations in Columns->days in rows
sim=sapply(1:simnum,function(x)newsim(x))
#simulations in rows->days in columns
m2=t(sim)
m2
#mean of the columns or days since we are applying to m2
apply(m2,2,mean)

tile=sapply(1:100,function(x)quantile(m2[,x], dist))
plot(density(tile))
tile

#plotting simulations
plot(0:100,sim[,1],type="l")
lines(0:100, sim[,2], type="l",col=2)
lines(0:100, sim[,3], type="l",col=3)
lines(0:100, sim[,4], type="l",col=4)
lines(0:100, sim[,5], type="l",col=5)
lines(0:100, sim[,6], type="l",col=6)
lines(0:100, sim[,7], type="l",col=7)
lines(0:100, sim[,8], type="l",col=8)
lines(0:100, sim[,9], type="l",col=9)
lines(0:100, sim[,10], type="l",col=10)