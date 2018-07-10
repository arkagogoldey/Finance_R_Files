"WienerSim" <- function(T=1000, iter=2000,seed = 201705){
  set.seed(seed)
  x=matrix(rnorm(T*iter),T,iter)
  y=apply(x,2,cumsum)
  y=y/sqrt(T)
  cy=range(y)*1.1
  plot(y[,1],xlab='time',ylab='Wiener',type='l',ylim=cy)
  for (i in 2:iter){
    lines(y[,i],col=i)
  }
  ###end of the program
}