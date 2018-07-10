"EVTsim" <- function(n=100,iter=3000,df=0){
  #### df > 0, use Cauchy distribution
  if(df <= 0){
    rt <- matrix(rnorm(n*iter),n,iter)
    cn <- 1/sqrt(2*log(n))
    dn <- sqrt(2*log(n))-(log(4*pi)+log(log(n)))/(2*sqrt(2*log(n)))
  }else{
    rt <- matrix(rt(n*iter,1),n,iter)
    dn <- 0
    cn <- n/pi
  }
  evt <- apply(rt,2,max)
  evt1 <- (evt-dn)/cn
  
  EVTsim <- list(max=evt,stmax=evt1)
}

"qgumble" <- function(prob=c(0.95,0.975,0.99)){
  ### compute the quantile of Gumble distribution
  n <- length(prob)
  qgum <- NULL
  for (i in 1:n){
    p <- prob[i]
    x <- -log(-log(p))
    qgum <- c(qgum,x)
  }
  qgum
}
###