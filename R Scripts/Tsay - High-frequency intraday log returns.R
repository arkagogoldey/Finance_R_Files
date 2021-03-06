"hfrtn" <- function(da,int,logrtn=TRUE){
  # Compute intraday returns
  #
  # int: time intervals in minutes
  # da: data in the format: date, hour, minute, second, price, volume
  #
  if(!is.matrix(da))da=as.matrix(da)
  intsec=int*60
  istart=9*60*60+30*60
  iend=16*60*60
  # compute the number of prices
  tradetime=6.5*60*60
  ntrade=floor(tradetime/intsec)
  T=dim(da)[1]
  
  ### renumber day number for ease in calculation
  tmp=diff(da[,1])
  idx=c(1:(T-1))[tmp > 0]
  tmp[idx]=1
  daynumber=cumsum(tmp)
  da[2:T,1]=daynumber+da[1,1]
  ###
  nday=length(idx)+1
  npri=nday*ntrade
  #print(c(ntrade,nday,npri))
  
  price=rep(0,npri)
  # price is the last transaction price of the time interval
  caltime=da[,2]*60*60+da[,3]*60+da[,4]
  #plot(caltime,type='l')
  
  icnt=0
  date=da[1,1]
  for (i in 1:T) {
    if(caltime[i] > istart){
      iday=da[i,1]-date
      if(caltime[i] < (iend+1)){
        
        if(caltime[i]==iend){
          price[iday*ntrade+ntrade]=da[i,5]
        }
        
        if((caltime[i] > istart) && (caltime[i] < iend)){
          ii=caltime[i]-istart
          ij=floor(ii/intsec)
          price[iday*ntrade+ij+1]=da[i,5]
        }
        
      }
    }
  }
  
  idx=c(1:length(price))[price > 0]
  if(price[1] <=0)price[1]=price[idx[1]]
  ##
  
  for (i in 2:npri){
    if(price[i] <= 0)price[i]=price[i-1]
  }
  
  
  plot(price,type='l')
  
  pri=log(price)
  #skip overnight returns
  nrtn=ntrade-1
  rtn=NULL
  for (i in 1:nday){
    ist=(i-1)*ntrade
    ##for (j in 2:ntrade){
    ##rtn=c(rtn,pri[ist+j]-pri[ist+j-1])
    ##}
    xx=pri[(ist+1):(ist+ntrade)]
    rtn=c(rtn,diff(xx))
  }
  
  hfrtn <- list(rtn=rtn,price=price)
}