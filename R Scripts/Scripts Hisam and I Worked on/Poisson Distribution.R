#Poisson Distribution 
#f(x)=(lambda^(x)*exp^(-lambda))/x!, where x= 0,1,2,3,..

#Ex:if there are tweleve cars crossing a bridge per minute on average, find the proability of having seventeen or more cars crossing the bridge in a particular minute.
	#lambda=12
	#the probability of having sixteen of less cars crossing the bridge in a particular minute is given by the function ppois. 
	#we can use the C.D.F. to solve this 
	#Pr(Y>=17)=1-Pr(Y<=16)
	#Pr(Y<=16)
	ppois(16,lambda=12)
	#[1] 0.898709
	#now we can subtract this from 1
	1-ppois(16, lambda=12)
	#Pr(Y>=17) = [1] 0.101291
	
	#However in R we can simply use this code to get the same answer
	ppois(16, lambda=12,lower.tail=F)
	#Pr(Y>=17) = [1] 0.101291