#Expected Move Options On Week of Expirations
EMC<-function(ATM_Call,ATM_Put,OTM_Call,OTM_Put)
{
	EMC=(ATM_Call+ATM_Put+OTM_Call+OTM_Put)/2
}

a<-EMC(0.99,1.23,0.55,.83)
a
#(ATM Straddle +OTM Straddle (one lower put, one higher call))/2
#Expected move of

#Probability of success= Max Loss/Strike Width