##Discounted Cash Flow Model
###Basic Sales Forecasts
SF<-function(Sales,Growth_Rate,Decay,Periods){
	z=0:Periods
	Sales_Forecast<-Sales*(1+Growth_Rate*((1-Decay)^(z)))^z
}
a<-SF(100000,.2,.05,5)
a
a
##We can build similar function to SF For each of the other Variables. 
##Operating Cost Forecast
OCF<-function(Sales,OP_R,Growth_Rate,Decay,Periods){
	Operating_C=OP_R*Sales
	z=0:Periods
	Operating_C_Forecast<-Operating_C*(1+Growth_Rate*((1-Decay)^(z)))^z
}
b<-OCF(a,.65,.02,0,5)
b
####Net Investment Forecast
NIF<-function(Sales,Net_INv,Growth_Rate,Decay,Periods){
	z=0:Periods
	Net_INv_<-Net_INv*(1+Growth_Rate*((1-Decay)^(z)))^z
	Net_INv_Forecast<-Sales*Net_INv_
}
c<-NIF(a,.07,.02,0,5)
c
###Change in Working Capital Forecast
CWCF<-function(Initial_WC,Growth_Rate,Decay,Periods){
	z=0:Periods
	WC_Forecast<-Initial_WC*(1+Growth_Rate*((1-Decay)^(z)))^z
}
d<-CWCF(9000,.2,0.04,6)
d<-as.data.frame(diff(d))
d<-t(d)
d<-as.numeric(d)
d

######Free Cash Flow Forecast##############
FCF<-function(Sales_Rev,Op_Cost,Tax_Rate,Net_Inv,Change_WC){
	
	Taxes=(Sales_Rev - Op_Cost)*Tax_Rate
	
	FCF=Sales_Rev- Op_Cost - Taxes- Net_Inv - Change_WC
}

e<-FCF(a,b,.3,c,d)
e
e<-as.data.frame(t(e))
colnames(e)
##Last forecasted cash flow 
e$V6
###Weighted Average Cost of Capital/Discount Rate
WACC<-function(Equity,Debt,Cost_of_Debt,Tax_rate,rf,b,rm){
	Re=rf+b*(rm-rf)
	Rd=Cost_of_Debt*(1-Tax_rate)
	V=Equity+Debt
	WACCE=(Equity)*(Re)
	WACCD=(Debt)*(Rd)
	WACC=WACCE+WACCD
}
test<-WACC(.4,.6,.08,.3,.05,1.5,.13)
wacc<-WACC(.6,.4,.05,.3,.05,1.3,.13)
wacc

##Gordon Growth Model Terminal Value
GGM<-function(Final_CF,CFGR,DR){
	Terminal.Value<-Final_CF*(1+CFGR)/(DR-CFGR)
}
f<-GGM(e$V6,.02,wacc)
f

###Enterprise Value
EV=function(C_F,DR,Gm){
	n<-1:nrow(t(C_F))
	EV<-C_F/(1+DR)^n
	EV<-sum(t(EV))+Gm/(1+DR)^nrow(t(C_F))
}
eta<-EV(e,wacc,f)
eta

##TO get stock price subtract out debt and divide by shares outstanding
P<- function(Enterprise_Value,Debt, Shares_O){
	P<-(Enterprise_Value-Debt)/Shares_O
}
g<-P(eta*10,500000,10000)
g