#Practice Writing Functions in R
#Pricing a semi-annual bond
price_bond_sa<-function(face,y,t,r)
{
	c=face*r/2
	n=t*2
	sa_y=y/2
	PV=(c*(1-(1/((1+sa_y)^n)))/sa_y)+face/((1+sa_y)^n)
}

f=price_bond_sa(1000,.12,10,.1)
f

#Pricing an annual bond
price_bond_a<-function(face,y,t,r)
{
	c=face*r
	PV=(c*(1-(1/((1+y)^t)))/y)+face/((1+y)^t)
}

p=price_bond_a(1000,.12,10,.1)
p


#Generic for Different Payment Frequencies
price_bond<-function(face,y,t,r,f)
{
	c=face*r/f
	n=t*f
	f_y=y/f
	PV=(c*(1-(1/((1+f_y)^n)))/f_y)+face/((1+f_y)^n)
}
q=price_bond(1000,.08,10,.1,2)
q


