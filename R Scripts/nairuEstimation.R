#Uncomment and run the line below if you do not have the package installed
#install.packages('quantmod')
library(quantmod)
getSymbols(c('CPALTT01USQ661S','UNRATE'),src = 'FRED')
df <- merge(CPALTT01USQ661S,UNRATE,all = T)
# Changed above object from CPALTT01USQ657N to CPALTT01USQ661S
df <- na.omit(df) #Remove NA entries
head(df) #Data is quarterly 
df <- df[grep('01-01',index(df)),] #Keep only first quarter
df$CPI.Change <- diff(df[,1]) #Changes in CPI
df <- na.omit(df) #Remove NA entries
head(df) #First 6 rows
tail(df) #Last 6 rows
plot(df$CPI.Change)
plot(df$UNRATE,main = 'Unemployment Rate',ylab = '%')
#Mankiws paper has data only through 2000
df_mankiw <- df[which(index(df) < as.Date('2001-01-01')),]
mdl <- lm(CPI.Change ~ UNRATE, data = df_mankiw)
summary(mdl)# Mankiw's intercept was 0.038 and coefficent on unemployment was -0.63
  #Our results are different as his CPI index must have been indexed to a different year.
  #Our CPI is indexed to 2010. His paper was published in 2000
u_star_mankiw <- mdl$coefficients[1]/abs(mdl$coefficients[2])
u_star_mankiw #NAIRU estimate of 6.14. Mankiw had an estimate of 6.1

#NAIRU Evolution
#Uncomment and run the line below if you do not have the package installed
#install.packages('mFilter')
library(mFilter) #HP-filter
u_star_plus_v <- as.numeric(df_mankiw$UNRATE) + as.numeric(df_mankiw$CPI.Change)/abs(mdl$coefficients[2])
hp_decomp_100 <- hpfilter(u_star_plus_v,freq = 100)
hp_decomp_1000 <- hpfilter(u_star_plus_v,freq = 1000)

plot(index(df_mankiw),as.numeric(u_star_plus_v),typ='l',main = "NAIRU through 2000",xlab='Time',ylab = '%')
lines(index(df_mankiw),hp_decomp_100$trend, col = 2)
lines(index(df_mankiw),hp_decomp_1000$trend, col = 3)
legend('topleft',legend = c('U* + v/a','U* lambda = 100','U* lambda = 1000'),lty = 1,col = 1:3,cex = 0.6)

#Updated Estimate
mdl <- lm(CPI.Change ~ UNRATE, data = df)
summary(mdl)
u_star <- mdl$coefficients[1]/abs(mdl$coefficients[2])
u_star #NAIRU estimate of 6.17
u_star_plus_v <- as.numeric(df$UNRATE) + as.numeric(df$CPI.Change)/abs(mdl$coefficients[2])
hp_decomp_100 <- hpfilter(u_star_plus_v,freq = 100)
hp_decomp_1000 <- hpfilter(u_star_plus_v,freq = 1000)

plot(index(df),as.numeric(u_star_plus_v),typ='l',main = "NAIRU through 2018",xlab='Time',ylab = '%')
lines(index(df),hp_decomp_100$trend, col = 2)
lines(index(df),hp_decomp_1000$trend, col = 3)
legend('topleft',legend = c('U* + v/a','U* lambda = 100','U* lambda = 1000'),lty = 1,col = 1:3,cex = 0.6)

