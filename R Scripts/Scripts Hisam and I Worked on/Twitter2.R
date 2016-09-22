#Booting up...
library(twitteR)
load("twitteR_credentials")
registerTwitterOAuth(twitCred)

#twitter seach experiments #1
c<-searchTwitter('claremont', geocode='34.1025,-117.7103,1mi',  n=500, retryOnRateLimit=1)
#number of tweets collected
length(c)
#show a specific tweet
c

tweet10 = c[[1]]
#show only the text from tweet 10
tweet10$getText()
#show the user from tweet 10
tweet10$getScreenName()
getFriends('Nosferican')
st<- searchTwitter('', n=500)
st

## experiment

c
names(c)
library(plyr)
b= lapply(c,function(t)t$getText())
z<-data.frame(b)
z