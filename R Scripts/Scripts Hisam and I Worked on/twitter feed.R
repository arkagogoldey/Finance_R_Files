#Booting up...
library(twitteR)
load("twitteR_credentials")
registerTwitterOAuth(twitCred)

#some basics on united
un.tweets = searchTwitter('@United', n=1000, cainfo="cacert.pem")
#number of tweets collected
length(un.tweets)
#show a specific tweet
tweet500 = un.tweets[[500]]
#show only the text from tweet 500
tweet500$getText()
#show the user from tweet 500
tweet500$getScreenName()

#Plyr Package which contains tools for splitting, applying and combining data!
library(plyr)

un.text = lapply(un.tweets, function(t)t$getText())

#show the first five entries
head(un.text,5)

install.packages('xlsx')
library(xlsx)
write.csv(un.text, "un.csv")
write.csv(un.tweets,"un2.csv")

head(un.text,10)
write.xlsx(un.text, "un.xlsx")

