library(streamR)
library(ROAuth)
load("my_oauth.Rdata")
fiveh.tweets<-filterStream("tweetsUS.json", locations=c(-125, 25, -66, 50),language="en", timeout = 500, oauth=my_oauth)
fiveh.df<-parseTweets("tweetsUS.json", simplify=T)
help(streamR)
length(fiveh.df)
class(fiveh.df)
fiveh.df
length(tweetsUS)
class("tweetsUS.json")
library(plyr)
"tweetsUS.json"
fiveh.text=laply(fiveh,function(t)t$getText())
colnames(fiveh.df)
fiveh<-as.vector(fiveh.df$text)
class(fiveh)
fiveh.scores= score.sentiment(fiveh, pos.words, neg.words, .progress='text')
class(fiveh.scores)
fiveh.scores$very.pos.bool = fiveh.scores$score >= 2
fiveh.scores$very.neg.bool = fiveh.scores$score <= -2
fiveh.scores$very.pos = as.numeric(fiveh.scores$very.pos.bool)
fiveh.scores$very.neg = as.numeric(fiveh.scores$very.neg.bool)
fiveh.scores$symbol=c("all tweets")
twitter.df=ddply(fiveh.scores,c('symbol'), summarize, very.pos.count=sum(very.pos), very.neg.count=sum(very.neg))
twitter.df$very.tot = twitter.df$very.pos.count + twitter.df$very.neg.count
twitter.df$score=round(100*twitter.df$very.pos.count/twitter.df$very.tot)
twitter.df
score<-fiveh.scores$score
var(score)
hist(score, freq=F)
mean(score)
median(score)
