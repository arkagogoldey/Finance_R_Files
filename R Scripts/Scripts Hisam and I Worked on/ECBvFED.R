library(twitteR)
registerTwitterOAuth(twitCred)
federalreserve.tweets<-searchTwitter('@federalreserve', n=1500, retryOnRateLimit=1)
length(federalreserve.tweets)
class(federalreserve.tweets)
federalreserve.text=laply(federalreserve.tweets,function(t)t$getText())
class(federalreserve.text)
federalreserve.scores= score.sentiment(federalreserve.text, pos.words, neg.words, .progress='text')
class(federalreserve.scores)
federalreserve.scores$very.pos.bool = federalreserve.scores$score > 0
federalreserve.scores$very.neg.bool = federalreserve.scores$score < 0
federalreserve.scores$very.pos = as.numeric(federalreserve.scores$very.pos.bool)
federalreserve.scores$very.neg = as.numeric(federalreserve.scores$very.neg.bool)
federalreserve.scores$symbol=c("FED")
twitter.df=ddply(federalreserve.scores,c('symbol'), summarize, very.pos.count=sum(very.pos), very.neg.count=sum(very.neg))
twitter.df$very.tot = twitter.df$very.pos.count + twitter.df$very.neg.count
twitter.df$score=round(100*twitter.df$very.pos.count/twitter.df$very.tot)
twitter.df

#ECB
ecb.tweets<-searchTwitter('@ecb', n=1500, retryOnRateLimit=1)
length(ecb.tweets)
class(ecb.tweets)
ecb.text=laply(ecb.tweets,function(t)t$getText())
class(ecb.text)
ecb.scores= score.sentiment(ecb.text, pos.words, neg.words, .progress='text')
class(ecb.scores)
ecb.scores$very.pos.bool = ecb.scores$score > 0
ecb.scores$very.neg.bool = ecb.scores$score < 0
ecb.scores$very.pos = as.numeric(ecb.scores$very.pos.bool)
ecb.scores$very.neg = as.numeric(ecb.scores$very.neg.bool)
ecb.scores$symbol=c("ECB")
twitter1.df=ddply(ecb.scores,c('symbol'), summarize, very.pos.count=sum(very.pos), very.neg.count=sum(very.neg))
twitter1.df$very.tot = twitter1.df$very.pos.count + twitter1.df$very.neg.count
twitter1.df$score=round(100*twitter1.df$very.pos.count/twitter1.df$very.tot)
twitter1.df

total2<-rbind(twitter1.df,twitter.df)
total2
hist(ecb.scores$score, freq=F, add=T, col=3)
hist(federalreserve.scores$score, freq=F, col=2)
length(ecb.scores$score)
length(federalreserve.scores$score)