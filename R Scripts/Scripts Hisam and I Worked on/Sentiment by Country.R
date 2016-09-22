library(twitteR)
load("twitteR_credentials")
registerTwitterOAuth(twitCred)
#step one seach for the company you would like
syria.tweets<-searchTwitter('#syria', n=1500, retryOnRateLimit=1)
length(syria.tweets)
class(syria.tweets)
tweet=syria.tweets[[1]]
class(tweet)

tweet$getScreenName()
tweet$getText()

#step 2 load plyr package
install.packages('plyr')
library(plyr)
#step 3 convert from list to character string
syria.text=laply(syria.tweets,function(t)t$getText())

length(syria.text)
head(syria.text,5)

class(syria.text)
#Step 3 Load in Word Lists
#positive words list
hu.liu.pos = scan('/Users/hisamsabouni2/Desktop/Mac Pro/R/opinion-lexicon-English/positive-words.txt', what='character', comment.char=';')
class(hu.liu.pos)
#negative words list
hu.liu.neg = scan('/Users/hisamsabouni2/Desktop/Mac Pro/R/opinion-lexicon-English/negative-words.txt', what='character', comment.char=';')
class(hu.liu.neg)

#add in some other words
pos.words =c(hu.liu.pos, 'upgrade')
neg.words=c(hu.liu.neg, 'wtf', 'wait', 'waiting', 'epicfail', 'mechanical')

#Step 4 Clean up the data by removing emoticons
##Dealing with emoticons (makes tweets with them NA)
tryTolower = function(x)
{
	y= NA
	try_error = tryCatch(tolower(x), error = function(e) e)
	if(!inherits(try_error, "error"))
	y= tolower(x)
	return(y)
}
#Step 5 run the scoring algo
#install stringr
install.packages('stringr')
##Scoring Algorithm
score.sentiment=function(sentences, pos.words, neg.words, .progress='none')
{
require(plyr)
require(stringr)
scores = laply(sentences, function(sentence, pos.words, neg.words) {
sentence = gsub('[[:punct:]]', '', sentence)
sentence = gsub('[[:cntrl:]]', '', sentence)
sentence = gsub('\\d+', '', sentence)
sentence = sapply(sentence, function(x) tryTolower(x))
word.list = str_split(sentence, '\\s+')
words = unlist(word.list)
pos.matches = match(words, pos.words)
neg.matches = match(words, neg.words)
pos.matches = !is.na(pos.matches)
neg.matches = !is.na(neg.matches)
score=sum(pos.matches)-sum(neg.matches)
return(score)}, pos.words, neg.words, .progress=.progress)
scores.df = data.frame(score=scores, text=sentences)
return(scores.df)}

# Step 6 Test it
sample=c("You're awesome and I love you", "I hate and hate and hate. So angry, Die!")
result= score.sentiment(sample, pos.words, neg.words)
result
class(result)
colnames(result)
rownames(result)
result[,'score']
#check if "it fucking works!"
#if step 6 is a sucess Run on the company
# lets see how syria is doing...
class(syria.text)
syria.scores= score.sentiment(syria.text, pos.words, neg.words, .progress='text')

class(syria.scores)
#now we have a data frame we can manipulate!
#Step 7 we can plot the data 
#lets pull up a histogram to see how pos or neg people are about syria
hist(syria.scores$score)
#another graphics package
install.packages('ggplot2')
library(ggplot2)
#nicer graph..
q=qplot(syria.scores$score)
q = q + theme_bw()
q

#Step 8 Scoring system based upon extremes.
##now we have a way of scoring individual tweets. It now be nice to summarize all the data for the company we are looking at. This could be done by focusing on the two extremes: very positive and very negative tweets. From here we can measure the value of tweets that are very positive realtive to the total number of extreme tweets and come up with a score between 0-100.
#define number for pos and neg and create new columns
syria.scores$very.pos.bool = syria.scores$score >= 2
syria.scores$very.neg.bool = syria.scores$score <= -2
#lets test if it works.. we select rows 3:4 to show both the very pos and very neg
syria.scores[c(1,6,47,99), c(1,3:4)]
#convert T and F to 1 & 0 so we can sum them.
syria.scores$very.pos = as.numeric(syria.scores$very.pos.bool)
syria.scores$very.neg = as.numeric(syria.scores$very.neg.bool)
#test again to see if printing ones and zeros now( rows 5 and 6 now will be binary 3 and 4 will still be true or false)
syria.scores[c(1,6,47,99), c(1,1:6)]
#add in a column to refer to syria(cleaning up)
syria.scores$symbol=c("SYRIA")
#aggregate very pos and very neg 
twitter.df=ddply(syria.scores,c('symbol'), summarize, very.pos.count=sum(very.pos), very.neg.count=sum(very.neg))
twitter.df$very.tot = twitter.df$very.pos.count + twitter.df$very.neg.count
#get a score!
twitter.df$score=round(100*twitter.df$very.pos.count/twitter.df$very.tot)
#view the score for the company
twitter.df
hist(syria.scores$score)
///////////////////////////////////////////////////////////////////
ukraine.tweets<-searchTwitter('#ukraine', n=1500, retryOnRateLimit=1)
length(ukraine.tweets)
class(ukraine.tweets)
ukraine.text=laply(ukraine.tweets,function(t)t$getText())
class(ukraine.text)
ukraine.scores= score.sentiment(ukraine.text, pos.words, neg.words, .progress='text')
class(ukraine.scores)
ukraine.scores$very.pos.bool = ukraine.scores$score >= 2
ukraine.scores$very.neg.bool = ukraine.scores$score <= -2
ukraine.scores$very.pos = as.numeric(ukraine.scores$very.pos.bool)
ukraine.scores$very.neg = as.numeric(ukraine.scores$very.neg.bool)
ukraine.scores$symbol=c("UKRAINE")
twitter1.df=ddply(ukraine.scores,c('symbol'), summarize, very.pos.count=sum(very.pos), very.neg.count=sum(very.neg))
twitter1.df$very.tot = twitter1.df$very.pos.count + twitter1.df$very.neg.count
twitter1.df$score=round(100*twitter1.df$very.pos.count/twitter1.df$very.tot)
twitter1.df
#merge syria and ukraine
total<-rbind(twitter.df,twitter1.df)
class(total)
total
hist(ukraine.scores$score)
///////////////////////////////////////////////////////////////////
russia.tweets<-searchTwitter('#russia', n=1500, retryOnRateLimit=1)
length(russia.tweets)
class(russia.tweets)
russia.text=laply(russia.tweets,function(t)t$getText())
class(russia.text)
russia.scores= score.sentiment(russia.text, pos.words, neg.words, .progress='text')
class(russia.scores)
russia.scores$very.pos.bool = russia.scores$score >= 2
russia.scores$very.neg.bool = russia.scores$score <= -2
russia.scores$very.pos = as.numeric(russia.scores$very.pos.bool)
russia.scores$very.neg = as.numeric(russia.scores$very.neg.bool)
russia.scores$symbol=c("RUSSIA")
twitter2.df=ddply(russia.scores,c('symbol'), summarize, very.pos.count=sum(very.pos), very.neg.count=sum(very.neg))
twitter2.df$very.tot = twitter2.df$very.pos.count + twitter2.df$very.neg.count
twitter2.df$score=round(100*twitter2.df$very.pos.count/twitter2.df$very.tot)
twitter2.df
#merge with the rest
total1<-rbind(total,twitter2.df)
total1
hist(russia.scores$score)
///////////////////////////////////////////////////////////////////
usa.tweets<-searchTwitter('#usa', n=1500, retryOnRateLimit=1)
length(usa.tweets)
class(usa.tweets)
usa.text=laply(usa.tweets,function(t)t$getText())
class(usa.text)
usa.scores= score.sentiment(usa.text, pos.words, neg.words, .progress='text')
class(usa.scores)
usa.scores$very.pos.bool = usa.scores$score >= 2
usa.scores$very.neg.bool = usa.scores$score <= -2
usa.scores$very.pos = as.numeric(usa.scores$very.pos.bool)
usa.scores$very.neg = as.numeric(usa.scores$very.neg.bool)
usa.scores$symbol=c("USA")
twitter3.df=ddply(usa.scores,c('symbol'), summarize, very.pos.count=sum(very.pos), very.neg.count=sum(very.neg))
twitter3.df$very.tot = twitter3.df$very.pos.count + twitter3.df$very.neg.count
twitter3.df$score=round(100*twitter3.df$very.pos.count/twitter3.df$very.tot)
twitter3.df
#merge with the rest
total2<-rbind(total1,twitter3.df)
total2
///////////////////////////////////////////////////////////////////
