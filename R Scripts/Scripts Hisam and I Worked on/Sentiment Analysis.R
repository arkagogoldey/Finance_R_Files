#Logging in to Twitter API
library(twitteR)
load("twitteR_credentials")
registerTwitterOAuth(twitCred)
##step one seach for the company you would like
IBM.tweets<-searchTwitter('@IBM', n=1500, retryOnRateLimit=1)
length(IBM.tweets)
class(IBM.tweets)
tweet=IBM.tweets[[1]]
class(tweet)

tweet$getScreenName()
tweet$getText()

##step 2 load plyr package
install.packages('plyr')
library(plyr)
#step 3 convert from list to character string
IBM.text=laply(IBM.tweets,function(t)t$getText())

length(IBM.text)
head(IBM.text,5)

class(IBM.text)
#Step 3 Load in Word Lists For Sentiment Analysis
#positive words list
hu.liu.pos = scan('/Users/hisamsabouni2/Desktop/R/opinion-lexicon-English/positive-words.txt', what='character', comment.char=';')
class(hu.liu.pos)
#negative words list
hu.liu.neg = scan('/Users/hisamsabouni2/Desktop/R/opinion-lexicon-English/negative-words.txt', what='character', comment.char=';')
class(hu.liu.neg)

#add in some other words
pos.words =c(hu.liu.pos)
neg.words=c(hu.liu.neg)

##Step 4 Clean up the data by removing emoticons
##Dealing with emoticons (makes tweets with them NA)
tryTolower = function(x)
{
	y= NA
	try_error = tryCatch(tolower(x), error = function(e) e)
	if(!inherits(try_error, "error"))
	y= tolower(x)
	return(y)
}
##Step 5 run the scoring algo
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

## Step 6 Test it
sample=c("You're awesome and I love you", "I hate and hate and hate. So angry, Die!")
result= score.sentiment(sample, pos.words, neg.words)
result
class(result)
colnames(result)
rownames(result)
result[,'score']
###check if it works!!
##if step 6 is a sucess Run on the company
## lets see how IBM is doing...
class(IBM.text)
IBM.scores= score.sentiment(IBM.text, pos.words, neg.words, .progress='text')

class(IBM.scores)
#now we have a data frame we can manipulate!
##Step 7 we can plot the data 
#lets pull up a histogram to see how pos or neg people are about IBM
hist(IBM.scores$score)
#another graphics package
install.packages('ggplot2')
library(ggplot2)
#nicer graph..
q=qplot(IBM.scores$score)
q = q + theme_bw()
q

###Step 8 Scoring system based upon extremes.
##now we have a way of scoring individual tweets. It now be nice to summarize all the data for the company we are looking at. This could be done by focusing on the two extremes: very positive and very negative tweets. From here we can measure the value of tweets that are very positive realtive to the total number of extreme tweets and come up with a score between 0-100.
#define number for pos and neg and create new columns
IBM.scores$very.pos.bool = IBM.scores$score >= 2
IBM.scores$very.neg.bool = IBM.scores$score <= -2
#lets test if it works.. we select rows 3:4 to show both the very pos and very neg
IBM.scores[c(1,6,47,99), c(1,3:4)]

#convert T and F to 1 & 0 so we can sum them.
IBM.scores$very.pos = as.numeric(IBM.scores$very.pos.bool)
IBM.scores$very.neg = as.numeric(IBM.scores$very.neg.bool)
#test again to see if printing ones and zeros now( rows 5 and 6 now will be binary 3 and 4 will still be true or false)
IBM.scores[c(1,6,47,99), c(1,1:6)]

#add in a column to refer to IBM(cleaning up)
IBM.scores$symbol=c("IBM")

#aggregate very pos and very neg 
twitter.df=ddply(IBM.scores,c('symbol'), summarize, very.pos.count=sum(very.pos), very.neg.count=sum(very.neg))
twitter.df$very.tot = twitter.df$very.pos.count + twitter.df$very.neg.count
##get a score!
twitter.df$score=round(100*twitter.df$very.pos.count/twitter.df$very.tot)
##view the score for the company
twitter.df