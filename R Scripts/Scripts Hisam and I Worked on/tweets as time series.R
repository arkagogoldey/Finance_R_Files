library(twitteR)
load("twitteR_credentials")
registerTwitterOAuth(twitCred)
#step one seach for the company you would like
united.tweets<-searchTwitter('@United', n=1500,retryOnRateLimit=1)
length(united.tweets)
class(united.tweets)
tweet1=united.tweets[[1500]]
class(tweet)
head(united.tweets)
tweet$getScreenName()
tweet1$created

#step 2 load plyr package
install.packages('plyr')
library(plyr)
#step 3 convert from list to character string
united.text=laply(united.tweets,function(t)t$getText())

length(united.text)
head(united.text,5)

class(united.text)
#Step 3 Load in Word Lists
#positive words list
hu.liu.pos = scan('/Users/hisamsabouni2/Desktop/R/opinion-lexicon-English/positive-words.txt', what='character', comment.char=';')
class(hu.liu.pos)
#negative words list
hu.liu.neg = scan('/Users/hisamsabouni2/Desktop/R/opinion-lexicon-English/negative-words.txt', what='character', comment.char=';')
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
# lets see how united is doing...
class(united.text)
united.scores= score.sentiment(united.text, pos.words, neg.words, .progress='text')

class(united.scores)
united.scores
df <- do.call("rbind", lapply(united.tweets, as.data.frame))
df
rownames(united.scores)=c(1:150)
z<-mean(united.scores$score)
s<-sd(united.scores$score)
p1<- plot(1:150, united.scores$score, type="l")
par(new=T)
p2<- plot(1:150,rep(z,150), type="l", )
par(new=T)
p3<- plot(1:150,rep((z+s),150),type="l")
help()
united.scores$Date = rownames(united.scores)
rownames(united.scores)=NULL
united.scores$Date = strptime(united.scores$Date, "%Y-%m-$d")
within(united.scores, Date<-united.tweets$created)
str(united.tweets)
plot(united.scores$score, united.tweets$created, type="l")
united.tweets$created
#######################################################
mean(united.scores$score)
##keep track of mean score for the day and make a data base
### on 11-23-2014 11:40 PM PST Mean=[1] 0.106
### on 11-24-2014 11:40 PM PST Mean= 
#dataset<-c(0.106,x )