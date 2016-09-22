##Importing data set
install.packages("xlsx")
library(xlsx)
New_EC<-read.xlsx("/Users/hisamsabouni2/Desktop/R/RE__IMF_meeting_followup/NEW EC.xlsx",sheetName="Sheet1",header=FALSE)

head(New_EC$X2)
new_ec<-as.data.frame(New_EC$X2)
head(new_ec1)
new_ec1<-as.data.frame(New_EC$X1)
newec<-cbind(new_ec1,new_ec)
newec
new<-newec
colnames(new_ec)<-c("Date","Statement")
colnames(new)
class(new$Statement)
#Convert text from factor to character strings.
new$Statement<-as.character(new$Statement)
class(new$Statement)


##Load in Word Lists For Sentiment Analysis
#positive words list
hu.liu.pos = scan('/Users/hisamsabouni2/Desktop/R/opinion-lexicon-English/positive-words.txt', what='character', comment.char=';')
class(hu.liu.pos)

#negative words list
hu.liu.neg = scan('/Users/hisamsabouni2/Desktop/R/opinion-lexicon-English/negative-words.txt', what='character', comment.char=';')
class(hu.liu.neg)


##define postive and negative words
pos.words =c(hu.liu.pos)
neg.words=c(hu.liu.neg)


##Clean up symbols in data
tryTolower = function(x)
{
	y= NA
	try_error = tryCatch(tolower(x), error = function(e) e)
	if(!inherits(try_error, "error"))
	y= tolower(x)
	return(y)
}

##Scoring Algorithm
library(plyr)
library(stringr)
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

## Test scoring system
sample=c("You're awesome and I love you", "I hate and hate and hate. So angry, Die!")
result= score.sentiment(sample, pos.words, neg.words)
result
##Success

##Run on data set
new.scores= score.sentiment(new$Statement, pos.words, neg.words, .progress='text')
new.scores<-cbind(new$Date, new.scores)
new.scores
hist(new.scores$score)

write.xlsx(new.scores,"/Users/hisamsabouni2/Desktop/R/RE__IMF_meeting_followup/New_EC_Scored.xlsx")

###////////END NEW EC FILE////////////####
rm(list=ls())
ls()
###///////Start NEW ECB WITHOUT TROIKA////////######
library(xlsx)
newecbt<-read.xlsx("/Users/hisamsabouni2/Desktop/R/RE__IMF_meeting_followup/NEW ECB without Troika.xlsx",sheetName="Sheet1",header=FALSE)
head(newecbt)

new_ecbt<-as.data.frame(newecbt$X2)
head(new_ecbt)

new_ecbt1<-as.data.frame(newecbt$X1)
head(new_ecbt1)
newecbt1<-cbind(new_ecbt1,new_ecbt)
head(newecbt1)
is.na(newecbt1)

colnames(newecbt1)<-c("Date","Statement")
colnames(newecbt1)
class(newecbt1$Statement)
#Convert text from factor to character strings.
newecbt1$Statement<-as.character(newecbt1$Statement)
class(newecbt1$Statement)

##Load in Word Lists For Sentiment Analysis
#positive words list
hu.liu.pos = scan('/Users/hisamsabouni2/Desktop/R/opinion-lexicon-English/positive-words.txt', what='character', comment.char=';')
class(hu.liu.pos)

#negative words list
hu.liu.neg = scan('/Users/hisamsabouni2/Desktop/R/opinion-lexicon-English/negative-words.txt', what='character', comment.char=';')
class(hu.liu.neg)


##define postive and negative words
pos.words =c(hu.liu.pos)
neg.words=c(hu.liu.neg)


##Clean up symbols in data
tryTolower = function(x)
{
	y= NA
	try_error = tryCatch(tolower(x), error = function(e) e)
	if(!inherits(try_error, "error"))
	y= tolower(x)
	return(y)
}

##Scoring Algorithm
library(plyr)
library(stringr)
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

## Test scoring system
sample=c("You're awesome and I love you", "I hate and hate and hate. So angry, Die!")
result= score.sentiment(sample, pos.words, neg.words)
result
##Success

##Run on data set
newecbt.scores= score.sentiment(newecbt1$Statement, pos.words, neg.words, .progress='text')
newecbt.scores<-cbind(newecbt1$Date, newecbt.scores)
newecbt.scores
hist(newecbt.scores$score)
is.na(newecbt.scores)
write.xlsx(newecbt.scores,"/Users/hisamsabouni2/Desktop/R/RE__IMF_meeting_followup/New_ECB_WITHOUT_TROIKA_Scored.xlsx")
###////////END NEW ECB WITHOUT TROIKA FILE////////////####
rm(list=ls())
ls()
###///////Start NEW IMF WITHOUT TROIKA////////######
library(xlsx)
newimfwt<-read.xlsx("/Users/hisamsabouni2/Desktop/R/RE__IMF_meeting_followup/NEW IMF without Troika.xlsx",sheetName="Sheet1",header=FALSE)
head(newimfwt)

new_imfwt<-as.data.frame(newimfwt$X2)
head(new_imfwt)

new_imfwt1<-as.data.frame(newimfwt$X1)
head(new_imfwt1)

newimftw1<-cbind(new_imfwt1,new_imfwt)
head(newimftw1)
is.na(newimftw1)

colnames(newimftw1)<-c("Date","Statement")
colnames(newimftw1)
class(newimftw1$Statement)
#Convert text from factor to character strings.
newimftw1$Statement<-as.character(newimftw1$Statement)
class(newimftw1$Statement)

##Load in Word Lists For Sentiment Analysis
#positive words list
hu.liu.pos = scan('/Users/hisamsabouni2/Desktop/R/opinion-lexicon-English/positive-words.txt', what='character', comment.char=';')
class(hu.liu.pos)

#negative words list
hu.liu.neg = scan('/Users/hisamsabouni2/Desktop/R/opinion-lexicon-English/negative-words.txt', what='character', comment.char=';')
class(hu.liu.neg)


##define postive and negative words
pos.words =c(hu.liu.pos)
neg.words=c(hu.liu.neg)


##Clean up symbols in data
tryTolower = function(x)
{
	y= NA
	try_error = tryCatch(tolower(x), error = function(e) e)
	if(!inherits(try_error, "error"))
	y= tolower(x)
	return(y)
}

##Scoring Algorithm
library(plyr)
library(stringr)
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

## Test scoring system
sample=c("You're awesome and I love you", "I hate and hate and hate. So angry, Die!")
result= score.sentiment(sample, pos.words, neg.words)
result
##Success

##Run on data set
newimftw.scores= score.sentiment(newimftw1$Statement, pos.words, neg.words, .progress='text')
newimftw.scores<-cbind(newimftw1$Date, newimftw.scores)
newimftw.scores
hist(newimftw.scores$score)

write.xlsx(newimftw.scores,"/Users/hisamsabouni2/Desktop/R/RE__IMF_meeting_followup/New_IMF_WITHOUT_TROIKA_Scored.xlsx")

###////////END NEW IMF WITHOUT TROIKA FILE////////////####
rm(list=ls())
ls()
###///////Start NEW TROIKA////////######
library(xlsx)
newt<-read.xlsx("/Users/hisamsabouni2/Desktop/R/RE__IMF_meeting_followup/NEW Troika.xlsx",sheetName="Sheet1",header=FALSE)
head(newt)

new_t<-as.data.frame(newt$X2)
head(new_t)

new_t1<-as.data.frame(newt$X1)
head(new_t1)

newt1<-cbind(new_t1,new_t)
head(newt1)
is.na(newt1)

colnames(newt1)<-c("Date","Statement")
colnames(newt1)
class(newt1$Statement)
#Convert text from factor to character strings.
newt1$Statement<-as.character(newt1$Statement)
class(newt1$Statement)

##Load in Word Lists For Sentiment Analysis
#positive words list
hu.liu.pos = scan('/Users/hisamsabouni2/Desktop/R/opinion-lexicon-English/positive-words.txt', what='character', comment.char=';')
class(hu.liu.pos)

#negative words list
hu.liu.neg = scan('/Users/hisamsabouni2/Desktop/R/opinion-lexicon-English/negative-words.txt', what='character', comment.char=';')
class(hu.liu.neg)


##define postive and negative words
pos.words =c(hu.liu.pos)
neg.words=c(hu.liu.neg)


##Clean up symbols in data
tryTolower = function(x)
{
	y= NA
	try_error = tryCatch(tolower(x), error = function(e) e)
	if(!inherits(try_error, "error"))
	y= tolower(x)
	return(y)
}

##Scoring Algorithm
library(plyr)
library(stringr)
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

## Test scoring system
sample=c("You're awesome and I love you", "I hate and hate and hate. So angry, Die!")
result= score.sentiment(sample, pos.words, neg.words)
result
##Success

##Run on data set
newt.scores= score.sentiment(newt1$Statement, pos.words, neg.words, .progress='text')
newt.scores<-cbind(newt1$Date, newt.scores)
newt.scores
hist(newt.scores$score)

write.xlsx(newt.scores,"/Users/hisamsabouni2/Desktop/R/RE__IMF_meeting_followup/New_TROIKA_Scored.xlsx")

