library(twitteR)
library(ROAuth)
library(RCurl)

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

requestURL<-"https://api.twitter.com/oauth/request_token"
accessURL<- "https://api.twitter.com/oauth/access_token"
authURL<-"https://api.twitter.com/oauth/authorize"
consumerKey<-"UshGitw4GYPSttaDTYnKqV3da"
consumerSecret<-"c8DYRd4JagzkXrj0Yt25OQRcO4qilqkzmnw8j32cR1dbwublA0"

twitCred<-OAuthFactory$new(consumerKey=consumerKey, consumerSecret=consumerSecret, requestURL=requestURL, accessURL=accessURL, authURL=authURL)

twitCred$handshake(cainfo="cacert.pem")

registerTwitterOAuth(twitCred)

save(list="twitCred", file="twitteR_credentials")