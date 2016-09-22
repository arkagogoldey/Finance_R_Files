library(streamR)
library(ROAuth)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "UshGitw4GYPSttaDTYnKqV3da"
consumerSecret <- "c8DYRd4JagzkXrj0Yt25OQRcO4qilqkzmnw8j32cR1dbwublA0"
my_oauth <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret,
                             requestURL=requestURL,
                             accessURL=accessURL,
                             authURL=authURL)
twitCred$handshake(cainfo="cacert.pem")
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
save(my_oauth, file = "my_oauth.Rdata")
#streaming in live tweets for 10 seconds
filterStream("tweets.json", track=c("Obama","Biden"), timeout=20, oauth=my_oauth)

tweeets.df<-parseTweets("tweets.json", simplify=T)

c(length(grep("obama", tweeets.df$text, ignore.case=T)),length(grep("biden", tweeets.df$text, ignore.case = T)))

filterStream("tweetsUS.json", locations=c(-125, 25, -66, 50), timeout = 300, oauth=my_oauth)

tweets.df <- parseTweets("tweetsUS.json", verbose = FALSE)
library(ggplot2)
library(grid)
install.packages('maps')
library(maps)
map.data <- map_data("state")
points <- data.frame(x = as.numeric(tweets.df$lon), y = as.numeric(tweets.df$lat))
points <- points[points$y > 25, ]
ggplot(map.data) + geom_map(aes(map_id = region), map = map.data, fill = "white", color = "grey20", size = 0.25) + expand_limits(x = map.data$long, y = map.data$lat) + theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(), plot.background = element_blank(), plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))+geom_point(data = points, aes(x = x, y = y), size = 1, alpha = 1/5, color = "darkblue")

