library(twitteR)
load("twitteR_credentials")
registerTwitterOAuth(twitCred)
tws<-searchTwitter('@fed',n=10)
df <- do.call("rbind", lapply(tws, as.data.frame))
df

df_text1=df[,"text", drop=F]
df_text
rownames(df_text1)=df$created
df_text