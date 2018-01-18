library(twitteR)
library(ROAuth)
# Download "cacert.pem" file
download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")

cred <- OAuthFactory$new(consumerKey="xxxxx",
                         consumerSecret="xxxx",
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

# Executing the next step generates an output --> To enable the connection, please direct your web browser to: <hyperlink> . Note:  You only need to do this part once
cred$handshake(cainfo="cacert.pem")

#save for later use for Windows
save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")
#OLD- registerTwitterOAuth(cred)

consumer_key <- "xxxxxxxxxxxxxxxxxx"

consumer_secret <- "xxxxxxxxxxxxxxxxxx"

access_key <- "xxxxxxxxxxxxxxxxxx-xxxxxxxxxxxxxxxxxx"

access_secret <- "xxxxxxxxxxxxxxxxxx"

setup_twitter_oauth(consumer_key, consumer_secret, access_token=access_key, access_secret=access_secret)


# Grab latest tweets
print("Grabbing tweets!!!!! Will take a second.")
tweets_OW <- searchTwitter("#overwatch, -filter:retweets", n=14586, lang = "en")

df_tweets_OW <- twListToDF(tweets_OW)
write.csv(df_tweets_OW, file = "OWT_12-22.csv")





