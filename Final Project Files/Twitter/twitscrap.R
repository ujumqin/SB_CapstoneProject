library(twitteR)
library(ROAuth)
# Download "cacert.pem" file
download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")

cred <- OAuthFactory$new(consumerKey="iIqt5d6bpRKF37RaWdyqJLAWJ",
                         consumerSecret="qQc7iiYZddit7EBhnJEmJmTGSyDiK3MEIWqjW1x3CwJnmy7C6u",
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

# Executing the next step generates an output --> To enable the connection, please direct your web browser to: <hyperlink> . Note:  You only need to do this part once
cred$handshake(cainfo="cacert.pem")

#save for later use for Windows
save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")
#OLD- registerTwitterOAuth(cred)

consumer_key <- ""

consumer_secret <- ""

access_key <- ""

access_secret <- ""

setup_twitter_oauth(consumer_key, consumer_secret, access_token=access_key, access_secret=access_secret)


# Grab latest tweets
print("Grabbing tweets!!!!! Will take a second.")
tweets_OW <- searchTwitter("#overwatch, -filter:retweets", n=100000, lang = "en")

df_tweets_OW <- twListToDF(tweets_OW)
write.csv(df_tweets_OW, file = "OWT_9-30_4thscrape.csv")





