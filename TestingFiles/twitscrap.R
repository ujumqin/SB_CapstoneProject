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

consumer_key <- "iIqt5d6bpRKF37RaWdyqJLAWJ"

consumer_secret <- "qQc7iiYZddit7EBhnJEmJmTGSyDiK3MEIWqjW1x3CwJnmy7C6u"

access_key <- "396944590-saMC6Z5r1dtjLUkz7AHKQBCgIBlb9T7czf1XaCwK"

access_secret <- "QYldXZMWypffxe2zbqOf00Y92SyG9PdOkhdtfL9WVM0X7"

setup_twitter_oauth(consumer_key, consumer_secret, access_token=access_key, access_secret=access_secret)


1# Grab latest tweets
tweets_OW <- searchTwitter('#overwatch', n=100000)

df_tweets_OW <- twListToDF(tweets_OW)
write.csv(df_tweets_OW, file = "OWT_9-22.csv")



# Loop over tweets and extract text
library(plyr)
OW_tweets = ldply(tweets_OW, function(t) t$toDataFrame())
write.csv(OW_tweets, file = "rawOWT_9-22.csv")

#feed_OW = laply(tweets_OW, function(t) t$getText())

write.csv(tweets_OW, file = "rawOWT_9-22")
#write.csv(feed_OW, file = "OW922.csv")
