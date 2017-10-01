# Grab latest tweets
tweets_OW <- searchTwitter('#overwatch', n=1500)

# Loop over tweets and extract text
library(plyr)
feed_OW = laply(tweets_OW, function(t) t$getText())
