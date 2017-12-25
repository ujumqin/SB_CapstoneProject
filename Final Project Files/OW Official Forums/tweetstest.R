library(tm)
library(SnowballC)

tweets = read.csv("tweets.csv", stringsAsFactors = FALSE)
tweets$Negative = as.factor(tweets$Avg <= -1)

corpus <- Corpus(VectorSource(tweets$Tweet))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus <- tm_map(corpus, stemDocument)

frequencies <- DocumentTermMatrix(corpus)
sparse <- removeSparseTerms(frequencies, 0.995)

tweetsSparse <- as.data.frame(as.matrix(sparse))
write.csv(tweetsSparse, file="tweetssparse.csv")

colnames(tweetsSparse) <- make.names(colnames(tweetsSparse))
tweetsSparse$Negative <- tweets$Negative

library()