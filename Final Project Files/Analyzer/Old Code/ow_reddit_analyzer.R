library(dplyr)
library(tidytext)
library(tm)
library(SnowballC)
library(ggplot2)
library(caTools)
library(rpart.plot)
library(randomForest)
library(caret)
library(e1071)

#read all three files into R. Ensure they are not factors
reddit <- read.csv("reddit12_22.csv", stringsAsFactors = FALSE)

#=================Solution to problem...hopefully=================


#duplicating the text column so we have a copy of the text
str(reddit)
reddit$text_topic <- reddit$X.text.

#tokenize the text column
tidy_reddit <- reddit %>%
  group_by(text_topic) %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word, X.text.) %>%
  ungroup()

#get the sentiment for the tokenized words
reddit_sentimentbing <- tidy_reddit %>%
  inner_join(get_sentiments("bing"))

#create new columns to use in for loop calculations
reddit_sentimentbing$score <- 0
reddit_sentimentbing$count <- 1

#this for loops codes the sentiment from bing into a -1 or 1. 1 is positive and -1 is negative. Also counts the number of sentiment words to be used later.
for(i in 1:length(reddit_sentimentbing$sentiment)) {
  if(reddit_sentimentbing$sentiment[i] == "negative") {
    print("negative")
    reddit_sentimentbing$sentiment[i] = -1
  } else {
    print("positive")
    reddit_sentimentbing$sentiment[i] = 1
  }
}


#convert sentiment into a double since it was converted into a character somehow...
reddit_sentimentbing$sentiment <- as.numeric(reddit_sentimentbing$sentiment)


#this shows us what sentiment is for each post and groups it by post
reddit_sentimentbing <- reddit_sentimentbing %>%
  group_by(text_topic) %>%
  mutate(sentimentscore = sum(sentiment)/sum(count))


#identify the negative values
reddit_sentimentbing$negative <- as.factor(reddit_sentimentbing$sentimentscore < 0)
table(reddit_sentimentbing$negative)

write.csv(reddit_sentimentbing, file="reddsentibing.csv")
str(reddit_sentimentbing)

#collapse the duplicated columns by text_topic
reddit_sentimentbing <- reddit_sentimentbing[!duplicated(reddit_sentimentbing$text_topic),]

#create a new df without all of the unnecessary data
redditsentiment <- data.frame(reddit_sentimentbing$text_topic, reddit_sentimentbing$sentimentscore, reddit_sentimentbing$negative)
names(redditsentiment) <- c("forum_text","sentiment_score","negative")

#test code
write.csv(redditsentiment, file="redditduplicatesgone2.csv")

#----------------AFINN SENTIMENT----------------#
#Get sentiment for each word using the afinn lexicon
reddit_sentimentafinn <- tidy_reddit %>%
  inner_join(get_sentiments("afinn"))

#create this column to help calculate sentiment score 
reddit_sentimentafinn$count <- 1

#calculate sentiment score using afinn
reddit_sentimentafinn <- reddit_sentimentafinn %>%
  group_by(X.) %>%
  mutate(sentimentscore = sum(score)/sum(count))

#collapse the duplicated columns by text_topic
reddit_sentimentafinn <- reddit_sentimentafinn[!duplicated(reddit_sentimentafinn$text_topic),]

write.csv(reddit_sentimentafinn, file = "reddit_afinn_sentiment.csv")


#Create a corpus and remove unnecessary words/text
#Corpus is necessary to do predictive analytics
redditcorpus <- Corpus(VectorSource(redditsentiment$forum_text))
inspect(redditcorpus[[110]])
redditcorpus <- tm_map(redditcorpus, removePunctuation)
redditcorpus <- tm_map(redditcorpus, tolower)
redditcorpus <- tm_map(redditcorpus, removeWords, c("mercy", "bastion", stopwords("english")))
redditcorpus <- tm_map(redditcorpus, stemDocument)

#Find frequencies
redditfreq <- DocumentTermMatrix(redditcorpus)

####test code to see frequencies
redditfreq
inspect(redditfreq[1000:1005, 505:515])
findFreqTerms(redditfreq, lowfreq = 20)
###############

#removing the sparse terms
redditparse <- removeSparseTerms(redditfreq, 0.995)
#.995 means keep terms that appear in .5 percent of the posts
redditparse

#check the sparse data
inspect(redditparse[1:10, 20:30])

#convert sparse data into a data frame
redditSparseDF <- as.data.frame(as.matrix(redditparse))

#converts variable names to appropriate names (some may start with numbers)
colnames(redditSparseDF) = make.names(colnames(redditSparseDF))

#add dependent variable
redditSparseDF$negative <- reddit_sentimentbing$negative

#setting a seed to get consistent results when running multiple times
set.seed(1113)

split <- sample.split(redditSparseDF$negative, SplitRatio = 0.8)
trainow <- subset(redditSparseDF, split == TRUE)
testow <- subset(redditSparseDF, split == FALSE)


#First model created. Default values used.
forumCART <- rpart(negative ~., data=trainow, method="class")
#plot the model
prp(forumCART)

#method for obtaining CP to try to fit a better model using cross-validation
fitOWControl = trainControl(method="cv",number=10)
cartGrid <- expand.grid(.cp=(1:50)*0.00001)
train(negative ~., data=trainow, method="rpart", trControl=fitOWControl, tuneGrid=cartGrid)

#second model created using recommended cp value
forumCART2 <- rpart(negative ~., data=trainow, method="class",control=rpart.control(cp=0.00013))
prp(forumCART2, fallen.leaves = FALSE, tweak = 1.0, compress = TRUE, ycompress = TRUE, box.palette = "auto")

#!!!!!!!!!!!!Tree model.WARNING! TAKES A LONG TIME TO RUN!!!!!!
forumRF <- randomForest(negative ~., data = trainow)


#testing model against test data. table is outputted for each model
predictForumCart <- predict(forumCART, newdata=testow, type="class")
table(testow$negative, predictForumCart)
(568+102)/(568+30+190+102)

predictForumCart2 <- predict(forumCART2, newdata = testow, type = "class")
table(testow$negative, predictForumCart2)
(580+152)/(580+18+140+152)

predictRF <- predict(forumRF, newdata = testow)
table(testow$negative, predictRF)
(585+259)/(585+13+33+259)






