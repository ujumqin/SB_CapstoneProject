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
twitter <- read.csv("OWT_12-22.csv", stringsAsFactors = FALSE)
str(twitter)

#duplicating the text column so we have a copy of the text
twitter$text_topic <- twitter$text

#tokenize the text column
tidy_twitter <- twitter %>%
  group_by(twitter$text_topic) %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word, text) %>%
  ungroup()

#get the sentiment for the tokenized words
twitter_sentimentbing <- tidy_twitter %>%
  inner_join(get_sentiments("bing"))

#create new columns to use in for loop calculations
twitter_sentimentbing$score <- 0
twitter_sentimentbing$count <- 1

#this for loops codes the sentiment from bing into a -1 or 1. 1 is positive and -1 is negative. Also counts the number of sentiment words to be used later.
for(i in 1:length(twitter_sentimentbing$sentiment)) {
  if(twitter_sentimentbing$sentiment[i] == "negative") {
    print("negative")
    twitter_sentimentbing$score[i] = -1
  } else {
    print("positive")
    twitter_sentimentbing$score[i] = 1
  }
}

#this shows us what sentiment is for each post and groups it by post
twitter_sentimentbing <- twitter_sentimentbing %>%
  group_by(text_topic) %>%
  mutate(sentimentscore = sum(score)/sum(count))

#identify the negative values
twitter_sentimentbing$negative <- as.factor(twitter_sentimentbing$sentimentscore < 0)
table(twitter_sentimentbing$negative)

#collapse the duplicated columns by text_topic
twitter_sentimentbing <- twitter_sentimentbing[!duplicated(twitter_sentimentbing$text_topic),]

str(twitter_sentimentbing)

#create a new df without all of the unnecessary data
twittersentiment <- data.frame(twitter_sentimentbing$text_topic, twitter_sentimentbing$sentimentscore, twitter_sentimentbing$negative)
names(twittersentiment) <- c("forum_text","sentiment_score","negative")


#test code
write.csv(twittersentiment, file="twitterduplicatesgone.csv")


#Create a corpus and remove unnecessary words/text
#Corpus is necessary to do predictive analytics
owcorpus <- Corpus(VectorSource(twittersentiment$forum_text))
inspect(owcorpus[[10]])
owcorpus <- tm_map(owcorpus, removePunctuation)
owcorpus <- tm_map(owcorpus, tolower)
owcorpus <- tm_map(owcorpus, removeWords, c("mercy", "bastion", stopwords("english")))
owcorpus <- tm_map(owcorpus, stemDocument)

#Find frequencies
owfreq <- DocumentTermMatrix(owcorpus)

####test code to see frequencies
owfreq
inspect(owfreq[1000:1005, 505:515])
findFreqTerms(owfreq, lowfreq = 20)
###############

#removing the sparse terms
owsparse <- removeSparseTerms(owfreq, 0.995)
#.995 means keep terms that appear in .5 percent of the posts
owsparse

#check the sparse data
inspect(owsparse[1:10, 20:30])

#convert sparse data into a data frame
owpostSparses <- as.data.frame(as.matrix(owsparse))

#converts variable names to appropriate names (some may start with numbers)
colnames(owpostSparses) = make.names(colnames(owpostSparses))

write.csv(owpostSparses, file="owpostspare.csv")
#add dependent variable
owpostSparses$negative <- twittersentiment$negative

#setting a seed to get consistent results when running multiple times
set.seed(1113)

split <- sample.split(owpostSparses$negative, SplitRatio = 0.8)
trainow <- subset(owpostSparses, split == TRUE)
testow <- subset(owpostSparses, split == FALSE)


#First model created. Default values used.
forumCART <- rpart(negative ~., data=trainow, method="class")
#plot the model
prp(forumCART)

#method for obtaining CP to try to fit a better model using cross-validation
fitOWControl = trainControl(method="cv",number=10)
cartGrid <- expand.grid(.cp=(1:50)*0.00001)
train(negative ~., data=trainow, method="rpart", trControl=fitOWControl, tuneGrid=cartGrid)

#second model created using recommended cp value
forumCART2 <- rpart(negative ~., data=trainow, method="class",control=rpart.control(cp=0.00038))
prp(forumCART2, fallen.leaves = FALSE, tweak = 1.0, compress = TRUE, ycompress = TRUE, box.palette = "auto")

#!!!!!!!!!!!!Tree model.WARNING! TAKES A LONG TIME TO RUN!!!!!!
forumRF <- randomForest(negative ~., data = trainow)

#testing model against test data. table is outputted for each model
predictForumCart <- predict(forumCART, newdata=testow, type="class")
table(testow$negative, predictForumCart)
(1063+109)/(1063+109+34+370)

predictForumCart2 <- predict(forumCART2, newdata = testow, type = "class")
table(testow$negative, predictForumCart2)
(1057+143)/(1057+143+40+336)

predictRF <- predict(forumRF, newdata = testow)
table(testow$negative, predictRF)
(989+324)/(989+324+108+155)
