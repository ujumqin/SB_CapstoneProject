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

#read owforums data into R. Ensure they are not factors
owforums <- read.csv("OWFORUMS12_22_FINAL.csv", stringsAsFactors = FALSE)

#duplicating the text column so we have a copy of the text. this text is destroyed during tokenization
owforums$text_topic <- owforums$text

#tokenize the text column
tidy_owforums <- owforums %>%
  group_by(X.) %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word, text) %>%
  ungroup()

#get the sentiment for the tokenized words
owforums_sentimentbing <- tidy_owforums %>%
  inner_join(get_sentiments("bing"))

#create new columns to use in for loop calculations. we will calculate the sentiment score by adding all the sentiments and dividing by number of sentiments.
owforums_sentimentbing$score <- 0
owforums_sentimentbing$count <- 1

#this for loops codes the sentiment from bing sentiment into a -1 or 1. 1 is positive and -1 is negative. Also counts the number of sentiment words to be used later.
for(i in 1:length(owforums_sentimentbing$sentiment)) {
  if(owforums_sentimentbing$sentiment[i] == "negative") {
    print("negative")
    owforums_sentimentbing$score[i] = -1
  } else {
    print("positive")
    owforums_sentimentbing$score[i] = 1
  }
}

#this calculates the sentiment score by adding up all sentiment values and dividing it by the total for a single post. X. is used since it identifies if a word belongs to a single post.
owforums_sentimentbing <- owforums_sentimentbing %>%
  group_by(X.) %>%
  mutate(sentimentscore = sum(score)/sum(count))

#identify the negative values. this is used later to predict negative sentiment
owforums_sentimentbing$negative <- as.factor(owforums_sentimentbing$sentimentscore < 0)
table(owforums_sentimentbing$negative)

#collapse the duplicated columns by text_topic
owsentiment <- owsentiment[!duplicated(owsentiment$forum_text),]

#create a new df without all of the unnecessary data
owsentiment <- data.frame(owforums_sentimentbing$text_topic, owforums_sentimentbing$sentimentscore, owforums_sentimentbing$negative)
names(owsentiment) <- c("forum_text","sentiment_score","negative")


#Create a corpus and remove unnecessary words/text
#Corpus is necessary to do predictive analytics
owcorpus <- Corpus(VectorSource(owsentiment$forum_text))
inspect(owcorpus[[100]])
owcorpus <- tm_map(owcorpus, removePunctuation)
owcorpus <- tm_map(owcorpus, tolower)
owcorpus <- tm_map(owcorpus, removeWords, c("mercy", "bastion", stopwords("english")))
owcorpus <- tm_map(owcorpus, stemDocument)

#Find frequencies of words
owfreq <- DocumentTermMatrix(owcorpus)

#removing the sparse terms
owsparse <- removeSparseTerms(owfreq, 0.995)

#convert sparse data into a data frame
owpostSparses <- as.data.frame(as.matrix(owsparse))

#converts variable names to appropriate names (some may start with numbers)
colnames(owpostSparses) = make.names(colnames(owpostSparses))

write.csv(owpostSparses, file="owpostspare.csv")

#add dependent variable to the dataframe. We will be predicting this value.
owpostSparses$negative <- owsentiment$negative

#setting a seed to get consistent results when running multiple times
set.seed(1113)

#split the data into a training set and a test set. 
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
forumCART2 <- rpart(negative ~., data=trainow, method="class",control=rpart.control(cp=0.0025))
prp(forumCART2, fallen.leaves = FALSE, tweak = 1.0, compress = TRUE, ycompress = TRUE, box.palette = "auto")


#testing model against test data. table is outputted for each model
predictForumCart2 <- predict(forumCART2, newdata = testow, type = "class")
table(testow$negative, predictForumCart2)
(339+85)/(339+45+130+85)

predictForumCart <- predict(forumCART, newdata=testow, type="class")
table(testow$negative, predictForumCart)
(364+48)/(364+23+167+48)

#takes a long time to run!
forumRF <- randomForest(negative ~., data = trainow)
predictRF <- predict(forumRF, newdata = testow)

table(testow$negative, predictRF)

#using this data, we can predict whether a Overwatch forum post will be negative or positive. This can be useful in the automatic moderation of a forum post.
#This model is falliable since we are relying on the bing lexicon to determine whether language is positive or negative.
#Given the nature of Overwatch, many words will be incorrectly identified. For example 'kill' is not necessarily a negative word and may be used to describe
#a kill in the Overwatch game, which may have a positive or negative connotation. Another example is Mercy, which is a character in Overwatch and can have
#both positive and negative connotations.

#The best accuracy given our data for this model is:
#70% from the CART method
#77% from the forest method