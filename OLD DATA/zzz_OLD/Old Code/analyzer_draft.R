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
owforums <- read.csv("OWFORUMS12_22_FINAL.csv", stringsAsFactors = FALSE)
twitter <- read.csv("OWT_12-22.csv", stringsAsFactors = FALSE)
reddit <- read.csv("reddit12_22.csv", stringsAsFactors = FALSE)

#=================Solution to problem...hopefully=================
#XXFirst create a duplicate column for the text and call it content_text
#XXTokenize the text column
#XXGroup by the text column. this will allow us to create a sentiment score for the text comment, without destroying the text in tokenization
#XXDetermine the score for the text. This will be useful in machine learning later
#Remove stopwords, punctuation. Change to lowercase and stem the words
#Start the analytics!

#duplicating the text column so we have a copy of the text
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

#create new columns to use in for loop calculations
owforums_sentimentbing$score <- 0
owforums_sentimentbing$count <- 1

#this for loops codes the sentiment from bing into a -1 or 1. 1 is positive and -1 is negative. Also counts the number of sentiment words to be used later.
for(i in 1:length(owforums_sentimentbing$sentiment)) {
  if(owforums_sentimentbing$sentiment[i] == "negative") {
    print("negative")
    owforums_sentimentbing$score[i] = -1
  } else {
    print("positive")
    owforums_sentimentbing$score[i] = 1
  }
}

#this shows us what sentiment is for each post and groups it by post
owforums_sentimentbing <- owforums_sentimentbing %>%
  group_by(X.) %>%
  mutate(sentimentscore = sum(score)/sum(count))

#identify the negative values
owforums_sentimentbing$negative <- as.factor(owforums_sentimentbing$sentimentscore < 0)
table(owforums_sentimentbing$negative)

#create a new df without all of the unnecessary data
owsentiment <- data.frame(owforums_sentimentbing$text_topic, owforums_sentimentbing$sentimentscore, owforums_sentimentbing$negative)
names(owsentiment) <- c("forum_text","sentiment_score","negative")

#collapse the duplicated columns by text_topic
owsentiment <- owsentiment[!duplicated(owsentiment$forum_text),]

#test code
write.csv(owsentiment, file="duplicatesgone.csv")


#Create a corpus and remove unnecessary words/text
#Corpus is necessary to do predictive analytics
owcorpus <- Corpus(VectorSource(owsentiment$forum_text))
inspect(owcorpus[[100]])
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
owpostSparses$negative <- owsentiment$negative

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







