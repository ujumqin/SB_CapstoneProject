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
all <- read.csv("all.csv", stringsAsFactors = FALSE)

#duplicating the text column so we have a copy of the text
all$text_topic <- all$text

#tokenize the text column
tidy_all <- all %>%
  group_by(all$text_topic) %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word, text) %>%
  ungroup()

#get the sentiment for the tokenized words
all_sentimentbing <- tidy_all %>%
  inner_join(get_sentiments("bing"))

#create new columns to use in for loop calculations
all_sentimentbing$score <- 0
all_sentimentbing$count <- 1

#this for loops codes the sentiment from bing into a -1 or 1. 1 is positive and -1 is negative. Also counts the number of sentiment words to be used later.
for(i in 1:length(all_sentimentbing$sentiment)) {
  if(all_sentimentbing$sentiment[i] == "negative") {
    print("negative")
    all_sentimentbing$score[i] = -1
  } else {
    print("positive")
    all_sentimentbing$score[i] = 1
  }
}

#this shows us what sentiment is for each post and groups it by post
all_sentimentbing <- all_sentimentbing %>%
  group_by(text_topic) %>%
  mutate(sentimentscore = sum(score)/sum(count))

#identify the negative values
all_sentimentbing$negative <- as.factor(all_sentimentbing$sentimentscore < 0)
table(all_sentimentbing$negative)

#collapse the duplicated columns by text_topic
all_sentimentbing <- all_sentimentbing[!duplicated(all_sentimentbing$text_topic),]

str(all_sentimentbing)

#create a new df without all of the unnecessary data
allsentiment <- data.frame(all_sentimentbing$text_topic, all_sentimentbing$sentimentscore, all_sentimentbing$negative)
names(allsentiment) <- c("forum_text","sentiment_score","negative")


#test code
write.csv(allsentiment, file="allduplicatesgone.csv")


#Create a corpus and remove unnecessary words/text
#Corpus is necessary to do predictive analytics
owcorpus <- Corpus(VectorSource(allsentiment$forum_text))
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
owpostSparses$negative <- allsentiment$negative

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
forumCART2 <- rpart(negative ~., data=trainow, method="class",control=rpart.control(cp=0.00050))
prp(forumCART2, fallen.leaves = FALSE, tweak = 1.0, compress = TRUE, ycompress = TRUE, box.palette = "auto")

#!!!!!!!!!!!!Tree model.WARNING! TAKES A LONG TIME TO RUN!!!!!!
forumRF <- randomForest(negative ~., data = trainow)

#testing model against test data. table is outputted for each model
predictForumCart <- predict(forumCART, newdata=testow, type="class")
table(testow$negative, predictForumCart)
(1644+94)/(1644+49+710+94)

predictForumCart2 <- predict(forumCART2, newdata = testow, type = "class")
table(testow$negative, predictForumCart2)
(1627+163)/(1627+163+66+641)

predictRF <- predict(forumRF, newdata = testow)
table(testow$negative, predictRF)
(1539+539)/(265+154+1539+539)
