library(dplyr)
library(tidytext)
library(tm)
library(caTools)
library(rpart.plot)

#-------------------------Reading Files Into R-------------------------#
#We are only interested in using reddit and forums data. Twitter was found to contain 
#irrelevant data.

#read files into R. Ensure they are not factors
forums <- read.csv("OWFORUMS12_22_FINAL.csv", stringsAsFactors = FALSE)
reddit <- read.csv("REDDIT_12_22_FINAL.csv", stringsAsFactors = FALSE)
forandred <- read.csv("OWFORUMS&REDDIT_12_22_FINAL.csv", stringsAsFactors = FALSE)

#-------------------------Tokenize Data-------------------------#
#duplicating the text column so we have a copy of the text. 
#This gets destroyed during tokenization, but we need it to group by.
forums$text_topic <- forums$text
reddit$text_topic <- reddit$X.text.
forandred$text_topic <- forandred$text

#tokenize the text column and group by text of each comment (text_topic)
tidy_forums <- forums %>%
  group_by(text_topic) %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word, text) %>%
  ungroup()

tidy_reddit <- reddit %>%
  group_by(text_topic) %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word, X.text.) %>%
  ungroup()

tidy_forandred <- forandred %>%
  group_by(text_topic) %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word, text) %>%
  ungroup()

#-------------------------Bing Sentiment Function-------------------------#

#bing maker is a function to get bing sentiment from tidy data
bing_maker <- function(tidy_data) {
  bing_data <- tidy_data %>%
    inner_join(get_sentiments("bing"))
  
  #create new columns to use in for loop calculations. we will calculate the sentiment score by adding all the sentiments and dividing by number of sentiments.
  bing_data$score <- 0
  bing_data$count <- 1
  
  #for loops codes the sentiment from bing sentiment into a -1 or 1. 1 is positive and -1 is negative. Also counts the number of sentiment words to be used later.
  for(i in 1:length(bing_data$sentiment)) {
    if(bing_data$sentiment[i] == "negative") {
      bing_data$score[i] = -1
    } else {
      bing_data$score[i] = 1
    }
  }
  
  #calculates the sentiment score by adding up all sentiment values and dividing it by the total for a single post. text_topic is used since it identifies if a word belongs to a single comment.
  bing_data <- bing_data %>%
    group_by(text_topic) %>%
    mutate(sentimentscore = sum(score)/sum(count))
  
  #identify the negative values. used later to predict negative sentiment
  bing_data$negative <- as.factor(bing_data$sentimentscore < 0)
  
  #collapse the duplicated columns by text_topic
  bing_data <- bing_data[!duplicated(bing_data$text_topic),]
  
  bing_data <- data.frame(bing_data$text_topic, bing_data$sentimentscore, bing_data$negative)
  names(bing_data) <- c("forum_text","sentiment_score","negative")
  return(bing_data)
}

#--------------------Perform bing--------------------#
forum_bing <- bing_maker(tidy_forums)
reddit_bing <- bing_maker(tidy_reddit)
forandred_bing <- bing_maker(tidy_forandred)

#-------------------------Afinn Sentiment Function-------------------------#
#afinn maker is a function to get afinn sentiment from tidy data
afinn_maker <- function(tidy_data) {
  afinn_data <- tidy_data %>%
    inner_join(get_sentiments("afinn"))
  
  #create this column to help calculate sentiment score 
  afinn_data$count <- 1

  #calculate sentiment score using afinn
  afinn_data <- afinn_data %>%
    group_by(text_topic) %>%
    mutate(sentimentscore = sum(score)/sum(count))
  
  #collapse the duplicated columns by text_topic
  afinn_data <- afinn_data[!duplicated(afinn_data$text_topic),]
  
  afinn_data <- data.frame(afinn_data$text_topic, afinn_data$sentimentscore)
  names(afinn_data) <- c("forum_text","sentiment_score")
  
  afinn_data$negative <- 0
  
  #for loops codes the sentiment from bing sentiment into a -1 or 1. 1 is positive and -1 is negative. Also counts the number of sentiment words to be used later.
  for(i in 1:length(afinn_data$sentiment_score)) {
    if(afinn_data$sentiment_score[i] < 0) {
      afinn_data$negative[i] = "TRUE"
    } else {
      afinn_data$negative[i] = "FALSE"
    }
  }
  
  return(afinn_data)
}

#--------------------Perform afinn--------------------#
forum_afinn <- afinn_maker(tidy_forums)
reddit_afinn <- afinn_maker(tidy_reddit)
forandred_afinn <- afinn_maker(tidy_forandred)

#----------------PREP DATA FOR MACHINE LEARNING----------------#

#----------------Corpus Maker----------------#
#Function to reate a corpus and remove unnecessary words/text
#sentiment_data should be the dataset created from bing or afinn maker above
corpus_maker <- function(sentiment_data) {
  data_corpus <- Corpus(VectorSource(sentiment_data$forum_text))
  data_corpus <- tm_map(data_corpus, removePunctuation)
  data_corpus <- tm_map(data_corpus, tolower)
  data_corpus <- tm_map(data_corpus, removeWords, stopwords("english"))
  data_corpus <- tm_map(data_corpus, stemDocument)
  return(data_corpus)
}

#----------------Make Corpus----------------#
bforum_corpus <- corpus_maker(forum_bing)
breddit_corpus <- corpus_maker(reddit_bing)
bforandred_corpus <- corpus_maker(forandred_bing)

aforum_corpus <- corpus_maker(forum_afinn)
areddit_corpus <- corpus_maker(reddit_afinn)
aforandred_corpus <- corpus_maker(forandred_afinn)


#----------------Process Corpus Function----------------#
#Function to process the corpus

process_corpus <- function(corpus_name, bing_name) {
  #Find frequencies of words
  freq <- DocumentTermMatrix(corpus_name)
  
  #removing the sparse terms
  sparse <- removeSparseTerms(freq, 0.995)
  
  #convert sparse data into a data frame
  sparse <- as.data.frame(as.matrix(sparse))
  
  #converts variable names to appropriate names (some may start with numbers)
  colnames(sparse) = make.names(colnames(sparse))
  
  #add dependent variable to the dataframe. We will be predicting this value.
  sparse$negative <- bing_name$negative
  return(sparse)
}

#----------------Processing Corpus----------------#
b_forum_predict <- process_corpus(bforum_corpus, forum_bing)
b_reddit_predict <- process_corpus(breddit_corpus, reddit_bing)
b_forandred_predict <- process_corpus(bforandred_corpus, forandred_bing)

a_forum_predict <- process_corpus(aforum_corpus, forum_afinn)
a_reddit_predict <- process_corpus(areddit_corpus, reddit_afinn)
a_forandred_predict <- process_corpus(aforandred_corpus, forandred_afinn)




#----------------Test Code----------------#
#setting a seed to get consistent results when running multiple times
set.seed(1113)

#split the data into a training set and a test set. 


split <- sample.split(b_forandred_predict$negative, SplitRatio = 0.8)
trainow <- subset(b_forandred_predict, split == TRUE)
testow <- subset(b_forandred_predict, split == FALSE)

#First model created. Default values used.
forumCART <- rpart(negative ~., data=trainow, method="class")
#plot the model
prp(forumCART)


predictForumCart <- predict(forumCART, newdata=testow, type="class")
table(testow$negative, predictForumCart)
