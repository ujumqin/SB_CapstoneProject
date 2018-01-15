

#==========JUNK SCRIPTS Create a corpus from text==============
str(owforums)



owcorpus = Corpus(VectorSource(owforums$text))
inspect(owcorpus[[100]])

owcorpus <- tm_map(owcorpus, removePunctuation)
owcorpus <- tm_map(owcorpus, tolower)

#I need to remove hero names and OW venacular like 'kill'
owcorpus <- tm_map(owcorpus, removeWords, c("kill", "mercy", "bastion", stopwords("english")))


#do this last?
owcorpus <- tm_map(owcorpus, stemDocument)

#===================Tokenize text and group by title===============

tidy_owforums <- owforums %>%
  group_by(title) %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word, text) %>%
  ungroup()

str(tidy_owforums)
write.csv(tidy_owforums, file="tidyowforums.csv")

#=====================bing solution===============================
#==============THIS CODE MAY BE VIABLE FOR DETERMINING THE SCORE FOR A THREAD=================
owforums_sentimentbing <- tidy_owforums %>%
  inner_join(get_sentiments("bing"))

str(owforums_sentimentbing)

owforums_sentimentbing$count <- 1
owforums_sentimentbing$score <- 0

for(i in 1:length(owforums_sentimentbing$sentiment)) {
  if(owforums_sentimentbing$sentiment[i] == "negative") {
    print("negative")
    owforums_sentimentbing$score[i] = -1
  } else {
    print("positive")
    owforums_sentimentbing$score[i] = 1
  }
}

str(owforums_sentimentbing)

owsent <- data.frame(owforums_sentimentbing$title, owforums_sentimentbing$sentimentscore)
owsent

str(owsent)
write.csv(owsent, file = "owsent.csv")

#his shows us what sentiment is for each topic
#THIS CODE SHOULD GROUP BY TITLE AND BY LINE NUMBER AND CREATE THE SUM!
owforums_sentimentbing <- owforums_sentiment %>%
  group_by(title) %>%
  mutate(sentimentscore = sum(score)/sum(count))

write.csv(owforums_sentimentbing, file = "bingcounttest.csv")

#=====================AFIN Solution===============================
owforums_sentiment <- tidy_owforums %>%
  inner_join(get_sentiments("afinn"))

str(owforums_sentiment)
write.csv(owforums_sentiment, file = "senttest.csv")


owforums_sentiment$count <- 1
owforums_sentiment
#his shows us what sentiment is for each topic
#THIS CODE SHOULD GROUP BY TITLE AND BY LINE NUMBER AND CREATE THE SUM!
owforums_sentiment <- owforums_sentiment %>%
  group_by(title) %>%
  mutate(wordtotal = sum(count))


testanaly <- owforums_sentiment %>%
  group_by(title) %>%
  mutate(total = sum(score)/wordtotal)

testanaly
write.csv(testanaly, file = "counttest.csv")
#=====================AFIN Solution===============================



#========================================

#steps for analytics
#load data x
#tokenize it x
#sentiment for each title average score x
#
#clean up (stop words, punctuations, case, and stem words)



#==============================================Data CAMP solution
#this code tokenizes the text
tidy_owforums <- owforums %>%
  group_by(title) %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word, text) %>%
  ungroup()

#this code counts the words(tokens) and tallies them. tells us how much of each word 
tidy_owforums %>%
  count(word, sort = TRUE)
#need to remove filler words

#this does an inner join with the lexicon nrc. basically find words that these two tables have in common and code the ones that are the stame
owforums_sentiment <- tidy_owforums %>%
  inner_join(get_sentiments("afinn"))



#this gives us the sentiment and the number of words that show up in each per topic
owforums_sentiment %>%
  count(title, score)

write.csv(owforums_sentiment, file = "owsentiment1.csv")

#This uses the bing sentiment. Later to be talied to see which of each word is used more

owsent_counts <- tidy_owforums %>%
  inner_join(get_sentiments("bing")) %>%
  count(title, sentiment)


str(owsent_counts)

#his shows us what sentiment is for each topic
owsent_counts %>%
  group_by(title) %>%
  mutate(total = sum(n), 
         percent = n/total) %>%
  filter(sentiment == "negative") %>%
  arrange(percent)


#This uses the afinn sentiment. Later to be talied to see which of each word is used more

owsent_counts_test <- tidy_owforums %>%
  inner_join(get_sentiments("afinn")) %>%
  count(title, score)

str(owsent_counts_test)
owsent_counts_test
write.csv(owsent_counts_test, file="testsent.csv")
#his shows us what sentiment is for each topic
owsent_counts_test %>%
  group_by(title) %>%
  mutate(total = sum(n), 
         percent = total/n) %>%
  arrange(percent)

#This will tally up the words for us. Which is most used?
ow_wordcounts <- tidy_owforums %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment)

#organize into top 10
ow_topwords <- ow_wordcounts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word,n))



#plots the words
ggplot(ow_topwords, aes(x=word, y=n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +
  coord_flip()

#bing lexicon
#bing has two sentiments: positive and negative
get_sentiments("bing")

#nrc lexicon
#nrc has 10 sentiments 
#(anger, anticipation, disgust, fear, joy, 
#negative, npositive, sadness, surprise, trust)
get_sentiments("nrc")

#affin lexicon
#gets sentiment based on score. not just postive or negative. 
#I should use this one!
get_sentiments("afinn")