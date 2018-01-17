library(dplyr)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(reshape2)
library(RCurl)

#read owforums data into R. Ensure they are not factors
#reddit <- read.csv(text=getURL("https://raw.githubusercontent.com/ujumqin/SB_CapstoneProject/master/Final%20Project%20Files/OW%20Data/Raw%20Files/reddit12_22.csv"), stringsAsFactors = FALSE)
reddit <- read.csv("REDDIT_12_22_FINAL.CSV", stringsAsFactors = FALSE)
#twitter <- read.csv(text=getURL("https://raw.githubusercontent.com/ujumqin/SB_CapstoneProject/master/Final%20Project%20Files/OW%20Data/Raw%20Files/OWT_12-22.csv"), stringsAsFactors = FALSE)
twitter <- read.csv("OWTWITTER_12-22_FINAL.csv", stringsAsFactors = FALSE)
#forums <- read.csv(text=getURL("https://raw.githubusercontent.com/ujumqin/SB_CapstoneProject/master/Final%20Project%20Files/OW%20Data/Raw%20Files/OWFORUMS12_22_FINAL.csv"), stringsAsFactors = FALSE)
forums <- read.csv("OWFORUMS12_22_FINAL.CSV", stringsAsFactors = FALSE)
#all <- read.csv(text=getURL("https://raw.githubusercontent.com/ujumqin/SB_CapstoneProject/master/Final%20Project%20Files/OW%20Data/Raw%20Files/all.csv"), stringsAsFactors = FALSE)
#withouttwitter <- read.csv(text=getURL("https://raw.githubusercontent.com/ujumqin/SB_CapstoneProject/master/Final%20Project%20Files/OW%20Data/Raw%20Files/WithoutTwitter_FINAL.csv"), stringsAsFactors = FALSE)


#duplicating the text column so we have a copy of the text. this text is used to group
reddit$text_topic <- reddit$X.text.
twitter$text_topic <- twitter$text
forums$text_topic <- forums$text
all$text_topic <- all$text
withouttwitter$text_topic <- withouttwitter$text

#-------------------------Tokenize Text-------------------------
#tokenize the text column for reddit
tidy_reddit <- reddit %>%
  group_by(text_topic) %>%
  dplyr::mutate(linenumber = row_number()) %>%
  unnest_tokens(word, X.text.) %>%
  ungroup()

#tokenize the text column for twitter
tidy_twitter <- twitter %>%
  group_by(text_topic) %>%
  dplyr::mutate(linenumber = row_number()) %>%
  unnest_tokens(word, text) %>%
  ungroup()

#tokenize the text column for forums
tidy_forums <- forums %>%
  group_by(X.) %>%
  dplyr::mutate(linenumber = row_number()) %>%
  unnest_tokens(word, text) %>%
  ungroup()

tidy_all <- all %>%
  group_by(X.) %>%
  dplyr::mutate(linenumber = row_number()) %>%
  unnest_tokens(word, text) %>%
  ungroup()

tidy_notwitter <- withouttwitter %>%
  group_by(X.) %>%
  dplyr::mutate(linenumber = row_number()) %>%
  unnest_tokens(word, text) %>%
  ungroup()

#-------------------------Bing Sentiment-------------------------
reddit_bing <- tidy_reddit %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

twitter_bing <- tidy_twitter %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

forum_bing <- tidy_forums %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

all_bing <- tidy_all %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

withouttwitter_bing <- tidy_notwitter %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

#-------------------------Plotting Data (Bing)-------------------------
reddit_bing %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Reddit Most Used Sentiment Words",
       x = NULL) +
  coord_flip()

twitter_bing %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Twitter Most Used Sentiment Words",
       x = NULL) +
  coord_flip()

forum_bing %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Overwatch Forums Most Used Sentiment Words",
       x = NULL) +
  coord_flip()

all_bing %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Most Used Sentiment Words By Overwatch Community",
       x = NULL) +
  coord_flip()

withouttwitter_bing %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Most Used Sentiment Words By Overwatch Community",
       x = NULL) +
  coord_flip()
#-------------------------nrc Sentiment-------------------------
reddit_nrc <- tidy_reddit %>%
  inner_join(get_sentiments("nrc"))

twitter_nrc <- tidy_twitter %>%
  inner_join(get_sentiments("nrc"))

forum_nrc <- tidy_forums %>%
  inner_join(get_sentiments("nrc"))

all_nrc <- tidy_all %>%
  inner_join(get_sentiments("nrc"))

withouttwitter_nrc <- tidy_all %>%
  inner_join(get_sentiments("nrc"))

#-------------------------Plotting Data (nrc)-------------------------
ggplot(reddit_nrc, aes(x="", fill=sentiment)) +
  geom_bar(width = 1) +
  ggtitle("reddit") 

reddit_bar

reddit_bar + coord_polar("y")

ggplot(forum_nrc, aes(x="", fill=sentiment)) +
  geom_bar(width = 1) +
  ggtitle("Overwatch Forums") 

forum_bar

forum_bar + coord_polar("y")


ggplot(twitter_nrc, aes(x=sentiment)) +
  geom_bar(aes(y=..count.., fill =..count..)) +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1, vjust = 1)) +
  ggtitle("Twitter") +
  coord_cartesian(ylim = c(0,11000), expand = TRUE)

ggplot(forum_nrc, aes(x=sentiment)) +
  geom_bar(aes(y=..count.., fill =..count..)) +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1, vjust = 1)) +
  ggtitle("Overwatch Forums") +
  coord_cartesian(ylim = c(0,11000), expand = TRUE)

ggplot(withouttwitter_nrc, aes(x=sentiment)) +
  geom_bar(aes(y=..count.., fill =..count..)) +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1, vjust = 1)) +
  ggtitle("All Communities") +
  coord_cartesian(ylim = c(0,24000), expand = TRUE)
  
#-------------------------afinn Sentiment-------------------------
reddit_afinn <- tidy_reddit %>%
  inner_join(get_sentiments("afinn"))

twitter_afinn <- tidy_twitter %>%
  inner_join(get_sentiments("afinn"))

forum_afinn <- tidy_forums %>%
  inner_join(get_sentiments("afinn"))

all_afinn <- tidy_all %>%
  inner_join(get_sentiments("afinn"))

withouttwitter_afinn <- tidy_notwitter %>%
  inner_join(get_sentiments("afinn"))

#-------------------------Plotting data (Afinn)-------------------------

ggplot(twitter_afinn, aes(x=score)) +
  geom_bar(aes(y=..count.., fill= ..count..)) + 
  labs(x="afinn score") +
  xlab("afinn score")+
  coord_cartesian(ylim = c(0,7000), expand = TRUE) +
  ggtitle("twitter") +
  scale_x_continuous(breaks = -5:5) 

ggplot(forum_afinn, aes(x=score)) +
  geom_bar(aes(y=..count.., fill=..count..)) + 
  labs(x="afinn score") +
  xlab("afinn score")+
  coord_cartesian(ylim = c(0,7000), expand = TRUE)+
  ggtitle("Overwatch Forums") +
  scale_x_continuous(breaks = -5:5) 

ggplot(reddit_afinn, aes(x=score)) +
  geom_bar(aes(y=..count.., fill=..count..)) + 
  labs(x="afinn score") +
  xlab("afinn score")  +
  coord_cartesian(ylim = c(0,7000), expand = TRUE)+
  ggtitle("Reddit") +
  scale_x_continuous(breaks = -5:5) 

ggplot(withouttwitter_afinn, aes(x=score)) +
  geom_bar(aes(y=..count.., fill=..count..)) + 
  labs(x="afinn score") +
  xlab("afinn score")  +
  coord_cartesian(ylim = c(0,13000), expand = TRUE)+
  ggtitle("All") +
  scale_x_continuous(breaks = -5:5) 

#-------------------------Word Clouds-------------------------

tidy_forums %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 50))

tidy_reddit %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 50))

tidy_twitter %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 50))

tidy_all %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 50))

tidy_notwitter %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 50))

#-------------------------Word Clouds (negative vs positive)-------------------------
par(mfrow=c(2,2))

tidy_forums %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 80)

 tidy_reddit %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 80)

tidy_twitter %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 80)

tidy_notwitter %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 80)
