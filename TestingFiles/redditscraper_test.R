library(rvest)
library(stringr)
library(tidyr)
library(RCurl)
library(dplyr)
library(curl)

#Read URL
ow.forums <- read_html("https://www.reddit.com/r/Overwatch/")

#scrapes links from read URL
links <- ow.forums %>% html_nodes("a") %>% html_attr("href")

#Makes links scrape a data frame
links <- as.data.frame(links, row.names = NULL)

#Find the links for the forums (since links are /forums) and keep these. 
keep<- grepl("^https://www.reddit.com/r/Overwatch/comments/", links$links)
print(keep)
keep <- as.data.frame(keep, row.names = NULL)

#bind the answers to the new dataframe
links <- cbind(links, keep)

#remove the rows of unneeded links
#links now contains all of the links for the forum post to be scrapped
#paste to strip out unnecessary rows
links <- links[links$keep == "TRUE",]
links <- paste(links$links)

#write comment links to file
write.csv(links, file="redditcommentslink.csv")


#new df ow.total
owreddit.total <- data.frame("link", "title", "time", "text", stringsAsFactors = FALSE)


###################test scrape
print(links)
read_single_links <- read_html(links[1])
#print(read_single_links)

############!!!!!!!!!!!!!!KEEEEPPP!!!!!!!!!!!!############
#this scrapes comments


comments <- read_single_links %>% html_nodes(".md")
#convert from xml to usable format
comments <- trimws(comments)
comments <- gsub("<.*?>", "", comments)
#script going too fast and wasn't cleaning
Sys.sleep(1)
comments <- gsub("\n", "", comments)
print(comments)

############!!!!!!!!!!!!!!KEEEEPPP!!!!!!!!!!!!############


#scrapes comment timestamp by date
comment_time <- read_single_links %>% html_node(".tagline, time")
comment_time <- trimws(comment_time)
comment_time <- gsub('<.*?>', "", comment_time)

print(comment_time)

#scrapes username
reddit_user <- read_single_links %>% html_node(".tagline, .author")
print(reddit_user)
reddit_user <- trimws(reddit_user)

reddit_user <- gsub('<.*?>', "", reddit_user)
print(reddit_user)

write.csv(clean_time, file = "reddittime.csv")


# 
# 
# title <- xml_contents(title_results) %>% html_text(trim = TRUE)
# #line for testing to see new threads easier
# print("---------------------------------------------------------------------------------------------------------")
# print(links[n])
# print(title)
# 
# #pulls content of thread
# topic_results <- read_single_links %>% html_nodes(".TopicPost-bodyContent")
# topic <- trimws(topic_results)
# 
# #cleanup of bodycontent text. 
# clean_topic <- gsub("<.*?>", "", topic)
# #script going too fast and wasn't cleaning
# Sys.sleep(1)
# clean_topic <- gsub("\n", "", clean_topic)
# #print(clean_topic)
# 
# #pulls date
# topic_time <- read_single_links %>% html_node(".TopicPost-timestamp")
# clean_time <- gsub('<.*content=\\"', "", topic_time)
# clean_time <- gsub('\">.*', "", clean_time)
# clean_time <- as.POSIXct(clean_time, format = "%m/%d/%Y %I:%M %p")
# print(clean_time)
# 
# 
# ###########################
# 
# #For loop to access URL from full_links  
# #loop for grabbing titles and topic
# n <- 1
# while(n <= length(links)) {
#   read_single_links <- read_html(links[n])
#   
#   title_results <- read_single_links %>% html_nodes(".Topic-title")
#   title <- xml_contents(title_results) %>% html_text(trim = TRUE)
#   #line for testing to see new threads easier
#   print("---------------------------------------------------------------------------------------------------------")
#   print(links[n])
#   print(title)
#   
#   #pulls content of thread
#   topic_results <- read_single_links %>% html_nodes(".TopicPost-bodyContent")
#   topic <- trimws(topic_results)
#   
#   #cleanup of bodycontent text. 
#   clean_topic <- gsub("<.*?>", "", topic)
#   #script going too fast and wasn't cleaning
#   Sys.sleep(1)
#   clean_topic <- gsub("\n", "", clean_topic)
#   #print(clean_topic)
#   
#   #pulls date
#   topic_time <- read_single_links %>% html_node(".TopicPost-timestamp")
#   clean_time <- gsub('<.*content=\\"', "", topic_time)
#   clean_time <- gsub('\">.*', "", clean_time)
#   clean_time <- as.POSIXct(clean_time, format = "%m/%d/%Y %I:%M %p")
#   print(clean_time)
#   
#   
#   #adds topic to a vector with link, title using a for loop
#   for(i in 1:length(clean_topic)) {
#     ow.total <- rbind(ow.total, c(clean_links[n], title, clean_time, clean_topic[i]))
#   }
#   
#   print(n)
#   n<-n+1
#   #Sys.sleep(2)
# }
# 
# write.csv(ow.total, file = "hopeitworks11_21.csv")
# 
# 
# #----------------------
# 
# 
# str(ow.total)
# 
# x[!duplicated(x[,1]),]
# newow.total <- ow.total[!duplicated(ow.total[4]),]
# 
# str(newow.total)
# write.csv(newow.total, file = "nodupes11_21.csv")           
# write.csv(ow.total, file = "withdupes11_21.csv")
