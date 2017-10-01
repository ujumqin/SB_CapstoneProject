library(rvest)
library(stringr)
library(tidyr)
library(RCurl)
library(dplyr)


#Read URL
ow.forums <- read_html("https://us.battle.net/forums/en/overwatch/22813879/")

#scrapes links from read URL
links <- ow.forums %>% html_nodes("a") %>% html_attr("href")

#Makes links scrape a data frame
links <- as.data.frame(links, row.names = NULL)

#Find the links for the forums (since links are /forums) and keep these. 
keep<- grepl("^/forums", links$links)
keep <- as.data.frame(keep, row.names = NULL)
keep

#bind the answers to the new dataframe
links <- cbind(links, keep)
links

#remove the rows of unneeded links
#links now contains all of the links for the forum post to be scrapped
links <- links[dflinks3$keep == "TRUE",]

#links only contains the tail end of the links. creating front_url to be concatenated later
front_url <- "https://us.battle.net"


#concatenate to create a full link
full_links <- paste(front_url,links$links, sep = "")
full_links
write.csv(full_links, file="full_links.csv")

#new vectors, ow.title, ow.topic
ow.title <- "first"
ow.topic <- "first"  

#For loop to access URL from full_links  
#loop for grabbing titles and topic
n <- 1
while(n <= length(full_links)) {
  single_links <- full_links[n]
  read_single_links <- read_html(single_links)
  
  title_results <- read_single_links %>% html_nodes(".ForumTopic-title")
  title <- xml_contents(title_results) %>% html_text(trim = TRUE)
  ow.title <- append(ow.title, title)
  
  titletopic_results <- read_single_links %>% html_nodes(".ForumTopic--preview")
  topic <- xml_contents(topic_results) %>% html_text(trim = TRUE)
  ow.topic <- append(ow.topic, topic)
  
  print(n)
  n<-n+1
}


#need to remove duplicate values in ow.topic
print(ow.title)
print(ow.topic)

write.csv(ow.title, file = "ow.title9_30_1.csv")
write.csv(ow.topic, file = "ow.topic9_30_1.csv")
