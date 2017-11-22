library(rvest)
library(stringr)
library(tidyr)
library(RCurl)
library(dplyr)
library(curl)

#Read URL
ow.forums <- read_html("https://us.battle.net/forums/en/overwatch/22813879/")

#scrapes links from read URL
links <- ow.forums %>% html_nodes("a") %>% html_attr("href")

#Makes links scrape a data frame
links <- as.data.frame(links, row.names = NULL)

#Find the links for the forums (since links are /forums) and keep these. 
keep<- grepl("^/forums", links$links)
keep <- as.data.frame(keep, row.names = NULL)

#bind the answers to the new dataframe
links <- cbind(links, keep)

#remove the rows of unneeded links
#links now contains all of the links for the forum post to be scrapped
links <- links[links$keep == "TRUE",]

#links only contains the tail end of the links. creating front_url to be concatenated later
front_url <- "https://us.battle.net"


#concatenate to create a full link and remove duplicates
full_links <- paste(front_url,links$links, sep = "")
#remove duplicate full_links
clean_links <- unique(full_links)

#write links to file 
write.csv(clean_links, file="clean_links.csv")

#new df ow.total
ow.total <- data.frame("link", "title", "time", "text", stringsAsFactors = FALSE)

#For loop to access URL from full_links  
#loop for grabbing titles and topic
n <- 1
while(n <= length(clean_links)) {
  read_single_links <- read_html(clean_links[n])
  
  title_results <- read_single_links %>% html_nodes(".Topic-title")
  title <- xml_contents(title_results) %>% html_text(trim = TRUE)
  #line for testing to see new threads easier
  print("---------------------------------------------------------------------------------------------------------")
  print(clean_links[n])
  print(title)
  
  #pulls content of thread
  topic_results <- read_single_links %>% html_nodes(".TopicPost-bodyContent")
  topic <- trimws(topic_results)
  
  #cleanup of bodycontent text. 
  clean_topic <- gsub("<.*?>", "", topic)
  #script going too fast and wasn't cleaning
  Sys.sleep(1)
  clean_topic <- gsub("\n", "", clean_topic)
  #print(clean_topic)

  #pulls date
  topic_time <- read_single_links %>% html_node(".TopicPost-timestamp")
  clean_time <- gsub('<.*content=\\"', "", topic_time)
  clean_time <- gsub('\">.*', "", clean_time)
  clean_time <- as.POSIXct(clean_time, format = "%m/%d/%Y %I:%M %p")
  print(clean_time)
  
  
  #adds topic to a vector with link, title using a for loop
  for(i in 1:length(clean_topic)) {
    ow.total <- rbind(ow.total, c(clean_links[n], title, clean_time, clean_topic[i]))
  }
  
  print(n)
  n<-n+1
  #Sys.sleep(2)
}

write.csv(ow.total, file = "hopeitworks.csv")


#----------------------


read_single_links <- read_html(clean_links[10])
                               
topic_time <- read_single_links %>% html_node(".TopicPost-timestamp")
print(topic_time)


clean_time <- gsub('<.*content=\\"', "", topic_time)
clean_time <- gsub('\">.*', "", clean_time)

clean_time <- as.POSIXct(clean_time, format = "%m/%d/%Y %I:%M %p")
print(clean_time)
typeof(clean_time)
clean_time <- as.numeric(clean_time)
print(clean_time)
NCOL(clean_time)

time <- xml_contents(topic_time) %>% trimws
print(time)
time <- xml_contents(topic_time) %>% html_text(trim = TRUE)

