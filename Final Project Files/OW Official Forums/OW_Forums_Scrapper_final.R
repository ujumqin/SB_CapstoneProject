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
  clean_topic <- gsub("\n", "", clean_topic)
  

  #pulls date
  topic_time <- read_single_links %>% html_node(".TopicPost-timestamp")
  clean_time <- gsub('<.*content=\\"', "", topic_time)
  clean_time <- gsub('\">.*', "", clean_time)
  clean_time <- as.POSIXct(clean_time, format = "%m/%d/%Y %I:%M %p")
  clean_time <- paste(clean_time)
  print(clean_time)
  
  #adds topic to a vector with link, title using a for loop
  for(i in 1:length(clean_topic)) {
    ow.total <- rbind(ow.total, c(clean_links[n], title, clean_time[i], clean_topic[i]))
  }
  
  print(n)
  n<-n+1
  
}


####-----Cleaning up the Text column since it has dates merged into some of them
newdate <- c()
newtext <- c()

for(i in 1:length(ow.total$X.text.)) {
  if(grepl('\\d\\d/\\d\\d/.*?M', ow.total$X.text.[i]) == FALSE) {
    print("CLEAN")
  } else {
    print("UNCLEAN! CLEANING")
  m <- regexpr('\\d\\d/\\d\\d/.*?M',ow.total$X.text.[i])
  owfdates <- regmatches(ow.total$X.text.[i], m, invert = FALSE)
  owcleantext <- gsub('\\d\\d/\\d\\d/.*?M', '', ow.total$X.text.[i])
  newdate[i] <- owfdates
  newtext[i] <- owcleantext
  }
}


for(i in 1:length(ow.total$X.text.)) {
  if(is.na(newtext[i])) {
    print("Do not write over old value")
  } else if(newtext[i] != 'NA') {
    #print("Copy over new data")
    ow.total$X.text.[i] <- newtext[i]
    ow.total$X.time.[i] <- newdate[i]
  }
}

#remove duplicate values
str(ow.total)
newow.total_clean <- ow.total[!duplicated(ow.total[4]),]

#write the file
write.csv(newow.total_clean, 'OWFORUMS_manualcheck_12_30.csv')


