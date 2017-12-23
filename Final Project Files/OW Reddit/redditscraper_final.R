library(rvest)
#library(stringr)
library(tidyr)
library(RCurl)
library(dplyr)
library(curl)
library(rebus)

#Read URL
ow.forums <- read_html("https://www.reddit.com/r/Overwatch/")

#scrapes links from read URL
links <- ow.forums %>% html_nodes("a") %>% html_attr("href")

#Makes scrapped links a data frame
links <- as.data.frame(links, row.names = NULL)

#Find the links for the forums (since links are /forums) and keep these. 
keep<- grepl("^https://www.reddit.com/r/Overwatch/comments/", links$links)
#print(keep)
keep <- as.data.frame(keep, row.names = NULL)

#bind the answers to the new dataframe
links <- cbind(links, keep)

#remove the rows of unneeded links
#links now contains all of the links for the forum post to be scrapped
#paste to strip out unnecessary rows
links <- links[links$keep == "TRUE",]
links <- paste(links$links)

print(links)


#write comment links to file
write.csv(links, file="redditcommentslink.csv")


#new df ow.total
owreddit.total <- data.frame("link", "title", "time", "text", "user", stringsAsFactors = FALSE)
print(owreddit.total)

n <- 1
while(n <= length(links)) {
  read_single_links <- read_html(links[n])
  
  #scrapes the link's title
  link_title <- gsub("https://www.reddit.com/r/Overwatch/comments/", "", links)
  link_title <- substr(link_title,8, nchar(link_title)-1)
  link_title <- gsub("_", " ", link_title)
  
  #scrapes comments
  comments <- read_single_links %>% html_nodes(".md")
  
  #convert from xml to usable format
  comments <- trimws(comments)
  comments <- gsub("<.*?>", "", comments)
  
  #script going too fast and wasn't cleaning
  Sys.sleep(1)
  comments <- gsub("\n", "", comments)
  
  
  #scrapes the .tagline node which has username data
  reddit_user <- read_single_links %>% html_nodes(".tagline")
  
  #Scrapes to the user name
  reddit_user <- gsub('.*user/', "", reddit_user)
  
  #Scrapes the stuff after class out
  reddit_user <- gsub('class.*', "", reddit_user)
  
  #clean up
  reddit_user <- gsub(" ","",reddit_user, fixed=TRUE)
  reddit_user <- gsub("\"","",reddit_user, fixed=TRUE)
  reddit_user <- gsub("<p","",reddit_user, fixed=TRUE)
  
  
  #scrapes comment timestamp by date
  comment_time <- read_single_links %>% html_nodes(".tagline, time")
  comment_time <- gsub('.*datetime=', "", comment_time)
  comment_time <- gsub('T.*', "", comment_time)
  comment_time <- gsub('\"', "", comment_time)
  comment_time <- gsub('<p.*', "", comment_time)
  
  
  #prints everything scrapped
  print("---------------------------------------------------------------------------------------------------------")
  print("====link====")
  print(links[n])
  print(" ")
  print("====title====")
  print(link_title[n])
  print(" ")
  print("====comment====")
  print(comments[n])
  print(" ")
  print("====user====")
  print(reddit_user[n])
  print(" ")
  print("====date====")
  print(comment_time[n])
  print(" ")

  

  #adds topic to a vector with link, title using a for loop
  for(i in 1:length(comments)) {
    owreddit.total <- rbind(owreddit.total, c(links[n], link_title[n], comment_time[i], comments[i+2], reddit_user[i+1]))
  }
  
  print(n)
  n<-n+1
  #Sys.sleep(2)
}

write.csv(owreddit.total, file = "reddit12_22.csv")

